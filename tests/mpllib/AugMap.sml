datatype scheme = WB of real

signature Aug =
sig
  type key
  type value (* can make this polymorhpic *)
  type aug
  val compare : key * key -> order
  val g : key * value -> aug
  val f : aug * aug -> aug
  val id : aug
  val balance : scheme
  val debug : key * value * aug -> string
end

signature AugMap =
sig
  exception Assert
  structure T : Aug
  datatype am = Leaf | Node of {l : am, k : T.key, v : T.value, a : T.aug, r :  am, size : int}
  val empty : unit -> am
  val size : am -> int
  val find : am -> T.key -> T.value option
  val union : am -> am -> (T.value * T.value -> T.value) -> am
  val insert : am -> T.key -> T.value -> (T.value * T.value -> T.value) -> am
  val multi_insert : am -> ((T.key * T.value) Seq.t) -> (T.value * T.value -> T.value) -> am
  val mapReduce : am -> (T.key * T.value -> 'b) -> ('b * 'b -> 'b)-> 'b -> 'b
  val join : am -> T.key -> T.value -> am -> am
  val filter : (T.key * T.value -> bool) -> am -> am
  val build : ((T.key * T.value) Seq.t) -> int -> int -> am
  val aug_left : am -> T.key -> T.aug
  val aug_filter : am -> (T.aug -> bool) -> am
  val aug_range : am -> T.key -> T.key -> T.aug
  val aug_project : (T.aug -> 'a) -> ('a * 'a -> 'a) -> am -> T.key -> T.key -> 'a
  val up_to : am -> T.key -> am
  val print_tree : am -> string -> unit
  val singleton : T.key -> T.value -> am
end

functor PAM (T: Aug) : AugMap =
struct
  type key = T.key
  type value = T.value
  type aug = T.aug

  (* how to add the metric for balancing? *)
  datatype am = Leaf | Node of {l : am, k : key, v : value, a : aug, r :  am, size : int}
  exception Assert
  structure T = T
  (* | FatLeaf (array ) in order traversal  gran + leaves*)
  (* joinG that takes the grain and join which does something itself *)
  (* maybe not store augmented values for thin leaves*)
  (* fat leaves trick -- use leaves of arrays *)
  val gran = 100

  fun weight m =
    case m of
      Leaf => 0
    | Node n => #size n

  fun size m =
    case m of
      Leaf => 0
    | Node n => #size n

  fun aug_val m =
    case m of
      Leaf => T.id
    | Node {a, ...} => a

  structure WBST =
  struct

    fun leaf_weight () = 0

    fun singleton_weight () = 1

    val ratio =
      case T.balance of
        WB (x) => x / (1.0 - x)

    fun size_heavy s1 s2 =
      ratio * (Real.fromInt s1) > (Real.fromInt s2)

    fun heavy (m1 : am, m2 : am) = size_heavy (weight m1) (weight m2)

    fun like s1 s2 = not (size_heavy s1 s2) andalso not (size_heavy s2 s1)

    fun compose m1 k v m2 =
      let
        val new_size = (size m1) + (size m2) + 1
        val a = T.f ((aug_val m1), T.f (T.g(k, v), (aug_val m2)))
      in
        Node {l = m1, k = k, v = v, a = a, r = m2, size = new_size}
      end

    fun rotateLeft m =
      case m of
        Leaf => m
      | Node {l, k, v, a, r, size} =>
          case r of
            Leaf => m
          | Node {l = rl, k = rk, v = rv, r = rr, ...} =>
              let
                val left = compose l k v rl
              in
                compose left rk rv rr
              end

    fun rotateRight m =
      case m of
        Leaf => m
      | Node {l, k, v, a, r, size} =>
          case l of
            Leaf => m
          | Node {l = ll, k = lk, v = lv, r = lr, ...} =>
              let
                val right = compose lr k v r
              in
                compose ll lk lv right
              end

    fun joinLeft (m1 : am) k v (m2 : am) =
      let
        val w1 = weight m1
        val w2 = weight m2
      in
        if (like w1 w2) then compose m1 k v m2
        else
          case m2 of
            Leaf => compose m1 k v m2
          | Node {l, k = kr, v = vr, r, size, ...} =>
              let
                val t' = joinLeft m1 k v l
                val (wlt', wrt') = case t' of
                                    Leaf => raise Assert
                                  | Node {l, r, ...} => ((weight l), (weight r))
                val wr = weight r
              in
                if like (weight t') wr then compose t' kr vr r
                else if like wrt' wr andalso like wlt' (wrt' + wr) then
                  rotateRight (compose t' kr vr r)
                else rotateRight (compose (rotateLeft t') kr vr r)
              end
      end

    fun joinRight m1 k v m2 =
      let
        val w1 = weight m1
        val w2 = weight m2
      in
        if like w1 w2 then compose m1 k v m2
        else
          case m1 of
            Leaf => compose m1 k v m2
          | Node {l, k = kl, v = vl, r, size, ...} =>
              let
                val t' = joinRight r k v m2
                val (wlt', wrt') = case t' of
                                    Leaf => raise Assert
                                  | Node n => ((weight (#l n)), (weight (#r n)))
                val wl = weight l
              in
                if like wl (weight t') then compose l kl vl t'
                else if like wl wlt' andalso like (wl + wlt') wrt' then
                  rotateLeft (compose l kl vl t')
                else rotateLeft (compose l kl vl (rotateRight t'))
              end
      end

    fun join m1 k v m2 =
      if heavy(m1, m2) then joinRight m1 k v m2
      else if heavy(m2, m1) then joinLeft m1 k v m2
      else compose m1 k v m2

  end

  fun par (f1, f2) = ForkJoin.par (f1, f2)

  fun eval (inpar, f1, f2) =
    if inpar then ForkJoin.par (f1, f2)
    else (f1(), f2())

  fun join m1 k v m2 =
    case T.balance of
      WB _ => WBST.join m1 k v m2

  fun join2 m1 m2 =
    let
      fun splitLast {l, k, v, a, r, size, ...} =
        case r of
          Leaf => (l, k, v)
        | Node n =>
            let
              val (m', k', v') = splitLast n
            in
              (join l k v m', k', v')
            end
    in
      case m1 of
        Leaf => m2
      | Node n =>
          let
            val (m', k', v') = splitLast n (*get the greatest element in m1*)
          in
            join m' k' v' m2
          end
    end

  fun empty () = Leaf

  fun singleton k v =
    Node {l = Leaf, k = k, v = v, a = T.g (k, v), r = Leaf, size = 1}

  fun build_sorted s i j =
    if i = j then empty()
    else if i + 1 = j then singleton (#1 (Seq.nth s i)) (#2 (Seq.nth s i))
    else
      let
        val m = i + Int.div ((j - i), 2)
        val (l, r) = if (j - i) > 1000 then eval (true, fn _ => build_sorted s i m, fn _ => build_sorted s (m + 1) j)
                    else (build_sorted s i m, build_sorted s (m + 1) j)
        val (x, y) = Seq.nth s m
      in
        join l x y r
      end

  fun find m k =
    case m of
      Leaf => NONE
    | Node ({l, k = kr, v, r, ...}) =>
        case T.compare(k, kr) of
          LESS => find l k
        | EQUAL => SOME v
        | GREATER => find r k

  fun insert m k v h =
    case m of
      Leaf => singleton k v
    | Node {l, k = kr, v = vr, r, ...} =>
        case T.compare(k, kr) of
          EQUAL => join l k (h (v, vr)) r
        | LESS => join (insert l k v h) kr vr r
        | GREATER => join l kr vr (insert r k v h)

  fun key_equal k1 k2 = T.compare (k1, k2) = EQUAL

  fun multi_insert m s h =
    let
      val ss = Mergesort.sort (fn (i, j) => T.compare(#1 i, #1 j)) s
      fun insert_helper m' i j =
        if i >= j then
          m'
        else if (j - i) < gran then
          (* this v/s the else branch with recursive calls done sequentially *)
          SeqBasis.foldl (fn (m'', (k, v)) => insert m'' k v h) m' (i, j) (Seq.nth ss)
        else
         case m' of
            Leaf => build_sorted s i j
          | Node {l, k = kr, v = vr, r, ...} =>
              let
                fun bin_search k i j =
                  (* inv i < j, all k inside [i, j) *)
                  (* returns [lk, rk) every element in the range has key = k *)
                  if (i + 1 = j) then
                    if key_equal k (#1 (Seq.nth ss i)) then (i, j)
                    else (i + 1, j)
                  else
                    let
                      val mid = i + Int.div ((j - i), 2) (* i < mid < j*)
                      val mid_val = Seq.nth ss mid
                      fun until_boundary b1 b2 e i f =
                        if i < b1 then b1
                        else if i >= b2 then b2 - 1
                        else if (e (Seq.nth ss i)) then
                          until_boundary b1 b2 e (f i) f
                        else i
                    in
                      case T.compare (k, #1 mid_val) of
                        LESS => bin_search k i mid
                      | EQUAL =>
                        let
                          val bound_func = until_boundary i j (fn t => key_equal k (#1 t)) mid
                        in
                          (bound_func (fn i => i - 1), 1 + bound_func (fn i => i + 1))
                        end
                      | GREATER => bin_search k mid j
                    end
                val (lk, rk) = bin_search kr i j
                val (l, r) = par (fn _ => insert_helper l i lk, fn _ => insert_helper r rk j)
                val nvr = SeqBasis.foldl (fn (v', (k, v)) => h (v, v')) vr (lk, rk) (Seq.nth ss)
              in
                join l kr nvr r
              end
    in
      insert_helper m 0 (Seq.length ss)
    end

  fun split m k =
    case m of
      Leaf => (Leaf, NONE, Leaf)
    | Node {l, k = kr, v = vr, r, ...} =>
        case T.compare(k, kr) of
          EQUAL => (l, SOME vr, r)
        | LESS =>
            let
              val (ll, so, lr) = split l k
            in
              (ll, so, join lr kr vr r)
            end
        | GREATER =>
            let
              val (rl, so, rr) = split r k
            in
              (join l kr vr rl, so, rr)
            end

  (* this is not efficient because each join is more expensive *)
  (* fun split_tail_rec m k =
  let
    fun split_helper m accl accr =
      case m of
        Leaf => (accl, NONE, accr)
      | Node {l, k = kr, v = vr, r, ...} =>
        case T.compare(k, kr) of
          EQUAL => (join2 l accl, SOME vr, join2 r accr)
        | LESS => split_helper l accl (join2 (singleton kr vr) (join2 r accr))
        | GREATER => split_helper r (join2 (singleton kr vr) (join2 accl l)) accr
  in
    split_helper m (empty()) (empty())
  end *)

  fun union m1 m2 h =
    case (m1, m2) of
      (Leaf, _) => m2
    | (_, Leaf) => m1
    | (Node {l = l1, k = k1, v = v1, r = r1, size, ...}, m2) =>
        let
          val (l2, so, r2) = split m2 k1
          val new_val = case so of
                          NONE => v1
                        | SOME v' => h(v1, v')

          val (ul, ur) = eval(size > gran, fn _ => union l1 l2 h, fn _ => union r1 r2 h)
        in
          join ul k1 new_val ur
        end

  fun filter h m =
    case m of
      Leaf => m
    | Node {l, k, v, r, ...} =>
        let
          val (l', r') = par(fn _ => filter h l, fn _ => filter h r)
        in
          if h(k, v) then join l' k v r'
          else join2 l' r'
        end

  fun mapReduce m g f id =
    case m of
      Leaf => id
    | Node {l, k, v, r, size, ...} =>
        let
          val (l', r') = eval(size > gran, fn _ => mapReduce l g f id, fn _ => mapReduce r g f id)
        in
          f(f(l', g(k, v)), r')
        end

  fun build ss i j =
    let

      val t0 = Time.now ()
      (* val _ = Mergesort.sortInPlace (fn (i, j) => T.compare(#1 i, #1 j)) ss *)

      val idx = Seq.tabulate (fn i => i) (Seq.length ss)
      fun cmp (x, y) = T.compare (#1 (Seq.nth ss x), #1 (Seq.nth ss y))

      val idx' = idx
      (* val idx' = Dedup.dedup (fn (i, j) => cmp (i, j) = EQUAL) (fn i => Util.hash64 (Word64.fromInt i)) (fn i =>Util.hash64 (Word64.fromInt (i + 1))) idx *)

      val _ = Mergesort.sortInPlace cmp idx'

      val ss' = Seq.map (Seq.nth ss) idx'

      val t1 = Time.now()
      val _ = print ("sorting time = " ^ Time.fmt 4 (Time.-(t1, t0)) ^ "s\n")
      val t2 = Time.now()
      val r = build_sorted ss' i (Seq.length ss')
      val t3 = Time.now()
      val _ = print ("from sorted time = " ^ Time.fmt 4 (Time.-(t3, t2)) ^ "s\n")
    in
      r
    end

  fun up_to m k =
    case m of
      Leaf => m
    | Node {l, k = k', r, v, ...} =>
      case T.compare (k, k') of
        LESS => up_to l k
      | EQUAL => join2 l (singleton k' v)
      | GREATER => join l k' v (up_to r k)

  fun aug_filter m h =
    case m of
      Leaf => m
    | Node {l, k, v, r, a, size, ...} =>
        let
          val (l', r') = eval (size > gran, fn _ => aug_filter l h, fn _ => aug_filter r h)
        in
          if h (T.g (k, v)) then join l' k v r'
          else join2 l' r'
        end

    (* case m of
      Leaf => T.id
    | Node {l, k, v, r, size, a, ...} =>
        case (T.compare (k, k1), T.compare (k, k2)) of
          (LESS, _) => aug_range r k1 k2
        | (EQUAL, _) => T.f ((aug_range r k1 k2), T.g (k, v))
        | (_, EQUAL) => T.f ((aug_range l k1 k2), T.g (k, v))
        | (_, GREATER) => aug_range l k1 k2
        | (GREATER, LESS) =>
            let
              val (lval, rval) = eval (size > gran, fn _ => aug_range l k1 k2, fn _ => aug_range r k1 k2)
            in
              T.f (lval, T.f (T.g (k, v), rval))
            end *)

  fun aug_project (ga : T.aug -> 'a) fa m (k1: T.key) (k2 : T.key) =
    let
      val default_val : 'a = ga T.id
      fun until_root_in_range m k1 k2 =
        case m of
          Leaf => m
        |  Node {l, k, r, ...} =>
            case (T.compare (k, k1), T.compare (k, k2)) of
              (LESS, _) => until_root_in_range r k1 k2
            | (_, GREATER) => until_root_in_range l k1 k2
            | _ => m

      fun compose_map_kv m k v = fa (ga (aug_val m), ga (T.g(k, v)))

      fun compose_kv_map k v m = fa (ga (T.g(k, v)), ga (aug_val m))

      fun aug_proj_left m k acc =
        case m of
          Leaf => acc
        | Node {l, k = k', v, r, ...} =>
            case T.compare (k, k') of
              LESS => aug_proj_left l k acc
            | EQUAL => fa (acc, compose_map_kv l k v)
            | GREATER => aug_proj_left r k (fa (acc, compose_map_kv l k v))

      fun aug_proj_right m k acc =
        case m of
          Leaf => acc
        | Node {l, k = k', v, r, ...} =>
            case T.compare (k, k') of
              LESS => aug_proj_right l k (fa (acc, compose_kv_map k v r))
            | EQUAL => fa (acc, compose_kv_map k v r)
            | GREATER =>  aug_proj_right r k acc

      fun aug_proj m =
        let
          val sm = until_root_in_range m k1 k2
        in
          case sm of
            Leaf => default_val
          | Node {l, k, v, r, ...} =>
              let
                val ra = aug_proj_left r k2 default_val
                val la = aug_proj_right l k1 default_val
                val ka = ga (T.g (k, v))
              in
                fa (fa (la, ka), ra)
              end
        end
    in
      aug_proj m
    end

  fun aug_range m k1 k2 = aug_project (fn x => x) (T.f) m k1 k2

  fun aug_left m k =
    case m of
      Leaf => T.id
    | Node {l, k = k', v, r, ...} =>
        case T.compare(k, k') of
          LESS => aug_left l k
        | EQUAL => T.f (aug_val l, T.g(k, v))
        | GREATER => T.f (aug_val l, T.f (T.g(k, v), aug_left r k))

  fun print_tree m indent =
    case m of
      Leaf => print (indent ^ "Leaf")
    | Node {l, k, v, r, a, size} =>
        let
          val _ = print "("
          val _ = print_tree l (indent^"")
          val _ = print (", "  ^(T.debug (k, v, a)))
          val _ = print (", weight = "  ^ (Int.toString size) ^ ",")
          val _ = print_tree r (indent^"")
        in
          print ")"
        end

end

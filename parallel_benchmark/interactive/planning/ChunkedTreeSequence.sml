signature BASE_SEQUENCE =
sig
  type 'a t
  type 'a seq = 'a t

  exception Range
  exception Size

  val length : 'a seq -> int
  val nth : 'a seq -> int -> 'a
  val singleton : 'a -> 'a seq
  val fromList : 'a list -> 'a seq
  val tabulate : (int -> 'a) -> int -> 'a seq
  val map : ('a -> 'b) -> 'a seq -> 'b seq
  val take : 'a seq -> int -> 'a seq
  val drop : 'a seq -> int -> 'a seq
  val rev : 'a seq -> 'a seq
  val iterate : ('b * 'a -> 'b) -> 'b -> 'a seq -> 'b
  val filter : ('a -> bool) -> 'a seq -> 'a seq
end

structure VectorBaseSequence :> BASE_SEQUENCE =
struct
  structure V = Vector
  structure VS = VectorSlice

  type 'a t = 'a VS.slice
  type 'a seq = 'a t

  exception Range
  exception Size

  val length = VS.length
  fun nth s i = VS.sub (s, i) handle Subscript => raise Range
  fun singleton x = VS.full (V.fromList [x])
  fun fromList l = VS.full (V.fromList l)
  fun tabulate f n = VS.full (V.tabulate (n, f))
  fun map f s = VS.full (VS.map f s)
  fun subseq s (i,l) =
    if l < 0 then raise Size else
    if i < 0 orelse i+l > length s then raise Range else
    VS.subslice (s, i, SOME l)
  fun take s n = subseq s (0, n)
  fun drop s n = subseq s (n, length s - n)
  fun rev s = tabulate (fn i => nth s (length s - i - 1)) (length s)
  fun iterate f b s = VS.foldl (fn (x,y) => f(y,x)) b s
  fun filter p s = fromList (VS.foldr (fn (x,l) => if p x then x :: l else l) [] s)
end

functor MkChunkedTreeSequence
  (structure BaseSeq : BASE_SEQUENCE
   val grainsize : int)
  :> SEQUENCE =
struct
  open Primitives
  exception NYI

  val alpha = 2;

  type 'a ord = 'a * 'a -> order

  datatype 'a tree = T0
                   | T1 of 'a BaseSeq.t
                   | T2 of int * int * 'a tree * 'a tree  (* T2 (length, weight, left, right) *)
  type 'a t = 'a tree
  type 'a seq = 'a t

  datatype 'a treeview = EMPTY | ONE of 'a | PAIR of 'a seq * 'a seq
  datatype 'a listview = NIL | CONS of 'a * 'a seq

  exception Range
  exception Size

  fun length S =
     case S of
       T0 => 0
     | T1 c => BaseSeq.length c
     | T2(n,_,_,_) => n

  fun weight s =
    case s of
      T0 => 0
    | T1 _ => 1
    | T2 (_,w,_,_) => w

  fun nth S i =
     case S of
       T0 => raise Range
     | T1 c => (BaseSeq.nth c i handle BaseSeq.Range => raise Range)
     | T2 (_,_,L,R) =>
        if i < length L then nth L i else nth R (i - length L)

  fun isBalanced s =
    case s of
      T0 => true
    | T1 c => (1 <= BaseSeq.length c) andalso (BaseSeq.length c <= grainsize)
    | T2 (n, w, l, r) => List.all (fn x => x) [
          isBalanced l, isBalanced r,
          n = length l + length r,
          w = weight l + weight r,
          Int.max (weight l, weight r) <= alpha * Int.min (weight l, weight r)
        ]

  fun verify s =
    if not (isBalanced s) then raise Fail "badly formatted tree seq" else s

  fun empty _ = T0
  fun singleton x = T1 (BaseSeq.singleton x)
  val $ = singleton

  fun makeT2 (l, r) =
    if length l + length r > grainsize
    then T2 (length l + length r, weight l + weight r, l, r)
    else let fun choose i = if i < length l then nth l i else nth r (i - length l)
         in T1 (BaseSeq.tabulate choose (length l + length r))
         end

  fun splitNonEmpty T0 = raise Fail "ChunkedTreeSequence: splitNonEmpty T0"
    | splitNonEmpty (T1 c) =
        if BaseSeq.length c <= 1
        then raise Fail "ChunkedTreeSequence: splitNonEmpty T1"
        else let val half = BaseSeq.length c div 2
             in (T1 (BaseSeq.take c half), T1 (BaseSeq.drop c half))
             end
    | splitNonEmpty (T2 (_, _, l, r)) = (l, r)

  fun makeNode(L,R) =
    let
       val lw = weight L
       val rw = weight R
       val w = lw + rw
    in
      if (lw > alpha * rw) then
        let val (LL,LR) = splitNonEmpty L
        in if ((alpha+1)*(weight LL) >= w)
           then makeT2 (LL, makeT2 (LR, R)) (* single rotation right *)
           else  (* double rotation *)
             let val (LRL,LRR) = splitNonEmpty LR
             in makeT2 (makeT2 (LL,LRL), makeT2 (LRR, R))
             end
        end
      else if (rw > alpha * lw) then
        let val (RL,RR) = splitNonEmpty R
        in if ((alpha+1)*(weight RR) >= w)
           then makeT2 (makeT2 (L, RL), RR) (* single rotation left *)
           else (* double rotation *)
             let val (RLL,RLR) = splitNonEmpty RL
             in makeT2 (makeT2 (L, RLL), makeT2 (RLR, RR))
             end
        end
      else makeT2(L,R) (* no rotation *)
    end

  fun append (s, t) =
     case (s,t) of
       (T0,_) => t
     | (_,T0) => s
     | _      =>
        let val ws = weight s
            val wt = weight t
        in if (ws > 2*wt) then
             let val (Ls,Rs) = splitNonEmpty s
             in makeNode(Ls,append(Rs,t))
             end
           else if (wt > 2*ws) then
             let val (Lt,Rt) = splitNonEmpty t
             in makeNode(append(s,Lt),Rt)
             end
           else makeT2 (s, t)
        end

  fun map f S =
     case S of
        T0 => T0
      | T1 c => T1 (BaseSeq.map f c)
      | T2 (n,w,L,R) =>
          let val (L',R') = par(fn () => map f L, fn () => map f R)
	        in T2 (n,w,L',R')
	        end

  fun tabulate f n =
    let
      fun tab(s,l) =
        if (l <= grainsize) then T1 (BaseSeq.tabulate (fn i => f(i+s)) l)
        else
          let val half = l div 2
              val (L,R) = par(fn () => tab(s,half),
	                            fn () => tab(s+half,l-half))
           in makeT2 (L,R)
           end
    in
      if n < 0 then raise Size
      else if n = 0 then empty ()
      else tab(0,n)
    end

  fun toString f s =
    "<" ^ String.concatWith "," (List.tabulate (length s, f o nth s)) ^ ">"

  fun fromList l =
    let
      fun pairWith f l =
        case l of
          [] => []
        | [x] => [x]
        | (x :: y :: l') => f (x, y) :: pairWith f l'
      fun reduceList f b l =
        case l of
          [] => b
        | [x] => x
        | _ => reduceList f b (pairWith f l)
      fun mapListChunk f s l =
        if List.length l <= s then [f l]
        else f (List.take (l, s)) :: mapListChunk f s (List.drop (l, s))
    in
      reduceList append (empty ()) (mapListChunk (fn l => T1 (BaseSeq.fromList l)) grainsize l)
    end

  (*fun fromList l =
     let
       fun flist(nil,_) = (T0,nil)
         | flist(a::r,n) =
         if (n=1) then (T1(a),r)
         else
           let val h = n div 2
               val (SL,rl) = flist(a::r,h)
               val (SR,rr) = flist(rl,n-h)
           in (T2(n,SL,SR),rr) end
        val (r,_) = flist(l,List.length l)
      in
          r
      end*)

  val % = fromList


  (* CONTINUE FROM HERE *)

  fun cut S i =
    case S of
      T0 => (T0,T0)
    | T1 c => (T1 (BaseSeq.take c (BaseSeq.length c div 2)),
               T1 (BaseSeq.drop c (BaseSeq.length c div 2)))
    | T2 (_,_,L,R) =>
        let val l = length L
        in
          if (i = l) then (L,R)
          else if (i < l) then
            let val (LL,LR) = cut L i
            in (LL,append(LR,R))
            end
          else
            let val (RL,RR) = cut R (i-l)
            in (append(L,RL),RR)
            end
	end

  fun subseq S (i, len') =
      if len' < 0 then raise Size
      else if i < 0 orelse i+len' > (length S) then raise Range
      else
        let
          val (_,R) = cut S i
          val (L,_) = cut R len'
        in L end

  fun take s n = subseq s (0, n)
  fun drop s n = subseq s (n, length s - n)

  fun splitHead T0 = NIL
    | splitHead s = CONS (nth s 0, drop s 1)

  fun splitMid s =
    case length s of
      0 => EMPTY
    | 1 => ONE (nth s 0)
    | n => PAIR (cut s (n div 2))

  fun rev s =
      case s of
        T0 => T0
      | T1 c => T1 (BaseSeq.rev c)
      | T2 (_,_,L,R) => makeT2 (par (fn () => rev R, fn () => rev L))

  fun iterate f b s =
    case s of
      T0 => b
    | T1 c => BaseSeq.iterate f b c
    | T2 (_, _, l, r) => iterate f (iterate f b l) r

  fun iteratePrefixes f b s =
    let val (ps, x) = iterate (fn ((ps, x), y) => (x :: ps, f (x, y))) ([], b) s
    in (rev (fromList ps), x)
    end

  fun iteratePrefixesIncl f b s =
    let val (partials, final) = iteratePrefixes f b s
    in drop (append (partials, singleton final)) 1
    end

  fun toList s = iterate (fn (l,x) => x::l) [] (rev s)

  fun merge cmp (s, t) =
    let
      (* Sequential merge. Pretend it's parallel! *)
      fun merge' [] t = t
        | merge' s [] = s
        | merge' (x::xs) (y::ys) =
          if cmp (y, x) = LESS
          then y::merge' (x::xs) ys
          else x::merge' xs (y::ys)

    in fromList (merge' (toList s) (toList t))
    end

  fun sort cmp s =
    case length s of
        0 => s
      | 1 => s
      | n => merge cmp (par (fn () => sort cmp (take s (n div 2)),
                           fn () => sort cmp (drop s (n div 2))))

  fun enum s = tabulate (fn i => (i, nth s i)) (length s)
  fun mapIdx f = map f o enum

  fun zipWith f (s, t) =
    tabulate (fn i => f (nth s i, nth t i)) (Int.min (length s, length t))

  fun unzipWith (spl : 'a -> 'b * 'c) s =
      let
        val n = length s
        val s' = map spl s
      in (tabulate (#1 o nth s') n, tabulate (#2 o nth s') n)
      end

  fun zip (s, t) = zipWith (fn x => x) (s, t)
  fun unzip s = unzipWith (fn x => x) s

  fun reduce f b s =
    case splitMid s of
      EMPTY => b
    | ONE x => x
    | PAIR (l, r) => f (par (fn () => reduce f b l, fn () => reduce f b r))

  (*fun reduce f b s =
    case length s
      of 0 => b
       | 1 => f (b, nth s 0)
       | n => let
                fun contract i =
                    if i = n div 2 then nth s (2*i)
                    else f (nth s (2*i), nth s (2*i+1))
              in reduce f b (tabulate contract ((n+1) div 2))
              end*)

  (* scan (1) combines base case at bottom of recursion *)
  fun scan f b s =
      case length s
        of 0 => (empty (), b)
         | 1 => (singleton b, f (b, nth s 0))
         | n =>
           let
             fun contract i =
                 if i = n div 2 then nth s (2*i)
                 else f (nth s (2*i), nth s (2*i+1))
             val s' = tabulate contract ((n+1) div 2)
             val (r, res) = scan f b s'
             fun expand i =
                 if i mod 2 = 0 then nth r (i div 2)
                 else f (nth r (i div 2), nth s (i-1))
           in (tabulate expand n, res)
           end

  (*local
    fun scan' f S =
        if length S = 1 then (empty (), nth S 0)
        else let
          val n = length S
          fun contract i =
              if i = n div 2 then nth S (2*i)
              else f (nth S (2*i), nth S (2*i + 1))
          val S' = tabulate contract ((n+1) div 2)
          val (R, res) = scan' f S'
          fun expand 0 = nth S 0
            | expand i =
              if i mod 2 = 1 then nth R (i div 2)
              else f (nth R ((i-1) div 2), nth S i)
        in (tabulate expand (n-1), res)
        end
  in
    (* scan (2) combines base case after recursion *)
    fun scan f b S =
        if length S = 0 then (empty (), b)
        else let
          val (R, res) = scan' f S
          val R' = map (fn x => f (b, x)) R
        in (append (singleton b, R'), f (b, res))
        end
  end*)

  fun scanIncl f b s =
    let val (r, res) = scan f b s
    in drop (append (r, singleton res)) 1
    end

  fun flatten ss = reduce append (empty()) ss

  fun filter p s =
     case s of
       T0 => T0
     | T1 c =>
        let val c' = BaseSeq.filter p c
        in if BaseSeq.length c' = 0 then T0 else T1 c'
        end
     | T2(_, _, L, R) => append (par (fn () => filter p L, fn () => filter p R))

  fun filterIdx p =
      map (fn (_, x) => x) o (filter p) o enum

  fun equal cmp (s1,s2) =
      length s1 = length s2 andalso
      reduce (fn (x,y) => x andalso y) true (zipWith cmp (s1, s2))

  fun argmax cmp s =
      if length s = 0 then raise Range
      else let
        fun best (i, j) =
            if cmp (nth s j, nth s i) = GREATER then j else i
      in reduce best 0 (tabulate (fn i => i) (length s))
      end

  fun inject _ = raise NYI
  fun update (s, (i, x)) = inject (s, singleton (i, x))

  fun collect cmp s =
      let
        val n = length s
        val (ks, vs) = unzip (sort (fn ((x,_), (y,_)) => cmp (x,y)) s)

        fun dk (0, _) = true
          | dk (i, k) = cmp (nth ks (i-1), k) <> EQUAL

        val starts = map (fn (i, _) => i) (filter dk (enum ks))
        val lengths = zipWith op- (drop (append (starts, %[n])) 1, starts)

        fun make (i, len) = (nth ks i, subseq vs (i, len))
      in zipWith make (starts, lengths)
      end

  fun collate cmp (s1, s2) =
      case (splitHead s1, splitHead s2)
        of (NIL, NIL) => EQUAL
         | (NIL, _) => LESS
         | (_, NIL) => GREATER
         | (CONS (x, xs), CONS (y, ys)) =>
           case cmp (x, y)
             of EQUAL => collate cmp (xs, ys)
              | ord => ord
end

structure ChunkedTreeSequence = MkChunkedTreeSequence
  (structure BaseSeq = VectorBaseSequence
   val grainsize = 512)

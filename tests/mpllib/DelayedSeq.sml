functor MkDelayedSeq (Stream: STREAM) : SEQUENCE =
struct

  exception NYI
  exception Range
  exception Size

  (* structure Stream = DelayedStream *)

  val for = Util.for
  val par = ForkJoin.par
  val parfor = ForkJoin.parfor
  val alloc = ForkJoin.alloc

  val gran = 5000
  val blockSize = 5000
  fun numBlocks n = Util.ceilDiv n blockSize

  fun blockStart b n = b * blockSize
  fun blockEnd b n = Int.min (n, (b+1) * blockSize)
  fun getBlockSize b n = blockEnd b n - blockStart b n
  fun convertToBlockIdx i n =
    (i div blockSize, i mod blockSize)

  structure A =
  struct
    open Array
    type 'a t = 'a array
    fun nth a i = sub (a, i)
  end

  structure AS =
  struct
    open ArraySlice
    type 'a t = 'a slice
    fun nth a i = sub (a, i)
  end


  type 'a rad = int * int * (int -> 'a)
  type 'a bid = int * (int -> 'a Stream.t)
  datatype 'a seq =
    Full of 'a AS.t
  | Rad of 'a rad
  | Bid of 'a bid

  type 'a t = 'a seq


  fun radlength (start, stop, _) = stop-start
  fun radnth (start, _, f) i = f (start+i)


  fun length s =
    case s of
      Full slice => AS.length slice
    | Rad rad => radlength rad
    | Bid (n, _) => n


  fun nth s i =
    case s of
      Full slice => AS.nth slice i
    | Rad rad => radnth rad i
    | Bid (n, getBlock) =>
        let
          val (outer, inner) = convertToBlockIdx i n
        in
          Stream.nth (getBlock outer) inner
        end


  fun bidify (s: 'a seq) : 'a bid =
    let
      fun block start nth b =
        Stream.tabulate (fn i => nth (start + b * blockSize + i))
    in
      case s of
        Full slice =>
          let
            val (a, start, n) = AS.base slice
          in
            (n, block start (A.nth a))
          end

      | Rad (start, stop, nth) =>
          (stop-start, block start nth)

      | Bid xx => xx
    end


  fun applyIdx (s: 'a seq) (g: int * 'a -> unit) =
    let
      val (n, getBlock) = bidify s
    in
      parfor 1 (0, numBlocks n) (fn b =>
        let
          val lo = blockStart b n
        in
          Stream.applyIdx (getBlockSize b n, getBlock b) (fn (j, x) => g (lo+j, x))
        end)
    end


  fun apply (s: 'a seq) (g: 'a -> unit) =
    applyIdx s (fn (_, x) => g x)


  fun reify s =
    let
      val a = alloc (length s)
    in
      applyIdx s (fn (i, x) => A.update (a, i, x));
      AS.full a
    end


  fun force s = Full (reify s)


  fun radify s =
    case s of
      Full slice =>
        let
          val (a, i, n) = AS.base slice
        in
          (i, i+n, A.nth a)
        end

    | Rad xx => xx

    | Bid (n, blocks) =>
        radify (force s)


  fun tabulate f n =
    Rad (0, n, f)


  fun fromList xs =
    Full (AS.full (Array.fromList xs))


  fun % xs =
    fromList xs


  fun singleton x =
    Rad (0, 1, fn _ => x)


  fun $ x =
    singleton x


  fun empty () =
    fromList []


  fun fromArraySeq a =
    Full a


  fun range (i, j) =
    Rad (i, j, fn k => k)


  fun toArraySeq s =
    case s of
      Full x => x
    | _ => reify s


  fun map f s =
    case s of
      Full _ => map f (Rad (radify s))
    | Rad (i, j, g) => Rad (i, j, f o g)
    | Bid (n, getBlock) => Bid (n, Stream.map f o getBlock)


  fun mapIdx f s =
    case s of
      Full _ => mapIdx f (Rad (radify s))
    | Rad (i, j, g) => Rad (0, j-i, fn k => f (k, g (i+k)))
    | Bid (n, getBlock) =>
        Bid (n, fn b =>
          Stream.mapIdx (fn (i, x) => f (b*blockSize + i, x)) (getBlock b))


  fun enum s =
    mapIdx (fn (i,x) => (i,x)) s


  fun flatten (ss: 'a seq seq) : 'a seq =
    let
      val numChildren = length ss
      val children: 'a rad AS.t = reify (map radify ss)
      val offsets =
        SeqBasis.scan gran op+ 0 (0, numChildren) (radlength o AS.nth children)
      val totalLen = A.nth offsets numChildren
      fun offset i = A.nth offsets i

      val getBlock =
        Stream.makeBlockStreams
          { blockSize = blockSize
          , numChildren = numChildren
          , offset = offset
          , getElem = (fn i => fn j => radnth (AS.nth children i) j)
          }
    in
      Bid (totalLen, getBlock)
    end


  fun mapOption (f: 'a -> 'b option) (s: 'a seq) =
    let
      val (n, getBlock) = bidify s
      val nb = numBlocks n
      val packed: 'b rad array =
        SeqBasis.tabulate 1 (0, nb) (fn b =>
          radify (Full (Stream.pack f (getBlockSize b n, getBlock b)))
        )
      val offsets =
        SeqBasis.scan gran op+ 0 (0, nb) (radlength o A.nth packed)
      val totalLen = A.nth offsets nb
      fun offset i = A.nth offsets i

      val getBlock' =
        Stream.makeBlockStreams
          { blockSize = blockSize
          , numChildren = nb
          , offset = offset
          , getElem = (fn i => fn j => radnth (A.nth packed i) j)
          }
    in
      Bid (totalLen, getBlock')
    end


  fun filter p s =
    mapOption (fn x => if p x then SOME x else NONE) s


  fun inject (s, u) =
    let
      val a = reify s
      val (base, i, _) = AS.base a
    in
      apply u (fn (j, x) => Array.update (base, i+j, x));
      Full a
    end


  fun bidZipWith f (s1, s2) =
    let
      val (n, getBlock1) = bidify s1
      val (_, getBlock2) = bidify s2
    in
      Bid (n, fn b => Stream.zipWith f (getBlock1 b, getBlock2 b))
    end

  fun radZipWith f (s1, s2) =
    let
      val (lo1, hi1, nth1) = radify s1
      val (lo2, _, nth2) = radify s2
    in
      Rad (0, hi1-lo1, fn i => f (nth1 (lo1+i), nth2 (lo2+i)))
    end

  fun zipWith f (s1, s2) =
    if length s1 <> length s2 then raise Size else
    case (s1, s2) of
      (Bid _, _) => bidZipWith f (s1, s2)
    | (_, Bid _) => bidZipWith f (s1, s2)
    | _ => radZipWith f (s1, s2)

  fun zip (s1, s2) =
    zipWith (fn (x, y) => (x, y)) (s1, s2)


  fun scan f z s =
    let
      val (n, getBlock) = bidify s
      val nb = numBlocks n
      val blockSums =
        SeqBasis.tabulate 1 (0, nb) (fn b =>
          Stream.iterate f z (getBlockSize b n, getBlock b)
        )
      val p = SeqBasis.scan gran f z (0, nb) (A.nth blockSums)
      val t = A.nth p nb
      val r = Bid (n, fn b => Stream.iteratePrefixes f (A.nth p b) (getBlock b))
    in
      (r, t)
    end


  fun scanIncl f z s =
    let
      val (n, getBlock) = bidify s
      val nb = numBlocks n
      val blockSums =
        SeqBasis.tabulate 1 (0, nb) (fn b =>
          Stream.iterate f z (getBlockSize b n, getBlock b)
        )
      val p = SeqBasis.scan gran f z (0, nb) (A.nth blockSums)
    in
      Bid (n, fn b =>
        Stream.iteratePrefixesIncl f (A.nth p b) (getBlock b))
    end


  fun reduce f z s =
    case s of
      Full xx => SeqBasis.reduce gran f z (0, length s) (AS.nth xx)
    | Rad xx => SeqBasis.reduce gran f z (0, length s) (radnth xx)
    | Bid (n, getBlock) =>
        let
          val nb = numBlocks n
        in
          SeqBasis.reduce gran f z (0, nb) (fn b =>
            Stream.iterate f z (getBlockSize b n, getBlock b)
          )
        end


  fun iterate f z s =
    case s of
      Full xx => SeqBasis.foldl f z (0, length s) (AS.nth xx)
    | Rad xx => SeqBasis.foldl f z (0, length s) (radnth xx)
    | Bid (n, getBlock) =>
        Util.loop (0, numBlocks n) z (fn (z, b) =>
          Stream.iterate f z (getBlockSize b n, getBlock b))


  fun rev s =
    let
      val n = length s
      val rads = radify s
    in
      tabulate (fn i => radnth rads (n-i-1)) n
    end


  fun append (s, t) =
    let
      val n = length s
      val m = length t

      val rads = radify s
      val radt = radify t

      fun elem i = if i < n then radnth rads i else radnth radt (i-n)
    in
      tabulate elem (n+m)
    end


  fun subseq s (i, len) =
    if i < 0 orelse len < 0 orelse i+len > length s then
      raise Subscript
    else
      let
        val n = length s
        val (start, stop, nth) = radify s
      in
        Rad (start+i, start+i+len, nth)
      end


  fun take s n = subseq s (0, n)
  fun drop s n = subseq s (n, length s - n)


  fun toList s =
    List.rev (iterate (fn (elems, x) => x :: elems) [] s)

  fun toString f s =
    "[" ^ String.concatWith "," (toList (map f s)) ^ "]"

  (* ===================================================================== *)

  datatype 'a listview = NIL | CONS of 'a * 'a seq
  datatype 'a treeview = EMPTY | ONE of 'a | PAIR of 'a seq * 'a seq

  type 'a ord = 'a * 'a -> order
  type 'a t = 'a seq

  fun filterIdx x = raise NYI
  fun iterateIdx x = raise NYI

  fun argmax x = raise NYI
  fun collate x = raise NYI
  fun collect x = raise NYI
  fun equal x = raise NYI
  fun iteratePrefixes x = raise NYI
  fun iteratePrefixesIncl x = raise NYI
  fun merge x = raise NYI
  fun sort x = raise NYI
  fun splitHead x = raise NYI
  fun splitMid x = raise NYI
  fun update x = raise NYI
  fun zipWith3 x = raise NYI

  fun filterSome x = raise NYI
  fun foreach x = raise NYI
  fun foreachG x = raise NYI

end



structure DelayedSeq = MkDelayedSeq (DelayedStream)
(* structure DelayedSeq = MkDelayedSeq (RecursiveStream) *)

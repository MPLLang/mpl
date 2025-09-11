structure PureSeq :>
sig
  type 'a seq = 'a VectorSlice.slice
  type 'a t = 'a seq

  val nth: 'a seq -> int -> 'a
  val length: 'a seq -> int

  val empty: unit -> 'a seq
  val fromList: 'a list -> 'a seq
  val fromSeq: 'a Seq.t -> 'a seq

  val tabulate: (int -> 'a) -> int -> 'a seq
  val tabulateG: int -> (int -> 'a) -> int -> 'a seq
  val map: ('a -> 'b) -> 'a seq -> 'b seq

  val filter: ('a -> bool) -> 'a seq -> 'a seq
  val filterIdx: (int * 'a -> bool) -> 'a seq -> 'a seq

  val reduce: ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a
  val scan: ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a seq * 'a
  val scanIncl: ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a seq

  val subseq: 'a seq -> int * int -> 'a seq
  val take: 'a seq -> int -> 'a seq
  val drop: 'a seq -> int -> 'a seq

  val merge: ('a * 'a -> order) -> 'a seq * 'a seq -> 'a seq
  val quicksort: ('a * 'a -> order) -> 'a seq -> 'a seq

  val summarize: int -> ('a -> string) -> 'a seq -> string

  val foreach: 'a seq -> (int * 'a -> unit) -> unit
end =
struct

  structure A = Array
  structure AS = ArraySlice
  structure V = Vector
  structure VS = VectorSlice

  val gran = 5000

  type 'a seq = 'a VS.slice
  type 'a t = 'a seq

  val unsafeCast: 'a array -> 'a vector = VectorExtra.unsafeFromArray

  fun nth s i = VS.sub (s, i)
  fun length s = VS.length s
  fun empty () = VS.full (V.fromList [])
  fun fromList xs = VS.full (V.fromList xs)

  fun subseq s (i, n) = VS.subslice (s, i, SOME n)
  fun take s k = subseq s (0, k)
  fun drop s k = subseq s (k, length s - k)

  fun tabulate f n =
    VS.full (unsafeCast (SeqBasis.tabulate gran (0, n) f))

  fun tabulateG gran f n =
    VS.full (unsafeCast (SeqBasis.tabulate gran (0, n) f))

  fun map f s =
    tabulate (f o nth s) (length s)

  fun filter p s =
    VS.full (unsafeCast (SeqBasis.filter gran (0, length s) (nth s) (p o nth s)))

  fun filterIdx p s =
    VS.full (unsafeCast (SeqBasis.filter gran (0, length s) (nth s)
      (fn i => p (i, nth s i))
    ))

  fun foreach s f =
    ForkJoin.parfor gran (0, length s) (fn i => f (i, nth s i))

  fun fromSeq xs =
    tabulate (Seq.nth xs) (Seq.length xs)

  fun reduce f b s =
    SeqBasis.reduce gran f b (0, length s) (nth s)

  fun scan f b s =
    let
      val n = length s
      val v = VS.full (unsafeCast (SeqBasis.scan gran f b (0, n) (nth s)))
    in
      (take v n, nth v n)
    end

  fun scanIncl f b s =
    let
      val n = length s
      val v = VS.full (unsafeCast (SeqBasis.scan gran f b (0, n) (nth s)))
    in
      drop v 1
    end

  (** ========================================================================
    * Merge
    *
    * This is copied from the Merge.sml implementation and modified slightly
    * to make it work with vectors. Really should just parameterize the
    * Merge.sml by a Seq implementation... or do the func-sequence trick
    * (pass the input sequences by length/nth functions)
    *)

  fun sliceIdxs s i j =
    VS.subslice (s, i, SOME (j-i))

  fun arraySliceIdxs s i j =
    AS.subslice (s, i, SOME (j-i))

  fun search cmp s x =
    let
      fun loop lo hi =
        case hi - lo of
          0 => lo
        | n =>
          let
            val mid = lo + n div 2
            val pivot = nth s mid
          in
            case cmp (x, pivot) of
              LESS    => loop lo mid
            | EQUAL   => mid
            | GREATER => loop (mid+1) hi
          end
    in
      loop 0 (length s)
    end

  fun writeMergeSerial cmp (s1, s2) t =
    let
      fun write i x = AS.update (t, i, x)

      val n1 = length s1
      val n2 = length s2

      (* i1 index into s1
       * i2 index into s2
       * j index into output *)
      fun loop i1 i2 j =
        if i1 = n1 then
          foreach (sliceIdxs s2 i2 n2) (fn (i, x) => write (i+j) x)
        else if i2 = n2 then
          foreach (sliceIdxs s1 i1 n1) (fn (i, x) => write (i+j) x)
        else
          let
            val x1 = nth s1 i1
            val x2 = nth s2 i2
          in
            case cmp (x1, x2) of
              LESS => (write j x1; loop (i1+1) i2 (j+1))
            | _    => (write j x2; loop i1 (i2+1) (j+1))
          end
    in
      loop 0 0 0
    end

  fun writeMerge cmp (s1: 'a seq, s2: 'a seq) (t: 'a AS.slice) =
    if AS.length t <= gran then
      writeMergeSerial cmp (s1, s2) t
    else if length s1 = 0 then
      foreach s2 (fn (i, x) => AS.update (t, i, x))
    else
      let
        val n1 = length s1
        val n2 = length s2
        val mid1 = n1 div 2
        val pivot = nth s1 mid1
        val mid2 = search cmp s2 pivot

        val l1 = sliceIdxs s1 0 mid1
        val r1 = sliceIdxs s1 (mid1+1) n1
        val l2 = sliceIdxs s2 0 mid2
        val r2 = sliceIdxs s2 mid2 n2

        val _ = AS.update (t, mid1+mid2, pivot)
        val tl = arraySliceIdxs t 0 (mid1+mid2)
        val tr = arraySliceIdxs t (mid1+mid2+1) (AS.length t)
      in
        ForkJoin.par
          (fn _ => writeMerge cmp (l1, l2) tl,
           fn _ => writeMerge cmp (r1, r2) tr);
        ()
      end

  fun merge cmp (s1, s2) =
    let
      val out = ForkJoin.alloc (length s1 + length s2)
    in
      writeMerge cmp (s1, s2) (AS.full out);
      VS.full (unsafeCast out)
    end

  fun quicksort cmp s =
    let
      val out = SeqBasis.tabulate gran (0, length s) (nth s)
    in
      Quicksort.sortInPlace cmp (AS.full out);
      VS.full (unsafeCast out)
    end

  fun summarize count toString xs =
    let
      val n = length xs
      fun elem i = nth xs i

      val strs =
        if count <= 0 then raise Fail "PureSeq.summarize needs count > 0"
        else if count <= 2 orelse n <= count then
          List.tabulate (n, toString o elem)
        else
          List.tabulate (count-1, toString o elem) @
          ["...", toString (elem (n-1))]
    in
      "[" ^ (String.concatWith ", " strs) ^ "]"
    end

end

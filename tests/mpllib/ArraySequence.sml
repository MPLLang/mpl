structure ArraySequence =
struct

  val for = Util.for
  val par = ForkJoin.par
  val parfor = ForkJoin.parfor
  val alloc = ForkJoin.alloc


  val GRAN = 5000


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


  type 'a t = 'a AS.t
  type 'a seq = 'a t

  (* for compatibility across all sequence implementations *)
  fun fromArraySeq s = s
  fun toArraySeq s = s
  fun force s = s


  val nth = AS.nth
  val length = AS.length

  fun empty () = AS.full (A.fromList [])
  fun singleton x = AS.full (A.array (1, x))
  val $ = singleton
  fun toString f s =
    "<" ^ String.concatWith "," (List.tabulate (length s, f o nth s)) ^ ">"


  fun fromArray a = AS.full a


  fun fromList l = AS.full (A.fromList l)
  val % = fromList
  fun toList s =
    SeqBasis.foldl (fn (list, x) => x :: list) []
      (0, length s) (fn i => nth s (length s - i - 1))


  fun subseq s (i, k) =
    AS.subslice (s, i, SOME k)
  fun take s n = subseq s (0, n)
  fun drop s n = subseq s (n, length s - n)
  fun first s = nth s 0
  fun last s = nth s (length s - 1)

  fun tabulate f n =
    AS.full (SeqBasis.tabulate GRAN (0, n) f)


  fun map f s =
    tabulate (f o nth s) (length s)


  fun mapIdx f s =
    tabulate (fn i => f (i, nth s i)) (length s)


  fun enum s =
    mapIdx (fn xx => xx) s


  fun zipWith f (s, t) =
    tabulate
      (fn i => f (nth s i, nth t i))
      (Int.min (length s, length t))


  fun zipWith3 f (s1, s2, s3) =
    tabulate
      (fn i => f (nth s1 i, nth s2 i, nth s3 i))
      (Int.min (length s1, Int.min (length s2, length s3)))


  fun zip (s, t) =
    zipWith (fn xx => xx) (s, t)


  fun rev s =
    tabulate (fn i => nth s (length s - 1 - i)) (length s)


  (** TODO: make faster *)
  fun fromRevList list = rev (fromList list)


  fun append (s, t) =
    let
      val (ns, nt) = (length s, length t)
      fun ith i = if i < ns then nth s i else nth t (i-ns)
    in
      tabulate ith (ns + nt)
    end


  fun append3 (a, b, c) =
    let
      val (na, nb, nc) = (length a, length b, length c)
      fun ith i =
        if i < na then
          nth a i
        else if i < na + nb then
          nth b (i - na)
        else
          nth c (i - na - nb)
    in
      tabulate ith (na + nb + nc)
    end


  fun foldl f b s =
    SeqBasis.foldl f b (0, length s) (nth s)

  fun foldr f b s =
    SeqBasis.foldr f b (0, length s) (nth s)

  fun iterate f b s =
    SeqBasis.foldl f b (0, length s) (nth s)


  fun iteratePrefixes f b s =
    let
      val prefixes = alloc (length s)
      fun g ((i, b), a) =
        let
          val _ = A.update (prefixes, i, b)
        in
          (i+1, f (b, a))
        end
      val (_, r) = iterate g (0, b) s
    in
        (AS.full prefixes, r)
    end


  fun reduce f b s =
    SeqBasis.reduce GRAN f b (0, length s) (nth s)


  fun scan f b s =
    let
      val p = AS.full (SeqBasis.scan GRAN f b (0, length s) (nth s))
    in
      (take p (length s), nth p (length s))
    end

  fun scanWithTotal f b s =
    AS.full (SeqBasis.scan GRAN f b (0, length s) (nth s))

  fun scanIncl f b s =
    let
      val p = AS.full (SeqBasis.scan GRAN f b (0, length s) (nth s))
    in
      drop p 1
    end


  fun filter p s =
    (* Assumes that the predicate p is pure *)
    AS.full (SeqBasis.filter GRAN (0, length s) (nth s) (p o nth s))

  fun filterSafe p s =
    (* Does not assume that the predicate p is pure *)
    AS.full (SeqBasis.tabFilter GRAN (0, length s) (fn i => if p (nth s i) then SOME (nth s i) else NONE))

  fun filterIdx p s =
    AS.full (SeqBasis.filter GRAN (0, length s) (nth s) (fn i => p (i, nth s i)))

  fun filtermap (p: 'a -> bool) (f:'a -> 'b) (s: 'a t): 'b t =
     AS.full (SeqBasis.filter GRAN (0, length s) (fn i => f (nth s i)) (p o nth s))


  fun mapOption f s =
    AS.full (SeqBasis.tabFilter GRAN (0, length s) (f o nth s))


  fun equal eq (s, t) =
    length s = length t andalso
    SeqBasis.reduce GRAN (fn (a, b) => a andalso b) true (0, length s)
      (fn i => eq (nth s i, nth t i))


  fun inject (s, updates) =
    let
      val result = map (fn x => x) s
    in
      parfor GRAN (0, length updates) (fn i =>
        let
          val (idx, r) = nth updates i
        in
          AS.update (result, idx, r)
        end);

      result
    end


  fun applyIdx s f =
    parfor GRAN (0, length s) (fn i => f (i, nth s i))


  fun foreach s f = applyIdx s f


  fun indexSearch (start, stop, offset: int -> int) k =
    case stop-start of
      0 =>
        raise Fail "ArraySequence.indexSearch: should not have hit 0"
    | 1 =>
        start
    | n =>
        let
          val mid = start + (n div 2)
        in
          if k < offset mid then
            indexSearch (start, mid, offset) k
          else
            indexSearch (mid, stop, offset) k
        end


  fun flatten s =
    let
      val offsets = SeqBasis.scan GRAN op+ 0 (0, length s) (length o nth s)
      fun offset i = A.nth offsets i
      val total = offset (length s)
      val result = alloc total

      val blockSize = GRAN
      val numBlocks = Util.ceilDiv total blockSize
    in
      parfor 1 (0, numBlocks) (fn blockIdx =>
        let
          val lo = blockIdx * blockSize
          val hi = Int.min (lo + blockSize, total)

          val firstOuterIdx = indexSearch (0, length s, offset) lo
          val firstInnerIdx = lo - offset firstOuterIdx

          (** i = outer index
            * j = inner index
            * k = output index, ranges from [lo] to [hi]
            *)
          fun loop i j k =
            if k >= hi then () else
            let
              val inner = nth s i
              val numAvailableHere = length inner - j
              val numRemainingInBlock = hi - k
              val numHere = Int.min (numAvailableHere, numRemainingInBlock)
            in
              for (0, numHere) (fn z => A.update (result, k+z, nth inner (j+z)));
              loop (i+1) 0 (k+numHere)
            end
        in
          loop firstOuterIdx firstInnerIdx lo
        end);

      AS.full result
    end


end

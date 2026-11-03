structure OldDelayedSeq =
struct

  val for = Util.for

  val par = ForkJoin.par
  val parfor = ForkJoin.parfor
  val alloc = ForkJoin.alloc

  val gran = 5000


  val blockSize = 10000
  fun numBlocks n = Util.ceilDiv n blockSize


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


  (* Using given offsets, find which inner sequence contains index [k] *)
  fun indexSearch (start, stop, offset: int -> int) k =
    case stop-start of
      0 =>
        raise Fail "OldDelayedSeq.indexSearch: should not have hit 0"
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


  (* ======================================================================= *)


  structure Stream:>
  sig
    type 'a t
    type 'a stream = 'a t

    val tabulate: (int -> 'a) -> 'a stream
    val map: ('a -> 'b) -> 'a stream -> 'b stream
    val mapIdx: (int * 'a -> 'b) -> 'a stream -> 'b stream
    val zipWith: ('a * 'b -> 'c) -> 'a stream * 'b stream -> 'c stream
    val iteratePrefixes: ('b * 'a -> 'b) -> 'b -> 'a stream -> 'b stream
    val iteratePrefixesIncl: ('b * 'a -> 'b) -> 'b -> 'a stream -> 'b stream

    val applyIdx: int * 'a stream -> (int * 'a -> unit) -> unit
    val iterate: ('b * 'a -> 'b) -> 'b -> int * 'a stream -> 'b

    val makeBlockStreams:
      { numChildren: int
      , offset: int -> int
      , getElem: int -> int -> 'a
      }
      -> (int -> 'a stream)

  end =
  struct

    (** A stream is a generator for a stateful trickle function:
      *   trickle = stream ()
      *   x0 = trickle 0
      *   x1 = trickle 1
      *   x2 = trickle 2
      *   ...
      *
      *  The integer argument is just an optimization (it could be packaged
      *  up into the state of the trickle function, but doing it this
      *  way is more efficient). Requires passing `i` on the ith call
      *  to trickle.
      *)
    type 'a t = unit -> int -> 'a
    type 'a stream = 'a t


    fun tabulate f =
      fn () => f


    fun map g stream =
      fn () =>
        let
          val trickle = stream ()
        in
          g o trickle
        end


    fun mapIdx g stream =
      fn () =>
        let
          val trickle = stream ()
        in
          fn idx => g (idx, trickle idx)
        end


    fun applyIdx (length, stream) g =
      let
        val trickle = stream ()
        fun loop i =
          if i >= length then () else (g (i, trickle i); loop (i+1))
      in
        loop 0
      end


    fun iterate g b (length, stream) =
      let
        val trickle = stream ()
        fun loop b i =
          if i >= length then b else loop (g (b, trickle i)) (i+1)
      in
        loop b 0
      end


    fun iteratePrefixes g b stream =
      fn () =>
        let
          val trickle = stream ()
          val stuff = ref b
        in
          fn idx =>
            let
              val acc = !stuff
              val elem = trickle idx
              val acc' = g (acc, elem)
            in
              stuff := acc';
              acc
            end
        end


    fun iteratePrefixesIncl g b stream =
      fn () =>
        let
          val trickle = stream ()
          val stuff = ref b
        in
          fn idx =>
            let
              val acc = !stuff
              val elem = trickle idx
              val acc' = g (acc, elem)
            in
              stuff := acc';
              acc'
            end
        end


    fun zipWith g (s1, s2) =
      fn () =>
        let
          val trickle1 = s1 ()
          val trickle2 = s2 ()
        in
          fn idx => g (trickle1 idx, trickle2 idx)
        end


    fun makeBlockStreams
          { numChildren: int
          , offset: int -> int
          , getElem: int -> int -> 'a
          } =
      let
        fun getBlock blockIdx =
          let
            val lo = blockIdx * blockSize
            val firstOuterIdx = indexSearch (0, numChildren, offset) lo
            (* val firstInnerIdx = lo - offset firstOuterIdx *)

            fun advanceUntilNonEmpty i =
              if i >= numChildren orelse offset i <> offset (i+1) then
                i
              else
                advanceUntilNonEmpty (i+1)
          in
            fn () =>
              let
                val outerIdx = ref firstOuterIdx
                (* val innerIdx = ref firstInnerIdx *)
              in
                fn idx =>
                  let
                    val i = !outerIdx
                    val j = lo + idx - offset i
                    (* val j = !innerIdx *)
                    val elem = getElem i j
                  in
                    if offset i + j + 1 < offset (i+1) then
                      (* innerIdx := j+1 *) ()
                    else
                      ( outerIdx := advanceUntilNonEmpty (i+1)
                      (* ; innerIdx := 0 *)
                      );

                    elem
                  end
              end
          end

      in
        getBlock
      end


  end


  (* ======================================================================= *)


  datatype 'a flat =
    Full of 'a AS.t

    (** [Delay (start, stop, lookup)]
      * start index is inclusive, stop is exclusive
      * length is [stop-start]
      * the ith element lives at [lookup (start+i)]
      *)
  | Delay of int * int * (int -> 'a)


  datatype 'a seq =
    Flat of 'a flat

    (** [Nest (length, getBlock)]
      * The block sizes are implicit: [gran]
      * The number of block is implicit: ceil(length / gran)
      *)
  | Nest of int * (int -> 'a Stream.t)


  fun makeBlocks s =
    case s of
      Flat (Full slice) =>
        let
          fun blockStream b =
            Stream.tabulate (fn i => AS.nth slice (b*blockSize + i))
        in
          blockStream
        end
    | Flat (Delay (start, _, f)) =>
        let
          fun blockStream b =
            Stream.tabulate (fn i => f (start + b*blockSize + i))
        in
          blockStream
        end
    | Nest (_, blockStream) =>
        blockStream


  fun subseq s (i, k) =
    case s of
      Flat (Delay (start, stop, f)) => Flat (Delay (start+i, start+i+k, f))
    | Flat (Full slice) => Flat (Full (AS.subslice (slice, i, SOME k)))
    | _ => raise Fail "delay subseq (Nest) not implemented yet"


  fun flatNth (s: 'a flat) k =
    case s of
      Full slice =>
        AS.nth slice k
    | Delay (i, j, f) =>
        f (i+k)


  fun nth (s: 'a seq) k =
    case s of
      Flat xs =>
        flatNth xs k
    | Nest _ =>
        raise Fail "delay nth (Nest) not implemented yet"


  fun flatLength s =
    case s of
      Full slice =>
        AS.length slice
    | Delay (i, j, f) =>
        j-i


  fun length s =
    case s of
      Flat xs =>
        flatLength xs
    | Nest (n, _) =>
        n


  fun flatSubseqIdxs s (i, j) =
    case s of
      Full slice =>
        Full (AS.subslice (slice, i, SOME (j-i)))
    | Delay (start, stop, f) =>
        Delay (start+i, start+j, f)


  fun flatIterateIdx (g: 'b * (int * 'a) -> 'b) (b: 'b) s =
    case s of
      Full slice =>
        SeqBasis.foldl g b (0, AS.length slice) (fn i => (i, AS.nth slice i))
    | Delay (i, j, f) =>
        SeqBasis.foldl g b (i, j) (fn k => (k-i, f k))


  fun iterateIdx g b s =
    case s of
      Flat xs =>
        flatIterateIdx g b xs
    | Nest _ =>
        raise Fail "delay iterateIdx (Nest) NYI"


  fun flatIterate g b s =
    flatIterateIdx (fn (b, (_, x)) => g (b, x)) b s


  fun iterate g b s =
    iterateIdx (fn (b, (_, x)) => g (b, x)) b s


  fun applyIdx (s: 'a seq) (g: int * 'a -> unit) =
    case s of
      Flat (Full slice) =>
        parfor gran (0, AS.length slice) (fn i => g (i, AS.nth slice i))
    | Flat (Delay (i, j, f)) =>
        parfor gran (0, j-i) (fn k => g (k, f (i+k)))
    | Nest (n, getBlock) =>
        parfor 1 (0, numBlocks n) (fn i =>
          let
            val lo = i*blockSize
            val hi = Int.min (lo+blockSize, n)
          in
            Stream.applyIdx (hi-lo, getBlock i) (fn (j, x) => g (lo+j, x))
          end)


  fun apply (s: 'a seq) (g: 'a -> unit) =
    applyIdx s (fn (_, x) => g x)


  fun unravelAndCopy (s: 'a seq): 'a array =
    let
      val n = length s
      val result = alloc n
    in
      applyIdx s (fn (i, x) => A.update (result, i, x));
      result
    end


  fun force s =
    case s of
      Flat (Full _) => s
    | _ => Flat (Full (AS.full (unravelAndCopy s)))


  fun forceFlat s =
    case s of
      Flat xx => xx
    | _ => Full (AS.full (unravelAndCopy s))


  fun tabulate f n =
    Flat (Delay (0, n, f))


  fun fromList xs =
    Flat (Full (AS.full (Array.fromList xs)))


  fun % xs =
    fromList xs


  fun singleton x =
    Flat (Delay (0, 1, fn _ => x))


  fun $ x =
    singleton x


  fun empty () =
    fromList []


  fun fromArraySeq a =
    Flat (Full a)


  fun range (i, j) =
    Flat (Delay (i, j, fn k => k))


  fun toArraySeq s =
    case s of
      Flat (Full x) => x
    | _ => AS.full (unravelAndCopy s)


  fun flatMap g s =
    case s of
      Full slice =>
        Delay (0, AS.length slice, g o AS.nth slice)
    | Delay (i, j, f) =>
        Delay (i, j, g o f)


  fun map g s =
    case s of
      Flat xs =>
        Flat (flatMap g xs)
    | Nest (n, getBlock) =>
        Nest (n, Stream.map g o getBlock)


  fun flatMapIdx g s =
    case s of
      Full slice =>
        Delay (0, AS.length slice, fn i => g (i, AS.nth slice i))
    | Delay (i, j, f) =>
        Delay (i, j, fn k => g (k-i, f i))


  fun mapIdx g s =
    case s of
      Flat xs =>
        Flat (flatMapIdx g xs)
    | Nest (n, getBlock) =>
        Nest (n, fn i =>
          Stream.mapIdx (fn (j, x) => g (i*blockSize + j, x)) (getBlock i))


  fun enum s =
    mapIdx (fn (i,x) => (i,x)) s


  fun flatten (ss: 'a seq seq) =
    let
      val numChildren = length ss
      val children: 'a flat array = unravelAndCopy (map forceFlat ss)
      val offsets =
        SeqBasis.scan gran op+ 0 (0, numChildren) (flatLength o A.nth children)
      val totalLen = A.nth offsets numChildren
      fun offset i = A.nth offsets i
      val getBlock =
        Stream.makeBlockStreams
          { numChildren = numChildren
          , offset = offset
          , getElem = (fn i => fn j => flatNth (A.nth children i) j)
          }
    in
      Nest (totalLen, getBlock)
    end


  fun flatRev s =
    case s of
      Full slice =>
        let
          val n = AS.length slice
        in
          Delay (0, n, fn i => AS.nth slice (n-i-1))
        end
    | Delay (i, j, f) =>
        Delay (i, j, fn k => f (j-k-1))


  fun rev s =
    case s of
      Flat xs =>
        Flat (flatRev xs)
    | Nest _ =>
        raise Fail "delay rev (Nest) NYI"


  fun reduceG newGran g b s =
    case s of
      Flat (Full slice) =>
        SeqBasis.reduce newGran g b (0, AS.length slice) (AS.nth slice)
    | Flat (Delay (i, j, f)) =>
        SeqBasis.reduce newGran g b (i, j) f
    | Nest (n, getBlock) =>
        let
          val nb = numBlocks n
          fun len i =
            if i < nb-1 then blockSize else n - i*blockSize
        in
          SeqBasis.reduce 1 g b (0, nb) (fn i =>
            Stream.iterate g b (len i, getBlock i))
        end


  fun reduce g b s =
    reduceG gran g b s


  (** =======================================================================
    * mapOption implementations
    *
    * first one delays output (like flattening)
    * second one has eager output
    *)

  fun mapOption1 (f: 'a -> 'b option) (s: 'a seq) : 'b seq =
    let
      val n = length s
      val nb = numBlocks n
      val getBlock: int -> 'a Stream.t =
        makeBlocks s

      val results: 'b array = alloc n

      fun packBlock b =
        let
          val start = b*blockSize
          val stop = Int.min (start+blockSize, n)
          val size = stop-start

          fun doNext (off, x) =
            case f x of
              NONE => off
            | SOME x' => (A.update (results, off, x'); off+1)

          val lastOffset =
            Stream.iterate doNext start (size, getBlock b)
        in
          lastOffset - start
        end

      val counts = SeqBasis.tabulate 1 (0, nb) packBlock
      val offsets = SeqBasis.scan 10000 op+ 0 (0, A.length counts) (A.nth counts)

      val totalLen = A.nth offsets nb
      fun offset i = A.nth offsets i
      val getBlock =
        Stream.makeBlockStreams
          { numChildren = nb
          , offset = offset
          , getElem = (fn i => fn j => A.nth results (i*blockSize + j))
          }
    in
      Nest (totalLen, getBlock)
    end


  fun mapOption2 (f: 'a -> 'b option) (s: 'a seq) =
    let
      val n = length s
      val nb = numBlocks n
      val getBlock: int -> 'a Stream.t =
        makeBlocks s

      val results: 'b array = alloc n

      fun packBlock b =
        let
          val start = b*blockSize
          val stop = Int.min (start+blockSize, n)
          val size = stop-start

          fun doNext (off, x) =
            case f x of
              NONE => off
            | SOME x' => (A.update (results, off, x'); off+1)

          val lastOffset =
            Stream.iterate doNext start (size, getBlock b)
        in
          lastOffset - start
        end

      val counts = SeqBasis.tabulate 1 (0, nb) packBlock
      val outOff = SeqBasis.scan 10000 op+ 0 (0, A.length counts) (A.nth counts)
      val outSize = A.sub (outOff, nb)

      val result = alloc outSize
    in
      parfor (n div (Int.max (outSize, 1))) (0, nb) (fn i =>
        let
          val soff = i * blockSize
          val doff = A.sub (outOff, i)
          val size = A.sub (outOff, i+1) - doff
        in
          Util.for (0, size) (fn j =>
            A.update (result, doff+j, A.sub (results, soff+j)))
        end);

      Flat (Full (ArraySlice.full result))
    end


  fun mapOption f s =
    (* mapOption1 f s *)
    mapOption2 f s


  fun filterIdx p s =
    case s of
      Flat (Full slice) =>
        Flat (Full (AS.full (SeqBasis.filter gran
          (0, AS.length slice)              (* range *)
          (AS.nth slice)                    (* index lookup *)
          (fn k => p (k, AS.nth slice k))   (* index predicate *)
        )))

    | Flat (Delay (i, j, f)) =>
        Flat (Full (AS.full (SeqBasis.filter gran
          (i, j)
          f
          (fn k => p (k, f k))
        )))

    | _ =>
        filterIdx p (force s)


  fun filter p s =
    filterIdx (fn (_, x) => p x) s


  fun inject (s, u) =
    let
      val base = unravelAndCopy s
    in
      apply u (fn (i, x) => A.update (base, i, x));
      Flat (Full (AS.full base))
    end


  fun injectG _ (s, u) =
    let
      val base = unravelAndCopy s
    in
      apply u (fn (i, x) => A.update (base, i, x));
      Flat (Full (AS.full base))
    end


  fun toList s =
    iterate (fn (xs, x) => x :: xs) [] (rev s)


  fun toString f s =
    "<" ^
    String.concatWith ","
      (iterate (fn (strs, next) => next :: strs) [] (map f (rev s)))
    ^ ">"


  fun append (s, t) =
    flatten (tabulate (fn 0 => s | _ => t) 2)


  (* Do the scan on a flat delayed sequence [f(i), f(i+1), ..., f(j-1)] *)
  fun scanDelay g b (i, j, f) =
    let
      val n = j-i
      val nb = numBlocks n

      val blockSums =
        SeqBasis.tabulate 1 (0, nb) (fn blockIdx =>
          let
            val blockStart = i + blockIdx*blockSize
            val blockEnd = Int.min (j, blockStart + blockSize)
          in
            SeqBasis.foldl g b (blockStart, blockEnd) f
          end)

      val partials =
        SeqBasis.scan gran g b (0, nb) (A.nth blockSums)

      val total = A.nth partials nb

      fun getChild blockIdx =
        let
          val firstElem = A.nth partials blockIdx
          val blockStart = i + blockIdx*blockSize
        in
          Stream.iteratePrefixes g firstElem
            (Stream.tabulate (fn k => f (blockStart+k)))
        end
    in
      ( Nest (n, getChild)
      , total
      )
    end


  fun scanInclDelay g b (i, j, f) =
    let
      val n = j-i
      val nb = numBlocks n

      val blockSums =
        SeqBasis.tabulate 1 (0, nb) (fn blockIdx =>
          let
            val blockStart = i + blockIdx*blockSize
            val blockEnd = Int.min (j, blockStart + blockSize)
          in
            SeqBasis.foldl g b (blockStart, blockEnd) f
          end)

      val partials =
        SeqBasis.scan gran g b (0, nb) (A.nth blockSums)

      fun getChild blockIdx =
        let
          val firstElem = A.nth partials blockIdx
          val blockStart = i + blockIdx*blockSize
        in
          Stream.iteratePrefixesIncl g firstElem
            (Stream.tabulate (fn k => f (blockStart+k)))
        end
    in
      Nest (n, getChild)
    end


  fun scanScan g b (n, getChild: int -> 'a Stream.t) =
    let
      val numChildren = Util.ceilDiv n blockSize
      fun len i =
        if i < numChildren-1 then blockSize else n - i*blockSize

      val childSums =
        SeqBasis.tabulate 1 (0, numChildren) (fn childIdx =>
          Stream.iterate g b (len childIdx, getChild childIdx)
        )

      val partials =
        SeqBasis.scan gran g b (0, numChildren) (A.nth childSums)
      val total = A.nth partials numChildren

      fun getChild' childIdx =
        let
          val childStream = getChild childIdx
          val initial = A.nth partials childIdx
        in
          Stream.iteratePrefixes g initial childStream
        end
    in
      ( Nest (n, getChild')
      , total
      )
    end


  fun scanScanIncl g b (n, getChild) =
    let
      val numChildren = Util.ceilDiv n blockSize
      fun len i =
        if i < numChildren-1 then blockSize else n - i*blockSize

      val childSums =
        SeqBasis.tabulate 1 (0, numChildren) (fn childIdx =>
          Stream.iterate g b (len childIdx, getChild childIdx)
        )

      val partials =
        SeqBasis.scan gran g b (0, numChildren) (A.nth childSums)

      fun getChild' childIdx =
        let
          val childStream = getChild childIdx
          val initial = A.nth partials childIdx
        in
          Stream.iteratePrefixesIncl g initial childStream
        end
    in
      Nest (n, getChild')
    end


  fun scan g b s =
    case s of
      Flat (Full slice) =>
        scanDelay g b (0, AS.length slice, AS.nth slice)
    | Flat (Delay (i, j, f)) =>
        scanDelay g b (i, j, f)
    | Nest (n, getChild) =>
        scanScan g b (n, getChild)


  fun scanIncl g b s =
    case s of
      Flat (Full slice) =>
        scanInclDelay g b (0, AS.length slice, AS.nth slice)
    | Flat (Delay (i, j, f)) =>
        scanInclDelay g b (i, j, f)
    | Nest (n, getChild) =>
        scanScanIncl g b (n, getChild)


  fun zipWithBothFlat g (i1, j1, f1) (i2, j2, f2) =
    let
      val n1 = j1-i1
      val n2 = j2-i2
      val n = Int.min (n1, n2)
    in
      Flat (Delay (0, n, fn i => g (f1 (i1+i), f2 (i2+i))))
    end


  fun zipWithOneNest g (n1, getChild1) (i, j, f) =
    let
      val n2 = j-i
      val _ =
        if n1 = n2 then () else
        raise Fail "OldDelayedSeq.zipWith lengths don't match"

      fun getChild childIdx =
        let
          val child1 = getChild1 childIdx
          val lo = i + childIdx * blockSize
        in
          Stream.mapIdx (fn (k, x) => g (x, f (lo+k))) child1
        end
    in
      Nest (n1, getChild)
    end


  fun zipWithBothNest g (n1, getChild1) (n2, getChild2) =
    let
      val _ =
        if n1 = n2 then () else
        raise Fail "OldDelayedSeq.zipWith lengths don't match"

      fun getChild childIdx =
        Stream.zipWith g (getChild1 childIdx, getChild2 childIdx)
    in
      Nest (n1, getChild)
    end

  fun flip g (a, b) = g (b, a)

  fun zipWith g (s1, s2) =
    case (s1, s2) of
      (Flat (Delay xx), Flat (Delay yy)) =>
        zipWithBothFlat g xx yy
    | (Flat (Full slice), Flat (Delay yy)) =>
        zipWithBothFlat g (0, AS.length slice, AS.nth slice) yy
    | (Flat (Delay xx), Flat (Full slice)) =>
        zipWithBothFlat g xx (0, AS.length slice, AS.nth slice)
    | (Flat (Full slice1), Flat (Full slice2)) =>
        zipWithBothFlat g
          (0, AS.length slice1, AS.nth slice1)
          (0, AS.length slice2, AS.nth slice2)
    | (Nest xx, Flat (Full slice)) =>
        zipWithOneNest g xx (0, AS.length slice, AS.nth slice)
    | (Nest xx, Flat (Delay yy)) =>
        zipWithOneNest g xx yy
    | (Flat (Full slice), Nest xx) =>
        zipWithOneNest (flip g) xx (0, AS.length slice, AS.nth slice)
    | (Flat (Delay yy), Nest xx) =>
        zipWithOneNest (flip g) xx yy
    | (Nest xx, Nest yy) =>
        zipWithBothNest g xx yy


  fun zip (a, b) = zipWith (fn x => x) (a, b)

  (* ===================================================================== *)

  exception NYI
  exception Range
  exception Size

  datatype 'a listview = NIL | CONS of 'a * 'a seq
  datatype 'a treeview = EMPTY | ONE of 'a | PAIR of 'a seq * 'a seq

  type 'a ord = 'a * 'a -> order
  type 'a t = 'a seq

  fun argmax x = raise NYI
  fun collate x = raise NYI
  fun collect x = raise NYI
  fun drop x = raise NYI
  fun equal x = raise NYI
  fun iteratePrefixes x = raise NYI
  fun iteratePrefixesIncl x = raise NYI
  fun merge x = raise NYI
  fun sort x = raise NYI
  fun splitHead x = raise NYI
  fun splitMid x = raise NYI
  fun take x = raise NYI
  fun update x = raise NYI
  fun zipWith3 x = raise NYI

  fun filterSome x = raise NYI
  fun foreach x = raise NYI
  fun foreachG x = raise NYI

end

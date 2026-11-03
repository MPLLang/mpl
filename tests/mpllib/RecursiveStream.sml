structure RecursiveStream :> STREAM =
struct

  (** Intended use: when the stream is consumed, the indices are provided
    * as input.
    *
    * For example, if `stream` represents elements x0,x1,...
    *   val S a = stream
    *   val (x0, S b) = a 0
    *   val (x1, S c) = b 1
    *   val (x2, S d) = c 2
    *   ...
    *
    * Requiring the index in this way is just an optimization: we know that all
    * streams require at least an index as state, so we can save storing one
    * piece of state by instead providing this only as needed.
    *)
  datatype 'a stream = S of int -> 'a * ('a stream)
  type 'a t = 'a stream

  fun nth stream i =
    let
      fun loop j (S g) =
        let
          val (first, rest) = g j
        in
          if j >= i then first else loop (j+1) rest
        end
    in
      loop 0 stream
    end

  fun tabulate f = S (fn i => (f i, tabulate f))

  fun map f (S g) =
    S (fn i =>
      let
        val (first, rest) = g i
      in
        (f first, map f rest)
      end)

  fun mapIdx f (S g) =
    S (fn i =>
      let
        val (first, rest) = g i
      in
        (f (i, first), mapIdx f rest)
      end)

  fun zipWith f (S g, S h) =
    S (fn i =>
      let
        val (gfirst, grest) = g i
        val (hfirst, hrest) = h i
      in
        (f (gfirst, hfirst), zipWith f (grest, hrest))
      end)

  fun iteratePrefixes f z (S g) =
    S (fn i =>
      let
        val (first, rest) = g i
        val z' = f (z, first)
      in
        (z, iteratePrefixes f z' rest)
      end)

  fun iteratePrefixesIncl f z (S g) =
    S (fn i =>
      let
        val (first, rest) = g i
        val z' = f (z, first)
      in
        (z', iteratePrefixesIncl f z' rest)
      end)

  fun applyIdx (n, stream) f =
    let
      fun loop i (S g) =
        if i >= n then () else
        let
          val (first, rest) = g i
        in
          f (i, first);
          loop (i+1) rest
        end
    in
      loop 0 stream
    end

  fun iterate f z (n, stream) =
    let
      fun loop acc i (S g) =
        if i >= n then acc else
        let
          val (first, rest) = g i
          val acc' = f (acc, first)
        in
          loop acc' (i+1) rest
        end
    in
      loop z 0 stream
    end

  fun resize arr =
    let
      val newCapacity = 2 * Array.length arr
      val dst = ForkJoin.alloc newCapacity
    in
      Array.copy {src = arr, dst = dst, di = 0};
      dst
    end

  fun pack f (length, stream) =
    let
      fun loop (data, next) i (S g) =
        if i < length andalso next < Array.length data then
          let
            val (first, rest) = g i
          in
            case f first of
              SOME y =>
                ( Array.update (data, next, y)
                ; loop (data, next+1) (i+1) rest
                )
            | NONE =>
                loop (data, next) (i+1) rest
          end

        else if next >= Array.length data then
          loop (resize data, next) i (S g)

        else
          (data, next)

      val (data, count) = loop (ForkJoin.alloc 10, 0) 0 stream
    in
      ArraySlice.slice (data, 0, SOME count)
    end


  fun makeBlockStreams
        { blockSize: int
        , numChildren: int
        , offset: int -> int
        , getElem: int -> int -> 'a
        } =
    let
      fun getBlock blockIdx =
        let
          fun advanceUntilNonEmpty i =
            if i >= numChildren orelse offset i <> offset (i+1) then
              i
            else
              advanceUntilNonEmpty (i+1)

          val lo = blockIdx * blockSize

          fun walk i =
            S (fn idx =>
              let
                val j = lo + idx - offset i
                val elem = getElem i j
              in
                if offset i + j + 1 < offset (i+1) then
                  (elem, walk i)
                else
                  (elem, walk (advanceUntilNonEmpty (i+1)))
              end)

          val firstOuterIdx =
            OffsetSearch.indexSearch (0, numChildren, offset) lo
        in
          walk firstOuterIdx
        end
    in
      getBlock
    end

end

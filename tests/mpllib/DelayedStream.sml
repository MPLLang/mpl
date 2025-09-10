structure DelayedStream :> STREAM =
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


  fun nth stream i =
    let
      val trickle = stream ()

      fun loop j =
        let
          val x = trickle j
        in
          if j = i then x else loop (j+1)
        end
    in
      loop 0
    end


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


  fun resize arr =
    let
      val newCapacity = 2 * Array.length arr
      val dst = ForkJoin.alloc newCapacity
    in
      Array.copy {src = arr, dst = dst, di = 0};
      dst
    end


  (** simple but less efficient: accumulate in list *)
  (*fun pack f (length, stream) =
    let
      val trickle = stream ()

      fun loop (data, count) i =
        if i < length then
          case f (trickle i) of
            SOME y =>
              loop (y :: data, count+1) (i+1)
          | NONE =>
              loop (data, count) (i+1)
        else
          (data, count)

      val (data, count) = loop ([], 0) 0
    in
      ArraySlice.full (Array.fromList (List.rev data))
      (* ArraySlice.slice (data, 0, SOME count) *)
    end*)


  (** more efficient: accumulate in dynamic resizing array *)
  fun pack f (length, stream) =
    let
      val trickle = stream ()

      fun loop (data, next) i =
        if i < length andalso next < Array.length data then
          case f (trickle i) of
            SOME y =>
              ( Array.update (data, next, y)
              ; loop (data, next+1) (i+1)
              )
          | NONE =>
              loop (data, next) (i+1)

        else if next >= Array.length data then
          loop (resize data, next) i

        else
          (data, next)

      val (data, count) = loop (ForkJoin.alloc 10, 0) 0
    in
      ArraySlice.slice (data, 0, SOME count)
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
        in
          fn () =>
            let
              val lo = blockIdx * blockSize
              val firstOuterIdx =
                OffsetSearch.indexSearch (0, numChildren, offset) lo
              val outerIdx = ref firstOuterIdx
            in
              fn idx =>
                (let
                  val i = !outerIdx
                  val j = lo + idx - offset i
                  (* val j = !innerIdx *)
                  val elem = getElem i j
                in
                  if offset i + j + 1 < offset (i+1) then
                    ()
                  else
                    outerIdx := advanceUntilNonEmpty (i+1);
                  elem
                end)
            end
        end

    in
      getBlock
    end


end

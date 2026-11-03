(** This is the algorithm I describe in my blog post. Asymptotically efficient
  * and simple to understand, but not very well optimized.
  *)
structure SimpleExpandFlatten =
struct

  fun flatten s =
    let
      val (offsets, total) = Seq.scan op+ 0 (Seq.map Seq.length s)

      fun expand (prevBoundaries, prevBlockSize) =
        let
          val newBlockSize = prevBlockSize div 2

          fun newBoundary b =
            let
              val blockNum = (b * newBlockSize) div prevBlockSize
              val offlo = #2 (Seq.nth prevBoundaries blockNum) - 1
              val offhi =
                if blockNum+1 < Seq.length prevBoundaries then
                  #1 (Seq.nth prevBoundaries (blockNum+1))
                else
                  Seq.length offsets

              val (nlt, nleq) =
                BinarySearch.numLtAndLeq
                  (Seq.subseq offsets (offlo, offhi-offlo))
                  (b * newBlockSize)
            in
              (offlo + nlt, offlo + nleq)
            end

          val newNumBlocks = Util.ceilDiv total newBlockSize
        in
          (Seq.tabulate newBoundary newNumBlocks, newBlockSize)
        end

      fun expansionLoop (boundaries, blockSize) =
        if blockSize = 1 then
          boundaries
        else
          expansionLoop (expand (boundaries, blockSize))

      val boundaries =
        expansionLoop (Seq.fromList [BinarySearch.numLtAndLeq offsets 0], total)

      fun getElem i =
        let
          val segIdx = #2 (Seq.nth boundaries i) - 1
          val segOff = Seq.nth offsets segIdx
        in
          Seq.nth (Seq.nth s segIdx) (i - segOff)
        end
    in
      Seq.tabulate getElem total
    end

end

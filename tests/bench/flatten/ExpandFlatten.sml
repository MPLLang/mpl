structure ExpandFlatten =
struct

  val expansionFactor = CommandLineArgs.parseInt "f" 8
  val targetOffsetsPerElem = CommandLineArgs.parseInt "off-per-elem" 8
  val targetBlockSize = CommandLineArgs.parseInt "min-block-size" 64

  fun reportTime msg f =
    let
      val (result, tm) = Util.getTime f
    in
      print (msg ^ " " ^ Time.fmt 4 tm ^ "\n");
      result
    end

  fun seqNumLeq (xs: int Seq.t) x =
    let
      val n = Seq.length xs
      fun loop i =
        if i < n andalso Seq.nth xs i <= x then
          loop (i+1)
        else
          i
    in
      loop 0
    end


  fun tabulateG grain f n =
    ArraySlice.full (SeqBasis.tabulate grain (0, n) f)


  fun flatten s =
    let
      val (offsets, total) = Seq.scan op+ 0 (Seq.map Seq.length s)

      fun expand (prevBoundaries, prevBlockSize) =
        let
          val newBlockSize = prevBlockSize div expansionFactor

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
          val bs =
            reportTime "boundaries" (fn _ =>
              Seq.tabulate newBoundary newNumBlocks)
        in
          (bs, newBlockSize)
        end

      fun expansionLoop (boundaries, blockSize) =
        if
          Seq.length boundaries >= Seq.length offsets div targetOffsetsPerElem
          orelse blockSize <= targetBlockSize
        then
          (boundaries, blockSize)
        else
          expansionLoop (expand (boundaries, blockSize))

      (** Initial block is the whole array, but we need to compute the proper
        * boundary for the start of that block. Then we expand until we
        * hit the target.
        *)
      val init = (Seq.fromList [BinarySearch.numLtAndLeq offsets 0], total)
      val (boundaries, blockSize) = expansionLoop init

      (** Finally, pull individual elements *)
      fun getElem i =
        let
          val blockNum = i div blockSize
          val offlo = #2 (Seq.nth boundaries blockNum) - 1
          val segIdx =
            offlo - 1
            + (seqNumLeq (Seq.drop offsets offlo) i)
          val segOff = Seq.nth offsets segIdx
        in
          Seq.nth (Seq.nth s segIdx) (i - segOff)
        end

      val result = reportTime "tabulate" (fn _ =>
        Seq.tabulate getElem total)
    in
      result
    end

end

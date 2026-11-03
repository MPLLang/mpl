structure MultiBlockedBSFlatten =
struct

  val blockSizesStr = CommandLineArgs.parseString "bs" "1000,50"
  val doReport = CommandLineArgs.parseFlag "report-times"

  val blockSizes =
    List.map (valOf o Int.fromString)
    (String.tokens (fn c => c = #",") blockSizesStr)
    handle _ => raise Fail ("error parsing block sizes '" ^ blockSizesStr ^ "'")

  fun reportTime msg f =
    let
      val (result, tm) = Util.getTime f
    in
      if not doReport then () else print (msg ^ " " ^ Time.fmt 4 tm ^ "\n");
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

      fun expand (prevBoundaries, prevBlockSize) newBlockSize =
        let
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

      (** compute initial boundary *)
      val blockSize = List.hd blockSizes
      val numBlocks1 = Util.ceilDiv total blockSize
      fun boundary1 b = BinarySearch.numLtAndLeq offsets (b * blockSize)
      val boundaries =
        reportTime "boundaries" (fn _ =>
          Seq.tabulate boundary1 numBlocks1)

      fun expansionLoop (boundaries, blockSize) bs =
        case bs of
          [] => (boundaries, blockSize)
        | b :: bs' =>
            expansionLoop (expand (boundaries, blockSize) b) bs'

      (** expand boundaries a few times *)
      val (boundaries, blockSize) =
        expansionLoop (boundaries, blockSize) (List.tl blockSizes)

      (** pull individual elements *)
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

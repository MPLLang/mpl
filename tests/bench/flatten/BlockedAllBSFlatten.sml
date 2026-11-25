structure BlockedAllBSFlatten =
struct

  val blockSize = CommandLineArgs.parseInt "block-size" 16

  fun flatten s =
    let
      val (offsets, total) = Seq.scan op+ 0 (Seq.map Seq.length s)

      (* val _ = print ("offsets " ^ Seq.toString Int.toString offsets ^ "\n") *)

      val numBlocks = Util.ceilDiv total blockSize

      (* fun blockOffsetLo b =
        BinarySearch.numLeq offsets (b * blockSize) - 1 *)
      (* fun blockOffsetHi b =
        BinarySearch.numLt offsets ((b+1) * blockSize) *)

      (* val (blockOffsetLos, tm1) = Util.getTime (fn _ =>
        Seq.tabulate blockOffsetLo numBlocks
      ) *)
      (* val (blockOffsetHis, tm2) = Util.getTime (fn _ =>
        Seq.tabulate blockOffsetHi numBlocks
      ) *)

      (* val _ = print ("offlos   " ^ Time.fmt 4 tm1 ^ "\n")
      val _ = print ("offhis   " ^ Time.fmt 4 tm2 ^ "\n") *)

      fun boundary b =
        BinarySearch.numLtAndLeq offsets (b * blockSize)

      val (boundaries, tm) = Util.getTime (fn _ =>
        Seq.tabulate boundary (numBlocks+1)
      )
      val _ = print ("boundaries " ^ Time.fmt 4 tm ^ "\n")

      fun getElem i =
        let
          val blockNum = i div blockSize
          (* val offlo = Seq.nth blockOffsetLos blockNum
          val offhi = Seq.nth blockOffsetHis blockNum *)
          val offlo = #2 (Seq.nth boundaries blockNum) - 1
          val offhi = #1 (Seq.nth boundaries (blockNum+1))
          val segIdx =
            offlo + (BinarySearch.numLeq (Seq.subseq offsets (offlo, offhi-offlo))) i - 1
          (* val _ =
            print ("getElem " ^ Int.toString i ^
              " blockNum " ^ Int.toString blockNum ^
              " offlo " ^ Int.toString offlo ^
              " offhi " ^ Int.toString offhi ^
              " segIdx " ^ Int.toString segIdx ^
              "\n") *)
          val segOff = Seq.nth offsets segIdx
        in
          Seq.nth (Seq.nth s segIdx) (i - segOff)
        end
        (* handle Subscript =>
          ( print ("getElem " ^ Int.toString i ^ "\n")
          ; raise Subscript
          ) *)

      val (result, tm3) = Util.getTime (fn _ =>
        Seq.tabulate getElem total
      )

      val _ = print ("tabulate " ^ Time.fmt 4 tm3 ^ "\n")
    in
      result
    end

end

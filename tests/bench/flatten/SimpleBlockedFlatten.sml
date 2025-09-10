structure SimpleBlockedFlatten =
struct

  val blockSize = CommandLineArgs.parseInt "block-size" 4096
  val doReport = CommandLineArgs.parseFlag "report-times"

  fun reportTime msg f =
    let
      val (result, tm) = Util.getTime f
    in
      if not doReport then () else print (msg ^ " " ^ Time.fmt 4 tm ^ "\n");
      result
    end

  fun tabulateG grain f n =
    ArraySlice.full (SeqBasis.tabulate grain (0, n) f)

  fun flatten s =
    let
      val (offsets, total) = reportTime "scan" (fn _ =>
        Seq.scan op+ 0 (Seq.map Seq.length s))

      val numBlocks = Util.ceilDiv total blockSize

      fun getBlock bidx =
        let
          val lo = bidx * blockSize
          val hi = (if bidx+1 = numBlocks then total else lo + blockSize)
          val size = hi - lo

          fun loop (count, elems) (segIdx, i) =
            if count < size andalso i < Seq.length (Seq.nth s segIdx) then
              loop (count+1, Seq.nth (Seq.nth s segIdx) i :: elems) (segIdx, i+1)
            else if count >= size then
              elems
            else
              loop (count, elems) (segIdx+1, 0)

          val segIdx = BinarySearch.numLeq offsets lo - 1
          val segOff = Seq.nth offsets segIdx
          val elems = loop (0, []) (segIdx, lo - segOff)
        in
          Vector.fromList elems
        end

      val blocks = reportTime "tab blocks" (fn _ =>
        tabulateG 1 getBlock numBlocks)

      fun getElem i =
        let
          val bidx = i div blockSize
          val blo = bidx * blockSize
          val block = Seq.nth blocks bidx
          val blen = Vector.length block
        in
          (** The vector is reversed, because built from list. *)
          Vector.sub (block, blen - 1 - (i - blo))
        end

      val result = reportTime "tabulate" (fn _ =>
        Seq.tabulate getElem total)
    in
      result
    end

end

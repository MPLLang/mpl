structure AllBSFlatten =
struct

  fun flatten s =
    let
      val (offsets, total) = Seq.scan op+ 0 (Seq.map Seq.length s)

      fun getElem i =
        let
          val segIdx = (BinarySearch.numLeq offsets i) - 1
          val segOff = Seq.nth offsets segIdx
        in
          Seq.nth (Seq.nth s segIdx) (i - segOff)
        end
    in
      Seq.tabulate getElem total
    end

end

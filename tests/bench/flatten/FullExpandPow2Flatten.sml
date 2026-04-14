structure FullExpandPow2Flatten =
struct

  fun biggestPow2LessOrEqualTo x =
    let
      fun loop y = if 2*y > x then y else loop (2*y)
    in
      loop 1
    end

  (** Choose segment indices for n elements based on the given segment offsets.
    *
    * EXAMPLE INPUT:
    *   offsets = [0,3,4,4,4,4,7,8]
    *   n = 10
    * OUTPUT:
    *   [ 0,0,0, 1, 5,5,5, 6, 7,7 ]
    *
    *)
  fun pickSegments (offsets, n) =
    let
      fun offmids half lo (offlo, offhi) =
        let
          val mid = lo + half
          val (offmidlo, offmidhi) =
            BinarySearch.numLtAndLeq (Seq.subseq offsets (offlo, offhi-offlo)) mid
        in
          (offlo + offmidlo, offlo + offmidhi - 1)
        end

      fun loop toWidth width results =
        if width = toWidth then
          results
        else
          let
            fun getOffmids i =
              offmids (width div 2) (i * width) (Seq.nth results i)

            val (O, tm) = Util.getTime (fn _ =>
              Seq.tabulate getOffmids (Seq.length results)
            )
            val _ =
              if width > 64 then ()
              else print ("offsets " ^ Int.toString width ^ ": " ^ Time.fmt 4 tm ^ "\n")

            fun get i =
              let
                val i' = i div 2
                val lo = i' * width
                val hi = lo + width
                val (offlo, offhi) = Seq.nth results i'
                val (offmidlo, offmidhi) = Seq.nth O i'
                val mid = lo + (hi - lo) div 2
              in
                if i mod 2 = 0 then
                  (offlo, offmidlo)  (* "left" *)
                else
                  (offmidhi, offhi)  (* "right" *)
              end
            val (results', tm) = Util.getTime (fn _ =>
              Seq.tabulate get (2 * Seq.length results)
            )
            val _ =
              if width > 64 then ()
              else print ("expand  " ^ Int.toString width ^ ": " ^ Time.fmt 4 tm ^ "\n")
          in
            loop toWidth (width div 2) results'
          end

      (** (width, results) represents current prefix that we've finished
        * processing: it's been decomposed into some number of subsequences
        * each of the given width.
        *
        * (offlo, lo) is the remaining suffix, where lo is the starting index
        * of the suffix and offlo is the segment index for lo.
        *)
      fun handleNonPow2Loop (width, results) (offlo, lo) =
        if lo >= n then
          loop 1 width results
        else
          let
            val remainingSize = n - lo
            val targetWidth = biggestPow2LessOrEqualTo remainingSize
            val results' = loop targetWidth width results

            val (offmidlo, offmidhi) = offmids targetWidth lo (offlo, Seq.length offsets)
            val new = (offlo, offmidlo)
            val results'' = Seq.append (results', Seq.fromList [new])
          in
            handleNonPow2Loop (targetWidth, results'') (offmidhi, lo+targetWidth)
          end

      val targetWidth = biggestPow2LessOrEqualTo n
      val offlo = (BinarySearch.numLeq offsets 0) - 1
      val (offmidlo, offmidhi) = offmids targetWidth 0 (0, Seq.length offsets)
      val init = Seq.fromList [(offlo, offmidlo)]
    in
      Seq.map #1 (handleNonPow2Loop (targetWidth, init) (offmidhi, targetWidth))
    end

  fun flatten s =
    let
      val n = Seq.length s
      val ((offsets, total), tm1) = Util.getTime (fn _ =>
        Seq.scan op+ 0 (Seq.map Seq.length s)
      )

      val (segIdxs, tm2) = Util.getTime (fn _ =>
        pickSegments (offsets, total)
      )

      fun getElem i =
        let
          val segIdx = Seq.nth segIdxs i
          val segOff = Seq.nth offsets segIdx
          val j = i - segOff
        in
          Seq.nth (Seq.nth s segIdx) j
        end

      val (result, tm3) = Util.getTime (fn _ =>
        Seq.tabulate getElem total
      )

      val _ = print ("scan:         " ^ Time.fmt 4 tm1 ^ "\n")
      val _ = print ("pickSegments: " ^ Time.fmt 4 tm2 ^ "\n")
      val _ = print ("tabulate:     " ^ Time.fmt 4 tm3 ^ "\n")
    in
      result
    end

end

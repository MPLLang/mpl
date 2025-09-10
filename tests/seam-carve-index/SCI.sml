structure SCI:
sig
  type image = PPM.image
  type seam = int Seq.t

  (* `makeSeamCarveIndex n img` removes `n` seams and returns
   * a mapping X that indicates the order in which pixels are removed.
   *
   * For a pixel at (i,j):
   *   - if not removed, then X[i*width + j] = -1
   *   - otherwise, removed in seam number X[i*width + j]
   *
   * So, for an image of height H, there will be H pixels that are marked 0
   * and H other pixels that are marked 1, etc.
   *)
  val makeSeamCarveIndex: int -> image -> int Seq.t
end =
struct

  type image = PPM.image
  type seam = int Seq.t

  val blockWidth = CommandLineArgs.parseInt "block-width" 80
  val _ = print ("block-width " ^ Int.toString blockWidth ^ "\n")
  val _ =
    if blockWidth mod 2 = 0 then ()
    else Util.die ("block-width must be even!")

  (* This is copied/adapted from ../seam-carve/SC.sml. See that file for
   * explanation of the algorithm. *)
  fun triangularBlockedWriteAllMinSeams width height energy minSeamEnergies =
    let
      fun M (i, j) =
        if j < 0 orelse j >= width then Real.posInf
        else Array.sub (minSeamEnergies, i*width + j)
      fun setM (i, j) =
        let
          val x =
            if i = 0 then 0.0
            else energy (i, j) +
                 Real.min (M (i-1, j), Real.min (M (i-1, j-1), M (i-1, j+1)))
        in
          Array.update (minSeamEnergies, i*width + j, x)
        end

      val blockHeight = blockWidth div 2
      val numBlocks = 1 + (width - 1) div blockWidth

      fun upperTriangle i jMid =
        Util.for (0, Int.min (height-i, blockHeight)) (fn k =>
          let
            val lo = Int.max (0, jMid-blockHeight+k)
            val hi = Int.min (width, jMid+blockHeight-k)
          in
            Util.for (lo, hi) (fn j => setM (i+k, j))
          end)

      fun lowerTriangle i jMid =
        Util.for (0, Int.min (height-i, blockHeight)) (fn k =>
          let
            val lo = Int.max (0, jMid-k-1)
            val hi = Int.min (width, jMid+k+1)
          in
            Util.for (lo, hi) (fn j => setM (i+k, j))
          end)

      fun setStripStartingAt i =
        ( ForkJoin.parfor 1 (0, numBlocks) (fn b =>
            upperTriangle i (b * blockWidth + blockHeight))
        ; ForkJoin.parfor 1 (0, numBlocks+1) (fn b =>
            lowerTriangle (i+1) (b * blockWidth))
        )

      fun loop i =
        if i >= height then () else
        ( setStripStartingAt i
        ; loop (i + blockHeight + 1)
        )
    in
      loop 0
    end

  (* ====================================================================== *)

  fun isolateMinSeam width height M =
    let
      fun idxMin2 ((j1, m1), (j2, m2)) =
        if m1 > m2 then (j2, m2) else (j1, m1)
      fun idxMin3 (a, b, c) = idxMin2 (a, idxMin2 (b, c))

      (* the index of the minimum seam in the last row *)
      val (jMin, _) =
        SeqBasis.reduce 1000
          idxMin2 (~1, Real.posInf) (0, width) (fn j => (j, M (height-1, j)))

      val seam = ForkJoin.alloc height

      fun computeSeamBackwards (i, j) =
        if i = 0 then
          Array.update (seam, 0, j)
        else
          let
            val (j', _) = idxMin3
              ( (j,   M (i-1, j  ))
              , (j-1, M (i-1, j-1))
              , (j+1, M (i-1, j+1))
              )
          in
            Array.update (seam, i, j);
            computeSeamBackwards (i-1, j')
          end
    in
      computeSeamBackwards (height-1, jMin);
      ArraySlice.full seam
    end

  (* ====================================================================== *)

  structure VSIM = VerticalSeamIndexMap

  fun makeSeamCarveIndex numSeamsToRemove image =
    let
      val N = #width image * #height image

      (* This buffer will be reused throughout *)
      val minSeamEnergies = ForkJoin.alloc N

      fun pixel idx (i, j) = PPM.elem image (VSIM.remap idx (i, j))

      (* ===========================================
       * computing the energy of all pixels
       * (gradient values)
       *)

      fun d p1 p2 = Color.distance (p1, p2)

      fun energy idx (i, j) =
        let
          val (h, w) = VSIM.domain idx
        in
          if j = w-1 then Real.posInf
          else if i = h - 1 then 0.0
          else let
            val p = pixel idx (i, j)
            val dx = d p (pixel idx (i, j+1))
            val dy = d p (pixel idx (i+1, j))
          in
            Math.sqrt (dx + dy)
          end
        end

      (* ============================================
       * loop to remove seams
       *)

      val X = ForkJoin.alloc N
      val _ = ForkJoin.parfor 4000 (0, N) (fn i => Array.update (X, i, ~1))
      fun setX (i, j) x = Array.update (X, i*(#width image) + j, x)

      val idx = VSIM.new (#height image, #width image)

      fun loop numSeamsRemoved =
        if numSeamsRemoved >= numSeamsToRemove then () else
        let
          val currentWidth = #width image - numSeamsRemoved
          val _ = triangularBlockedWriteAllMinSeams
            currentWidth
            (#height image)
            (energy idx)
            minSeamEnergies (* results written here *)

          fun M (i, j) =
            if j < 0 orelse j >= currentWidth then Real.posInf
            else Array.sub (minSeamEnergies, i*currentWidth + j)

          val seam = isolateMinSeam currentWidth (#height image) M
        in
          Seq.foreach seam (fn (i, j) =>
            setX (VSIM.remap idx (i, j)) numSeamsRemoved);

          VSIM.carve idx seam;

          loop (numSeamsRemoved+1)
        end

    in
      loop 0;

      ArraySlice.full X
    end

end

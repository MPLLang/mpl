structure SC:
sig
  type image = PPM.image
  type seam = int Seq.t

  val minSeam: image -> seam
  val paintSeam: image -> seam -> PPM.pixel -> image

  val carve: image -> seam -> image

  val removeSeams: int -> image -> image
end =
struct

  structure AS = ArraySlice

  type image = PPM.image
  type seam = int Seq.t

  (* ======================================================================
   * seam finding algorithms to solve the equation
   *   M(i,j) = E(i,j) + min(M(i-1,j), M(i-1,j-1), M(i-1,j+1))
   *)

  (* row-wise bottom-up DP goes by increasing i *)
  fun rowWiseAllMinSeams width height (energy: int -> int -> real): real Seq.t =
    let
      val minSeamEnergies = ForkJoin.alloc (width * height)
      fun M i j =
        if j < 0 orelse j >= width then Real.posInf
        else Array.sub (minSeamEnergies, i*width + j)
      fun setM i j =
        let
          val x =
            if i = 0 then 0.0
            else energy i j +
                 Real.min (M (i-1) j, Real.min (M (i-1) (j-1), M (i-1) (j+1)))
        in
          Array.update (minSeamEnergies, i*width + j, x)
        end

      fun computeMinSeamEnergies i =
        if i >= height then ()
        else ( ForkJoin.parfor 1000 (0, width) (setM i)
             ; computeMinSeamEnergies (i+1)
             )

      val _ = computeMinSeamEnergies 0
    in
      AS.full minSeamEnergies
    end

  (* I tuned this a little bit. For an image approximately 1000 pixels
   * wide, this only gives us about 12x possible speedup. But any smaller
   * and the grains are too small! *)
  val blockWidth = CommandLineArgs.parseInt "block-width" 80
  val _ = print ("block-width " ^ Int.toString blockWidth ^ "\n")
  val _ =
    if blockWidth mod 2 = 0 then ()
    else Util.die ("block-width must be even!")

  (* Triangular-blocked bottom-up DP does fancy triangular strategy, to
   * improve granularity.
   *
   * Imaging breaking up the image into a bunch of strips, where each strip
   * is then divided into triangles. If each triangle is processed sequentially
   * row-wise from top to bottom, then we can compute a strip in parallel
   * by first doing all of upper triangles (#), and then doing all of the lower
   * triangles (.):
   *
   *             +------------------------------------+
   *  strip 1 -> |\####/\####/\####/\####/\####/\####/|
   *             |.\##/..\##/..\##/..\##/..\##/..\##/.|
   *             |..\/....\/....\/....\/....\/....\/..|
   *  strip 2 -> |\####/\####/\####/\####/\####/\####/|
   *             |.\##/..\##/..\##/..\##/..\##/..\##/.|
   *             |..\/....\/....\/....\/....\/....\/..|
   *  strip 3 -> |                                    |
   *)
  fun triangularBlockedAllMinSeams width height energy =
    let
      val minSeamEnergies = ForkJoin.alloc (width * height)
      fun M i j =
        if j < 0 orelse j >= width then Real.posInf
        else Array.sub (minSeamEnergies, i*width + j)
      fun setM i j =
        let
          val x =
            if i = 0 then 0.0
            else energy i j +
                 Real.min (M (i-1) j, Real.min (M (i-1) (j-1), M (i-1) (j+1)))
        in
          Array.update (minSeamEnergies, i*width + j, x)
        end

      val blockHeight = blockWidth div 2
      val numBlocks = 1 + (width - 1) div blockWidth

      (* Fill in a triangle starting at row i, centered at jMid, with
       * the fat end at top and small end at bottom.
       *
       * For example with blockWidth 6:
       *               jMid
       *                |
       *      i -- X X X X X X
       *             X X X X
       *               X X
       *)
      fun upperTriangle i jMid =
        Util.for (0, Int.min (height-i, blockHeight)) (fn k =>
          let
            val lo = Int.max (0, jMid-blockHeight+k)
            val hi = Int.min (width, jMid+blockHeight-k)
          in
            Util.for (lo, hi) (fn j => setM (i+k) j)
          end)

      (* The other way around. For example with blockWidth 6:
       *               jMid
       *                |
       *      i --     X X
       *             X X X X
       *           X X X X X X
       *)
      fun lowerTriangle i jMid =
        Util.for (0, Int.min (height-i, blockHeight)) (fn k =>
          let
            val lo = Int.max (0, jMid-k-1)
            val hi = Int.min (width, jMid+k+1)
          in
            Util.for (lo, hi) (fn j => setM (i+k) j)
          end)

      (* This sets rows [i, i + blockHeight].
       * Note that this includes the row i+blockHeight; i.e. the number of
       * rows set is blockHeight+1
       *)
      fun setStripStartingAt i =
        ( ForkJoin.parfor 1 (0, numBlocks) (fn b =>
            upperTriangle i (b * blockWidth + blockHeight))
        ; ForkJoin.parfor 1 (0, numBlocks+1) (fn b =>
            lowerTriangle (i+1) (b * blockWidth))
        )

      fun computeMinSeamEnergies i =
        if i >= height then () else
        ( setStripStartingAt i
        ; computeMinSeamEnergies (i + blockHeight + 1)
        )

      val _ = computeMinSeamEnergies 0
    in
      AS.full minSeamEnergies
    end

  (* ======================================================================
   * find the min seam
   * can choose for the allMinSeams algorithm:
   *   1. rowWiseAllMinSeams
   *   2. triangularBlockedAllMinSeams
   *)

  fun minSeam' allMinSeams image =
    let
      val height = #height image
      val width = #width image
      fun pixel i j = PPM.elem image (i, j)

      (* ===========================================
       * compute the energy of all pixels
       * (gradient values)
       *)

      fun c x = Real.fromInt (Word8.toInt x) / 255.0
      fun sq (x: real) = x * x
      fun d {red=r1, green=g1, blue=b1} {red=r2, green=g2, blue=b2} =
        sq (c r2 - c r1) + sq (c g2 - c g1) + sq (c b2 - c b1)

      fun computeEnergy i j =
        if j = width-1 then Real.posInf
        else if i = height - 1 then 0.0
        else let
          val dx = d (pixel i j) (pixel i (j+1))
          val dy = d (pixel i j) (pixel (i+1) j)
        in
          Math.sqrt (dx + dy)
        end

      val energies =
        AS.full (SeqBasis.tabulate 4000 (0, width*height) (fn k =>
          computeEnergy (k div width) (k mod width)))
      fun energy i j =
        Seq.nth energies (i*width + j)

      (* ===========================================
       * compute the min seam energies
       *)

      val MM = allMinSeams width height energy
      fun M i j =
        if j < 0 orelse j >= width then Real.posInf
        else Seq.nth MM (i*width + j)

      (* ===========================================
       * isolate the minimum seam
       *)

      fun idxMin2 ((j1, m1), (j2, m2)) =
        if m1 > m2 then (j2, m2) else (j1, m1)
      fun idxMin3 (a, b, c) = idxMin2 (a, idxMin2 (b, c))

      (* the index of the minimum seam in the last row *)
      val (jMin, _) =
        SeqBasis.reduce 1000
          idxMin2 (~1, Real.posInf) (0, width) (fn j => (j, M (height-1) j))

      fun computeSeamBackwards seam (i, j) =
        if i = 0 then j::seam else
        let
          val (j', _) = idxMin3
            ( (j,   M (i-1) j    )
            , (j-1, M (i-1) (j-1))
            , (j+1, M (i-1) (j+1))
            )
        in
          computeSeamBackwards (j::seam) (i-1, j')
        end
    in
      Seq.fromList (computeSeamBackwards [] (height-1, jMin))
    end

  (* val minSeam = minSeam' rowWiseAllMinSeams *)
  val minSeam = minSeam' triangularBlockedAllMinSeams

  (* ======================================================================
   * utilities: carving, painting seams, etc.
   *)

  fun carve image seam =
    let
      val height = #height image
      val width = #width image
      fun pixel i j = PPM.elem image (i, j)

      fun newElem k =
        let
          val i = k div (width-1)
          val j = k mod (width-1)
          val r = Seq.nth seam i
        in
          if j < r then pixel i j else pixel i (j+1)
        end
    in
      { width = width-1
      , height = height
      , data = AS.full (SeqBasis.tabulate 4000 (0, (width-1)*height) newElem)
      }
    end

  fun paintSeam image seam seamColor =
    let
      val height = #height image
      val width = #width image
      fun pixel i j = PPM.elem image (i, j)

      fun newElem k =
        let
          val i = k div width
          val j = k mod width
          val r = Seq.nth seam i
        in
          if j = r then seamColor else pixel i j
        end
    in
      { width = width
      , height = height
      , data = Seq.tabulate newElem (height * width)
      }
    end

  fun removeSeams n image =
    if n = 0 then
      image
    else
      removeSeams (n-1) (carve image (minSeam image))

end

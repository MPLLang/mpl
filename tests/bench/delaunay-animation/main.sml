structure CLA = CommandLineArgs
structure T = Topology2D
structure DT = DelaunayTriangulation

val n = CLA.parseInt "n" (1000 * 1000)
val seed = CLA.parseInt "seed" 15210
val filename = CLA.parseString "input" ""

fun generateInputPoints () =
  let
    fun genReal i =
      let
        val x = Word64.fromInt (seed + i)
      in
        Real.fromInt (Word64.toInt (Word64.mod (Util.hash64 x, 0w1000000)))
        / 1000000.0
      end

    fun genPoint i = (genReal (2*i), genReal (2*i + 1))

    val (points, tm) = Util.getTime (fn _ => Seq.tabulate genPoint n)
    val _ = print ("generated input in " ^ Time.fmt 4 tm ^ "s\n")
  in
    points
  end

fun parseInputFile () =
  let
    val (points, tm) = Util.getTime (fn _ =>
      ParseFile.readSequencePoint2d filename)
  in
    print ("parsed input points in " ^ Time.fmt 4 tm ^ "s\n");
    points
  end


val input =
  case filename of
    "" => generateInputPoints ()
  | _ => parseInputFile ()


val (steps, mesh) = Benchmark.run "delaunay" (fn _ => DT.triangulate input)
val _ = print ("num rounds " ^ Int.toString (Seq.length steps) ^ "\n")

(* val _ =
  print ("\n" ^ T.toString mesh ^ "\n") *)


(* ==========================================================================
 * output result image
 * only works if all input points are in range [0,1)
 *)

val filename = CLA.parseString "output" ""
val _ =
  if filename <> "" then ()
  else ( print ("\nto see output, use -output, -resolution, and -fps arguments\n" ^
                "for example: delaunay -n 1000 -output result.gif -resolution 1000 -fps 10.0\n")
       ; OS.Process.exit OS.Process.success
       )

val t0 = Time.now ()

val resolution = CLA.parseInt "resolution" 1000
val fadeEdgeMargin = CLA.parseInt "fade-edge" 0
val borderEdgeMargin = CLA.parseInt "border-edge" 0
val fps = CLA.parseReal "fps" 10.0

val niceBackground: Color.color =
  let
    val c = Real.fromInt 0xFD / 255.0
  in
    {red = c, green = c, blue = c, alpha = 1.0}
  end
val niceBackgroundPx: Color.pixel = Color.colorToPixel niceBackground

(* val bg = #fdfdfd *)

(* val image = MeshToImage.toImage {mesh = mesh, resolution = resolution} *)

(* fun makeRed b =
  let
    val c = Word8.fromInt (Real.ceil (255.0 * (1.0 - b)))
    val color = {blue = c, green = c, red = 0w255}
  in
    color
  end

fun makeGray b =
  Color.colorToPixel ({red = 0.5, blue = 0.5, green = 0.5, alpha = b}) *)

(* val niceGray = Color.hsv {h=0.0, s=0.0, v=0.88} *)
(* val niceRed = Color.colorToPixel
  (Color.hsva {h = 0.0, s = 0.55, v = 0.95, a = 0.8}) *)
(* val colors = [Color.white, Color.black, Color.red]
val palette =
  GIF.Palette.summarizeBySampling colors 103
    (fn i =>
      if i < 50 then
        (* 50 shades of gray *)
        makeGray (Real.fromInt (1 + i mod 50) / 50.0)
      else
        (* ... and 50 shades of red *)
        makeRed (Real.fromInt (1 + i mod 50) / 50.0)) *)

fun fadeEdges margin (img as {width, height, data}) =
  let
    fun clampedDistFromZero x =
      Real.fromInt (margin - (Int.max (0, margin - x))) / Real.fromInt margin
    fun hbrightness col =
      Real.min (clampedDistFromZero col, clampedDistFromZero (width - col - 1))
    fun vbrightness row =
      Real.min (clampedDistFromZero row, clampedDistFromZero (height - row - 1))
    fun brightness (row, col) =
      Real.max (0.0, Real.min (hbrightness col, vbrightness row))
    fun update (row, col) =
      if 0 <= row andalso row < height andalso 0 <= col andalso col < width then
      let
        val px = Seq.nth data (row*width + col)
        val {red, green, blue, ...} = Color.pixelToColor px
        val alpha = brightness (row, col)
        val px' = Color.colorToPixel (Color.overlayColor
          { fg = {red=red, green=green, blue=blue, alpha=alpha}
          , bg = niceBackground
          })
      in
        ArraySlice.update (data, row*width + col, px')
      end
      else ()

    fun updateBox {topleft=(row0, col0), botright=(row1, col1)} =
      Util.for (row0, row1) (fn row =>
      Util.for (col0, col1) (fn col =>
        update (row, col)
      ))
  in
    updateBox {topleft = (0, 0), botright = (margin, width)};
    updateBox {topleft = (margin, 0), botright = (height-margin, margin)};
    updateBox {topleft = (margin, width-margin), botright = (height-margin, width)};
    updateBox {topleft = (height-margin, 0), botright = (height, width)};

    img
  end

fun drawBorder margin (img as {width, height, data}) =
  let
    fun update (row, col) =
      if 0 <= row andalso row < height andalso 0 <= col andalso col < width then
      let
      in
        ArraySlice.update (data, row*width + col, Color.black)
      end
      else ()

    fun updateBox {topleft=(row0, col0), botright=(row1, col1)} =
      Util.for (row0, row1) (fn row =>
      Util.for (col0, col1) (fn col =>
        update (row, col)
      ))
  in
    updateBox {topleft = (0, 0), botright = (margin, width)};
    updateBox {topleft = (margin, 0), botright = (height-margin, margin)};
    updateBox {topleft = (margin, width-margin), botright = (height-margin, width)};
    updateBox {topleft = (height-margin, 0), botright = (height, width)};

    img
  end

(* val numImages = 2 * (Seq.length steps) + 1 *)
(* val numImages = 3 *)
val numImages = Seq.length steps + 1
val images = SeqBasis.tabulate 1 (0, numImages) (fn i =>
  let
    val j = i (*div 2*)
    val mesh =
      if j < Seq.length steps then #mesh (Seq.nth steps j) else mesh
    val cavs =
      if (*i mod 2 = 1 andalso*) j < Seq.length steps then
        SOME (#updates (Seq.nth steps j))
      else
        NONE
    val img =
      MeshToImage.toImage {mesh = mesh, resolution = resolution, cavities = cavs, background = niceBackground}
  in
    if fadeEdgeMargin > 0 then
      fadeEdges fadeEdgeMargin img
    else if borderEdgeMargin > 0 then
      drawBorder borderEdgeMargin img
    else
      img
  end)

val t1 = Time.now ()

val _ = print ("generated frames in " ^ Time.fmt 4 (Time.- (t1, t0)) ^ "s\n")

val _ = print ("writing to " ^ filename ^"...\n")

val palette =
  GIF.Palette.summarizeBySampling [niceBackgroundPx, Color.black, Color.red] 256
    (fn i =>
      let
        val j1 = Util.hash (2*i) mod (Array.length images)
        val img = Array.sub (images, j1)
        val j2 = Util.hash (2*i + 1) mod (Seq.length (#data img))
      in
        Seq.nth (#data img) j2
      end)

val msBetween = Real.round ((1.0 / fps) * 100.0)
val (_, tm) = Util.getTime (fn _ =>
  GIF.writeMany filename msBetween palette
    { width = resolution
    , height = resolution
    , numImages = Array.length images
    , getImage = fn i => #remap palette (Array.sub (images, i))
    })
val _ = print ("wrote all frames in " ^ Time.fmt 4 tm ^ "s\n")

(* val (_, tm) = Util.getTime (fn _ => PPM.write "first.ppm" (Array.sub (images, 1)))
val _ = print ("wrote to " ^ "first.ppm" ^ " in " ^ Time.fmt 4 tm ^ "s\n") *)

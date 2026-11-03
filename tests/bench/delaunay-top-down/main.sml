structure CLA = CommandLineArgs
structure T = Topology2D
structure R = Real32
structure I = Int32
structure DT = DelaunayTriangulationTopDown (structure R = R structure I = I)

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

    fun genPoint i =
      (genReal (2 * i), genReal (2 * i + 1))

    val (points, tm) = Util.getTime (fn _ => Seq.tabulate genPoint n)
    val _ = print ("generated input in " ^ Time.fmt 4 tm ^ "s\n")
  in
    points
  end

(* This silly thing helps ensure good placement, by
 * forcing points to be reallocated more adjacent.
 * It's a no-op, but gives us as much as 2x time
 * improvement (!)
 *)
fun swap pts =
  Seq.map (fn (x, y) => (y, x)) pts
fun compactify pts =
  swap (swap pts)

fun parseInputFile () =
  let
    val (points, tm) = Util.getTime (fn _ =>
      compactify (ParseFile.readSequencePoint2d filename))
  in
    print ("parsed input points in " ^ Time.fmt 4 tm ^ "s\n");
    points
  end


val points =
  case filename of
    "" => generateInputPoints ()
  | _ => parseInputFile ()

val input =
  Seq.mapIdx
    (fn (i, (x, y)) => DT.Point.T {id = I.fromInt i, x = DT.r x, y = DT.r y})
    points

val mesh = Benchmark.run "delaunay-top-down" (fn _ => DT.triangulate input)
val _ = print ("num triangles " ^ Int.toString (Seq.length mesh) ^ "\n")

(* ===================================================================== *)

val filename = CLA.parseString "output" ""
val _ =
  if filename <> "" then
    ()
  else
    ( print
        ("\nto see output, use -output and -resolution arguments\n"
         ^
         "for example: delaunay-top-down -n 1000 -output result.ppm -resolution 1000\n")
    ; OS.Process.exit OS.Process.success
    )

val t0 = Time.now ()

val resolution = CLA.parseInt "resolution" 1000
val width = resolution
val height = resolution

val image =
  { width = width
  , height = height
  , data = Seq.tabulate (fn _ => Color.white) (width * height)
  }

fun set (i, j) x =
  if 0 <= i andalso i < height andalso 0 <= j andalso j < width then
    ArraySlice.update (#data image, i * width + j, x)
  else
    ()

fun setxy (x, y) z =
  set (resolution - y - 1, x) z

val r = Real.fromInt resolution
fun px x =
  Real.floor (x * r + 0.5)

fun ipart x = Real.floor x
fun fpart x = x - Real.realFloor x
fun rfpart x = 1.0 - fpart x

(** input points should be in range [0,1] *)
fun aaLine (x0, y0) (x1, y1) =
  if x1 < x0 then
    aaLine (x1, y1) (x0, y0)
  else
    let
      (** scale to resolution *)
      val (x0, y0, x1, y1) =
        (r * x0 + 0.5, r * y0 + 0.5, r * x1 + 0.5, r * y1 + 0.5)

      fun plot (x, y, c) =
        let
          val c = Word8.fromInt (Real.ceil (255.0 * (1.0 - c)))
          val color = {blue = c, green = c, red = 0w255}
        in
          (* print (Int.toString x ^ " " ^ Int.toString y ^ "\n"); *)
          setxy (x, y) color
        end

      val dx = x1 - x0
      val dy = y1 - y0
      val yxSlope = dy / dx
      val xySlope = dx / dy
      (* val xhop = Real.fromInt (Real.sign dx) *)
      (* val yhop = Real.fromInt (Real.sign dy) *)

      (* fun y x = x0 + (x-x0) * slope  *)

      (** (x,y) = current point on the line *)
      fun normalLoop (x, y) =
        if x > x1 then
          ()
        else
          ( plot (ipart x, ipart y, rfpart y)
          ; plot (ipart x, ipart y + 1, fpart y)
          ; normalLoop (x + 1.0, y + yxSlope)
          )

      fun steepUpLoop (x, y) =
        if y > y1 then
          ()
        else
          ( plot (ipart x, ipart y, rfpart x)
          ; plot (ipart x + 1, ipart y, fpart x)
          ; steepUpLoop (x + xySlope, y + 1.0)
          )

      fun steepDownLoop (x, y) =
        if y < y1 then
          ()
        else
          ( plot (ipart x, ipart y, rfpart x)
          ; plot (ipart x + 1, ipart y, fpart x)
          ; steepDownLoop (x - xySlope, y - 1.0)
          )
    in
      if Real.abs dx > Real.abs dy then normalLoop (x0, y0)
      else if y1 > y0 then steepUpLoop (x0, y0)
      else steepDownLoop (x0, y0)
    end

(* draw all triangle edges as straight red lines *)
val _ = ForkJoin.parfor 1000 (0, Seq.length mesh) (fn i =>
  let
    (* val _ = print ("triangle number " ^ Int.toString i ^ "\n") *)
    (** cut off anything that is outside the image (not important other than
      * a little faster this way).
      *)
    fun constrain (x, y) =
      (Real.min (1.0, Real.max (0.0, x)), Real.min (1.0, Real.max (0.0, y)))

    (* fun vpos v = constrain (T.vdata mesh v)
    fun doLineIf b (u, v) =
      if b then aaLine (vpos u) (vpos v) else () *)

    fun doLineIf _ (id1, id2) =
      let
        val id1 = I.toInt id1
        val id2 = I.toInt id2
      in
        if
          id1 >= Seq.length points orelse id2 >= Seq.length points
          orelse id1 < 0 orelse id2 < 0
        then
          ()
        else
          aaLine (constrain (Seq.nth points id1)) (constrain
            (Seq.nth points id2))
      end

    val (u, v, w) = Seq.nth mesh i
  (* val () = print
    ("drawing " ^ I.toString u ^ " " ^ I.toString v ^ " " ^ I.toString w
     ^ "\n") *)
  in
    (** TODO: ensure each line segment is only drawn once? There is overlap
      * here with adjacent triangles.
      *)
    doLineIf true (w, u);
    doLineIf true (u, v);
    doLineIf true (v, w)
  end
  handle e => (print ("error at " ^ Int.toString i ^ "\n"); raise e))

val _ = print ("drew all triangles\n")

(* mark input points as a pixel *)
val _ = ForkJoin.parfor 10000 (0, Seq.length points) (fn i =>
  let
    val (x, y) = Seq.nth points i
    val (x, y) = (px x, px y)
    fun b spot = setxy spot Color.black
  in
    b (x - 1, y);
    b (x, y - 1);
    b (x, y);
    b (x, y + 1);
    b (x + 1, y)
  end)

val t1 = Time.now ()

val _ = print ("generated image in " ^ Time.fmt 4 (Time.- (t1, t0)) ^ "s\n")

val (_, tm) = Util.getTime (fn _ => PPM.write filename image)
val _ = print ("wrote to " ^ filename ^ " in " ^ Time.fmt 4 tm ^ "s\n")

structure CLA = CommandLineArgs
structure T = Topology2D
structure DT = DelaunayTriangulation

val n = CLA.parseInt "n" (1000 * 1000)
val seed = CLA.parseInt "seed" 15210

fun genReal i =
  let
    val x = Word64.fromInt (seed + i)
  in
    Real.fromInt (Word64.toInt (Word64.mod (Util.hash64 x, 0w1000000)))
    / 1000000.0
  end

fun genPoint i = (genReal (2*i), genReal (2*i + 1))
val (input, tm) = Util.getTime (fn _ => Seq.tabulate genPoint n)
val _ = print ("generated input in " ^ Time.fmt 4 tm ^ "s\n")

val mesh = Benchmark.run "delaunay" (fn _ => DT.triangulate input)

(* val _ =
  print ("\n" ^ T.toString mesh ^ "\n") *)


(* ==========================================================================
 * output result image
 * only works if all input points are in range [0,1)
 *)

val filename = CLA.parseString "output" ""
val _ =
  if filename <> "" then ()
  else ( print ("to see output, use -output and -resolution arguments\n" ^
                "for example: delaunay -n 1000 -output result.ppm -resolution 1000\n")
       ; OS.Process.exit OS.Process.success
       )

val t0 = Time.now ()

val resolution = CLA.parseInt "resolution" 1000
val width = resolution
val height = resolution

val image =
  { width = width
  , height = height
  , data = Seq.tabulate (fn _ => Color.white) (width*height)
  }

fun set (i, j) x =
  if 0 <= i andalso i < height andalso
     0 <= j andalso j < width
  then ArraySlice.update (#data image, i*width + j, x)
  else ()

val r = Real.fromInt resolution
fun px x = Real.floor (x * r)
fun pos (x, y) = (resolution - px x - 1, px y)

fun horizontalLine i (j0, j1) =
  if j1 < j0 then horizontalLine i (j1, j0)
  else Util.for (j0, j1) (fn j => set (i, j) Color.red)

fun sign xx =
  case Int.compare (xx, 0) of LESS => ~1 | EQUAL => 0 | GREATER => 1

(* Bresenham's line algorithm *)
fun line (x1, y1) (x2, y2) =
  let
    val w = x2 - x1
    val h = y2 - y1
    val dx1 = sign w
    val dy1 = sign h
    val (longest, shortest, dx2, dy2) =
      if Int.abs w > Int.abs h then
        (Int.abs w, Int.abs h, dx1, 0)
      else
        (Int.abs h, Int.abs w, 0, dy1)

    fun loop i numerator x y =
      if i > longest then () else
      let
        val numerator = numerator + shortest;
      in
        set (x, y) Color.red;
        if numerator >= longest then
          loop (i+1) (numerator-longest) (x+dx1) (y+dy1)
        else
          loop (i+1) numerator (x+dx2) (y+dy2)
      end
  in
    loop 0 (longest div 2) x1 y1
  end

(* draw all triangle edges as straight red lines *)
val _ = ForkJoin.parfor 1000 (0, T.numTriangles mesh) (fn i =>
  let
    fun vpos v = pos (T.vdata mesh v)
    val T.Tri {vertices=(u,v,w), ...} = T.tdata mesh i
  in
    line (vpos u) (vpos v);
    line (vpos v) (vpos w);
    line (vpos w) (vpos u)
  end)

(* mark input points as a pixel *)
val _ =
  ForkJoin.parfor 10000 (0, Seq.length input) (fn i =>
    let
      val (x, y) = pos (Seq.nth input i)
      fun b spot = set spot Color.black
    in
      b (x-1, y);
      b (x, y-1);
      b (x, y);
      b (x, y+1);
      b (x+1, y)
    end)

val t1 = Time.now ()

val _ = print ("generated image in " ^ Time.fmt 4 (Time.- (t1, t0)) ^ "s\n")

val (_, tm) = Util.getTime (fn _ => PPM.write filename image)
val _ = print ("wrote to " ^ filename ^ " in " ^ Time.fmt 4 tm ^ "s\n")

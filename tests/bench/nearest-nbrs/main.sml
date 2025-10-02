structure NN = NearestNeighbors
structure CLA = CommandLineArgs

val n = CLA.parseInt "N" 1000000
val inputFile = CLA.parseString "input" ""
val leafSize = CLA.parseInt "leafSize" 50
val grain = CLA.parseInt "grain" 1000
val seed = CLA.parseInt "seed" 15210

fun genReal i =
  let
    val x = Word64.fromInt (seed + i)
  in
    Real.fromInt (Word64.toInt (Word64.mod (Util.hash64 x, 0w1000000)))
    / 1000000.0
  end

fun genPoint i = (genReal (2*i), genReal (2*i + 1))

(* This silly thing helps ensure good placement, by
 * forcing points to be reallocated more adjacent.
 * It's a no-op, but gives us as much as 2x time
 * improvement (!)
 *)
fun swap pts = Seq.map (fn (x, y) => (y, x)) pts
fun compactify pts = swap (swap pts)

val input =
  case inputFile of
    "" =>
      let
        val (input, tm) = Util.getTime (fn _ => Seq.tabulate genPoint n)
      in
        print ("generated input in " ^ Time.fmt 4 tm ^ "s\n");
        input
      end

  | filename =>
      let
        val (points, tm) = Util.getTime (fn _ =>
          compactify (ParseFile.readSequencePoint2d filename))
      in
        print ("parsed input points in " ^ Time.fmt 4 tm ^ "s\n");
        points
      end

fun nnEx() =
  let
    val (tree, tm) = Util.getTime (fn _ => NN.makeTree leafSize input)
    val _ = print ("built quadtree in " ^ Time.fmt 4 tm ^ "s\n")

    val (nbrs, tm) = Util.getTime (fn _ => NN.allNearestNeighbors grain tree)
    val _ = print ("found all neighbors in " ^ Time.fmt 4 tm ^ "s\n")
  in
    (tree, nbrs)
  end

val (tree, nbrs) = Benchmark.run "running nearest neighbors" nnEx
val _ =
  print ("result " ^ Util.summarizeArraySlice 12 Int.toString nbrs ^ "\n")

(* now input[nbrs[i]] is the closest point to input[i] *)

(* ==========================================================================
 * write image to output
 * this only works if all input points are within [0,1) *)

val filename = CLA.parseString "output" ""
val _ =
  if filename <> "" then ()
  else ( print ("to see output, use -output and -resolution arguments\n" ^
                "for example: nn -N 10000 -output result.ppm -resolution 1000\n")
       ; GCStats.report ()
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

(* mark all nearest neighbors with straight red lines *)
val t0 = Time.now ()

val _ = ForkJoin.parfor 10000 (0, Seq.length input) (fn i =>
  line (pos (Seq.nth input i)) (pos (Seq.nth input (Seq.nth nbrs i))))

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


structure NN :
sig
  type t
  type 'a seq = 'a PureSeq.t

  type point = Geometry2D.point

  (* makeTree leafSize points *)
  val makeTree : int -> point seq -> t

  (* allNearestNeighbors grain quadtree *)
  val allNearestNeighbors : int -> t -> int seq
end =
struct

  structure A = Array
  structure AS = ArraySlice
  structure V = Vector
  structure VS = VectorSlice
  val unsafeCast: 'a array -> 'a vector = VectorExtra.unsafeFromArray

  type 'a seq = 'a VS.slice
  structure G = Geometry2D
  type point = G.point

  fun par4 (a, b, c, d) =
    let
      val ((ar, br), (cr, dr)) =
        ForkJoin.par (fn _ => ForkJoin.par (a, b),
                      fn _ => ForkJoin.par (c, d))
    in
      (ar, br, cr, dr)
    end

  datatype tree =
    Leaf of { anchor : point
            , width : real
            , vertices : int seq  (* indices of original point seq *)
            }
  | Node of { anchor : point
            , width : real
            , count : int
            , children : tree seq
            }

  type t = tree * point seq

  fun count t =
    case t of
      Leaf {vertices, ...} => PureSeq.length vertices
    | Node {count, ...} => count

  fun anchor t =
    case t of
      Leaf {anchor, ...} => anchor
    | Node {anchor, ...} => anchor

  fun width t =
    case t of
      Leaf {width, ...} => width
    | Node {width, ...} => width

  fun boxOf t =
    case t of
      Leaf {anchor=(x,y), width, ...} => (x, y, x+width, y+width)
    | Node {anchor=(x,y), width, ...} => (x, y, x+width, y+width)

  fun indexApp grain f t =
    let
      fun downSweep offset t =
        case t of
          Leaf {vertices, ...} =>
            VS.appi (fn (i, v) => f (offset + i, v)) vertices
        | Node {children, ...} =>
            let
              fun q i = VS.sub (children, i)
              fun qCount i = count (q i)
              val offset0 = offset
              val offset1 = offset0 + qCount 0
              val offset2 = offset1 + qCount 1
              val offset3 = offset2 + qCount 2
            in
              if count t <= grain then
                ( downSweep offset0 (q 0)
                ; downSweep offset1 (q 1)
                ; downSweep offset2 (q 2)
                ; downSweep offset3 (q 3)
                )
              else
                ( par4
                    ( fn _ => downSweep offset0 (q 0)
                    , fn _ => downSweep offset1 (q 1)
                    , fn _ => downSweep offset2 (q 2)
                    , fn _ => downSweep offset3 (q 3)
                    )
                ; ()
                )
            end
    in
      downSweep 0 t
    end

  fun indexMap grain f t =
    let
      val result = ForkJoin.alloc (count t)
      val _ = indexApp grain (fn (i, v) => A.update (result, i, f (i, v))) t
    in
      VS.full (unsafeCast result)
    end

  fun flatten grain t = indexMap grain (fn (_, v) => v) t

  (* val lowerTime = ref Time.zeroTime
  val upperTime = ref Time.zeroTime
  fun addTm r t =
    if Primitives.numberOfProcessors = 1 then
      r := Time.+ (!r, t)
    else ()
  fun clearAndReport r name =
    (print (name ^ " " ^ Time.fmt 4 (!r) ^ "\n"); r := Time.zeroTime) *)

  (* Make a tree where all points are in the specified bounding box. *)
  fun makeTreeBounded leafSize (verts : point seq) (idx : int Seq.t) ((xLeft, yBot) : G.point) width =
    if Seq.length idx <= leafSize then
      Leaf { anchor = (xLeft, yBot)
           , width = width
           , vertices = PureSeq.fromSeq idx
           }
    else let
      val qw = width/2.0 (* quadrant width *)
      val center = (xLeft + qw, yBot + qw)

      val ((sorted, offsets), tm) = Util.getTime (fn () =>
        CountingSort.sort idx (fn i =>
          G.quadrant center (PureSeq.nth verts (Seq.nth idx i))) 4)

      (* val _ =
        if AS.length idx >= 4 * leafSize then
          addTm upperTime tm
        else
          addTm lowerTime tm *)

      fun quadrant i =
        let
          val start = AS.sub (offsets, i)
          val len = AS.sub (offsets, i+1) - start
          val childIdx = AS.subslice (sorted, start, SOME len)
          val qAnchor =
            case i of
              0 => (xLeft + qw, yBot + qw)
            | 1 => (xLeft, yBot + qw)
            | 2 => (xLeft, yBot)
            | _ => (xLeft + qw, yBot)
        in
          makeTreeBounded leafSize verts childIdx qAnchor qw
        end

      (* val children = Seq.tabulate (Perf.grain 1) quadrant 4 *)
      val (a, b, c, d) =
        if AS.length idx <= 100 then
          (quadrant 0, quadrant 1, quadrant 2, quadrant 3)
        else
          par4
          ( fn _ => quadrant 0
          , fn _ => quadrant 1
          , fn _ => quadrant 2
          , fn _ => quadrant 3 )
      val children = PureSeq.fromList [a,b,c,d]
    in
      Node { anchor = (xLeft, yBot)
           , width = width
           , count = AS.length idx
           , children = children
           }
    end

  fun loop (lo, hi) b f =
    if (lo >= hi) then b else loop (lo+1, hi) (f (b, lo)) f

  fun reduce grain f b (get, lo, hi) =
    if hi - lo <= grain then
      loop (lo, hi) b (fn (b, i) => f (b, get i))
    else let
        val mid = lo + (hi-lo) div 2
        val (l,r) = ForkJoin.par
          ( fn _ => reduce grain f b (get, lo, mid)
          , fn _ => reduce grain f b (get, mid, hi)
          )
      in
        f (l, r)
      end

  fun makeTree leafSize (verts : point seq) =
    if PureSeq.length verts = 0 then raise Fail "makeTree with 0 points" else
    let
      (* calculate the bounding box *)
      fun maxPt ((x1,y1),(x2,y2)) = (Real.max (x1, x2), Real.max (y1, y2))
      fun minPt ((x1,y1),(x2,y2)) = (Real.min (x1, x2), Real.min (y1, y2))
      fun getPt i = PureSeq.nth verts i
      val (xLeft,yBot) = reduce 10000 minPt (Real.posInf, Real.posInf) (getPt, 0, VS.length verts)
      val (xRight,yTop) = reduce 10000 maxPt (Real.negInf, Real.negInf) (getPt, 0, VS.length verts)
      val width = Real.max (xRight-xLeft, yTop-yBot)

      val idx = Seq.tabulate (fn i => i) (PureSeq.length verts)
      val result = makeTreeBounded leafSize verts idx (xLeft, yBot) width
    in
      (* clearAndReport upperTime "upper sort time"; *)
      (* clearAndReport lowerTime "lower sort time"; *)
      (result, verts)
    end

  (* ======================================================================== *)

  fun constrain (x : real) (lo, hi) =
    if x < lo then lo
    else if x > hi then hi
    else x

  fun distanceToBox (x,y) (xLeft, yBot, xRight, yTop) =
    G.distance (x,y) (constrain x (xLeft, xRight), constrain y (yBot, yTop))

  val dummyBest = (~1, Real.posInf)

  fun nearestNeighbor (t : tree, pts) (pi : int) =
    let
      fun pt i = PureSeq.nth pts i

      val p = pt pi

      fun refineNearest (qi, (bestPt, bestDist)) =
        if pi = qi then (bestPt, bestDist) else
        let
          val qDist = G.distance p (pt qi)
        in
          if qDist < bestDist
          then (qi, qDist)
          else (bestPt, bestDist)
        end

      fun search (best as (_, bestDist : real)) t =
        if distanceToBox p (boxOf t) > bestDist then best else
        case t of
          Leaf {vertices, ...} =>
            VS.foldl refineNearest best vertices
        | Node {anchor=(x,y), width, children, ...} =>
            let
              val qw = width/2.0
              val center = (x+qw, y+qw)

              (* search the quadrant that p is in first *)
              val heuristicOrder =
                case G.quadrant center p of
                  0 => [0,1,2,3]
                | 1 => [1,0,2,3]
                | 2 => [2,1,3,0]
                | _ => [3,0,2,1]

              fun child i = VS.sub (children, i)
              fun refine (i, best) = search best (child i)
            in
              List.foldl refine best heuristicOrder
            end

      val (best, _) = search dummyBest t
    in
      best
    end

  fun allNearestNeighbors grain (t, pts) =
    let
      val n = PureSeq.length pts
      val idxs = flatten 10000 t
      val nn = ForkJoin.alloc n
    in
      ForkJoin.parfor grain (0, n) (fn i =>
        let
          val j = PureSeq.nth idxs i
        in
          A.update (nn, j, nearestNeighbor (t, pts) j)
        end);
      VS.full (unsafeCast nn)
    end

end

(* ==========================================================================
 * Now the main bit
 *)

structure CLA = CommandLineArgs

val n = CLA.parseInt "N" 1000000
val leafSize = CLA.parseInt "leafSize" 16
val grain = CLA.parseInt "grain" 100
val seed = CLA.parseInt "seed" 15210

fun genReal i =
  let
    val x = Word64.fromInt (seed + i)
  in
    Real.fromInt (Word64.toInt (Word64.mod (Util.hash64 x, 0w1000000)))
    / 1000000.0
  end

fun genPoint i = (genReal (2*i), genReal (2*i + 1))
val (input, tm) = Util.getTime (fn _ => PureSeq.tabulate genPoint n)
val _ = print ("generated input in " ^ Time.fmt 4 tm ^ "s\n")

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

val _ = ForkJoin.parfor 10000 (0, PureSeq.length input) (fn i =>
  line (pos (PureSeq.nth input i)) (pos (PureSeq.nth input (PureSeq.nth nbrs i))))

(* mark input points as a pixel *)
val _ =
  ForkJoin.parfor 10000 (0, PureSeq.length input) (fn i =>
    let
      val (x, y) = pos (PureSeq.nth input i)
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


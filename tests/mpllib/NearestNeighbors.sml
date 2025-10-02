structure NearestNeighbors:
sig
  type point = Geometry2D.point
  type 'a seq = 'a ArraySlice.slice

  type tree
  type t = tree * point seq

  (* makeTree leafSize points *)
  val makeTree: int -> point seq -> t

  val nearestNeighbor: t -> point -> int (* id of nearest neighbor *)
  val nearestNeighborOfId: t -> int -> int

  (* allNearestNeighbors grain quadtree *)
  val allNearestNeighbors: int -> t -> int seq
end =
struct

  structure A = Array
  structure AS = ArraySlice

  type 'a seq = 'a ArraySlice.slice
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
      Leaf {vertices, ...} => AS.length vertices
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
            AS.appi (fn (i, v) => f (offset + i, v)) vertices
        | Node {children, ...} =>
            let
              fun q i = AS.sub (children, i)
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
      AS.full result
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
  fun makeTreeBounded leafSize (verts : point seq) (idx : int seq) ((xLeft, yBot) : G.point) width =
    if AS.length idx <= leafSize then
      Leaf { anchor = (xLeft, yBot)
           , width = width
           , vertices = idx
           }
    else let
      val qw = width/2.0 (* quadrant width *)
      val center = (xLeft + qw, yBot + qw)

      val ((sorted, offsets), tm) = Util.getTime (fn () =>
        CountingSort.sort idx (fn i =>
          G.quadrant center (Seq.nth verts (Seq.nth idx i))) 4)

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
      val children = AS.full (Array.fromList [a,b,c,d])
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
    if AS.length verts = 0 then raise Fail "makeTree with 0 points" else
    let
      (* calculate the bounding box *)
      fun maxPt ((x1,y1),(x2,y2)) = (Real.max (x1, x2), Real.max (y1, y2))
      fun minPt ((x1,y1),(x2,y2)) = (Real.min (x1, x2), Real.min (y1, y2))
      fun getPt i = Seq.nth verts i
      val (xLeft,yBot) = reduce 10000 minPt (Real.posInf, Real.posInf) (getPt, 0, AS.length verts)
      val (xRight,yTop) = reduce 10000 maxPt (Real.negInf, Real.negInf) (getPt, 0, AS.length verts)
      val width = Real.max (xRight-xLeft, yTop-yBot)

      val idx = Seq.tabulate (fn i => i) (Seq.length verts)
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


  (** The function isSamePoint given as argument indicates whether or not some
    * other index is the same as the input point p. This is important for
    * querying nearest neighbors of points already in the set, for example
    * nearestNeighborOfId below. For query points outside of the set,
    * isSamePoint can always return false.
    *)
  fun nearestNeighbor_ (t : tree, pts) (p: G.point, isSamePoint: int -> bool) =
    let
      fun pt i = Seq.nth pts i

      fun refineNearest (qi, (bestPt, bestDist)) =
        if isSamePoint qi then (bestPt, bestDist) else
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
            AS.foldl refineNearest best vertices
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

              fun child i = AS.sub (children, i)
              fun refine (i, best) = search best (child i)
            in
              List.foldl refine best heuristicOrder
            end

      val (best, _) = search dummyBest t
    in
      best
    end


  fun nearestNeighborOfId (tree, pts) pi =
    nearestNeighbor_ (tree, pts) (Seq.nth pts pi, fn qi => pi = qi)

  fun nearestNeighbor (tree, pts) p =
    nearestNeighbor_ (tree, pts) (p, fn _ => false)


  fun allNearestNeighbors grain (t, pts) =
    let
      val n = Seq.length pts
      val idxs = flatten 10000 t
      val nn = ForkJoin.alloc n
    in
      ForkJoin.parfor grain (0, n) (fn i =>
        let
          val j = Seq.nth idxs i
        in
          A.update (nn, j, nearestNeighborOfId (t, pts) j)
        end);
      AS.full nn
    end

end

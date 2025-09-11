structure Topology2D:
sig
  type vertex = int
  type vertex_data = Geometry2D.point

  type triangle = int
  datatype triangle_data =
    Tri of
      { vertices: vertex * vertex * vertex
      , neighbors: triangle * triangle * triangle
      }

  type mesh
  val parseFile: string -> mesh
  val numVertices: mesh -> int
  val numTriangles: mesh -> int
  val toString: mesh -> string

  val initialMeshWithBoundaryCircle
    : {numVertices: int, numBoundaryVertices: int}
   -> {center: Geometry2D.point, radius: real}
   -> mesh

  val vdata: mesh -> vertex -> vertex_data
  val tdata: mesh -> triangle -> triangle_data
  val verticesOfTriangle: mesh -> triangle -> vertex * vertex * vertex
  val neighborsOfTriangle: mesh -> triangle -> triangle * triangle * triangle
  val triangleOfVertex: mesh -> vertex -> triangle
  val getPoints: mesh -> Geometry2D.point Seq.t
  (* val neighbor: triangle_data -> int -> triangle option *)
  (* val locate: triangle_data -> triangle -> int option *)

  type simplex

  val find: mesh -> vertex -> simplex -> simplex
  val findPoint: mesh -> Geometry2D.point -> simplex -> simplex

  val across: mesh -> simplex -> simplex option
  val rotateClockwise: simplex -> simplex
  val outside: mesh -> simplex -> vertex -> bool
  val pointOutside: mesh -> simplex -> Geometry2D.point -> bool
  val inCircle: mesh -> simplex -> vertex -> bool
  val pointInCircle: mesh -> simplex -> Geometry2D.point -> bool
  val firstVertex: mesh -> simplex -> vertex

  val split: mesh -> triangle -> Geometry2D.point -> mesh
  val flip: mesh -> simplex -> mesh

  (** A cavity is a center triangle and a set of nearby connected simplices.
    * The order of the nearby simplices is important: these must emanate
    * from the center triangle.
    *)
  type cavity = triangle * (simplex list)

  val findCavityAndPerimeter: mesh
                           -> simplex            (** where to start search *)
                           -> Geometry2D.point   (** center of the cavity *)
                           -> cavity * (vertex list)

  val loopPerimeter: mesh
                  -> triangle          (* triangle containing center point *)
                  -> Geometry2D.point  (* center of the cavity *)
                  -> 'a
                  -> ('a * vertex -> 'a)
                  -> 'a

  val findCavity: mesh
               -> triangle          (* triangle containing center point *)
               -> Geometry2D.point  (* center point of the cavity *)
               -> cavity

  val ripAndTentCavity: mesh
                     -> triangle                    (* center triangle *)
                     -> (vertex * Geometry2D.point) (* center of cavity and vertex id to use *)
                     -> triangle * triangle         (* two new triangles to use *)
                     -> unit

  (** For each (c, p), replace cavity c with a tent using p as the center
    * point. The center triangle of of the cavity must contain p.
    *)
  val ripAndTent: (cavity * Geometry2D.point) Seq.t -> mesh -> mesh
  val ripAndTentOne: cavity * Geometry2D.point -> mesh -> mesh


  (** The following are for imperative algorithms on meshes. *)

  val new: {numVertices: int, numTriangles: int} -> mesh

  val doSplit: mesh
            -> triangle                   (* triangle to split *)
            -> vertex * Geometry2D.point  (* point inside triangle, and vertex identifier to use *)
            -> triangle * triangle        (* two new triangle identifiers to create *)
            -> unit

  val doFlip: mesh -> simplex -> unit

  val copyData: {src: mesh, dst: mesh} -> unit
  val copy: mesh -> mesh

end =
struct

  structure AS = ArraySlice
  structure G = Geometry2D

  fun upd s i x = AS.update (s, i, x)
  fun nth s i = AS.sub (s, i)

  (** vertex and triangle identifiers are indices into a mesh *)
  type vertex = int
  type triangle = int

  val INVALID_ID = ~1

  type vertex_data = G.point

  (** Triangles with vertices (u,v,w) and neighbors (a,b,c) must be in
    * counter-clockwise order.
    *
    *        u
    *        | \ --> a
    *  b <-- |  w
    *        | / --> c
    *        v
    *
    * This is equivalent to any rotation, e.g. [(v,w,u),(b,c,a)]. But CCW
    * order must be preserved.
    *)
  datatype triangle_data =
    Tri of
      { vertices: vertex * vertex * vertex
      , neighbors: triangle * triangle * triangle
      }


  val dummyPt = (0.0, 0.0)
  val dummyTriple = (INVALID_ID, INVALID_ID, INVALID_ID)
  val dummyTri =
    Tri {vertices = dummyTriple, neighbors = dummyTriple}


  datatype mesh =
    Mesh of
      { vdata: vertex_data Seq.t
      , verticesOfTriangle: (vertex * vertex * vertex) Seq.t
      , neighborsOfTriangle: (triangle * triangle * triangle) Seq.t
      , triangleOfVertex: triangle Seq.t
      }

  fun new {numVertices, numTriangles} =
    Mesh { vdata = Seq.tabulate (fn _ => dummyPt) numVertices
         , triangleOfVertex = Seq.tabulate (fn _ => INVALID_ID) numVertices
         , verticesOfTriangle = Seq.tabulate (fn _ => dummyTriple) numTriangles
         , neighborsOfTriangle = Seq.tabulate (fn _ => dummyTriple) numTriangles
         }

  fun copyData {src = Mesh src, dst = Mesh dst} =
    let
      val len = Seq.length
      val lengthsOkay =
        len (#vdata src) <= len (#vdata dst) andalso
        len (#triangleOfVertex src) <= len (#triangleOfVertex dst) andalso
        len (#verticesOfTriangle src) <= len (#verticesOfTriangle dst) andalso
        len (#neighborsOfTriangle src) <= len (#neighborsOfTriangle dst)
      val _ =
        if lengthsOkay then ()
        else raise Fail "Topology2D.copyData: dst smaller than src"
    in
      ForkJoin.parfor 10000 (0, len (#vdata src)) (fn i =>
        upd (#vdata dst) i (nth (#vdata src) i));

      ForkJoin.parfor 10000 (0, len (#triangleOfVertex src)) (fn i =>
        upd (#triangleOfVertex dst) i (nth (#triangleOfVertex src) i));

      ForkJoin.parfor 10000 (0, len (#verticesOfTriangle src)) (fn i =>
        upd (#verticesOfTriangle dst) i (nth (#verticesOfTriangle src) i));

      ForkJoin.parfor 10000 (0, len (#neighborsOfTriangle src)) (fn i =>
        upd (#neighborsOfTriangle dst) i (nth (#neighborsOfTriangle src) i))
    end

  fun tdata (Mesh mesh) t =
    Tri { vertices = nth (#verticesOfTriangle mesh) t
        , neighbors = nth (#neighborsOfTriangle mesh) t
        }

  fun verticesOfTriangle (Mesh mesh) t =
    nth (#verticesOfTriangle mesh) t

  fun neighborsOfTriangle (Mesh mesh) t =
    nth (#neighborsOfTriangle mesh) t

  fun vdata (Mesh mesh) t = nth (#vdata mesh) t

  fun triangleOfVertex (Mesh mesh) v = nth (#triangleOfVertex mesh) v

  fun getPoints (Mesh {vdata, ...}) = vdata

  fun numVertices (Mesh {vdata, ...}) = Seq.length vdata
  fun numTriangles (Mesh {verticesOfTriangle, ...}) =
    Seq.length verticesOfTriangle

  fun copy mesh =
    let
      val n = numVertices mesh
      val m = numTriangles mesh
      val vdata = AS.full (ForkJoin.alloc n)
      val triangleOfVertex = AS.full (ForkJoin.alloc n)
      val verticesOfTriangle = AS.full (ForkJoin.alloc m)
      val neighborsOfTriangle = AS.full (ForkJoin.alloc m)

      val mesh' =
        Mesh { vdata = vdata
             , triangleOfVertex = triangleOfVertex
             , verticesOfTriangle = verticesOfTriangle
             , neighborsOfTriangle = neighborsOfTriangle
             }
    in
      copyData {src = mesh, dst = mesh'};
      mesh'
    end


  fun vertex (vertices as (a,b,c)) i =
    case i of
      0 => a
    | 1 => b
    | _ => c


  fun neighbor (neighbors as (a,b,c)) i =
    let
      val t' =
        case i of
          0 => a
        | 1 => b
        | _ => c
    in
      if t' < 0 then NONE else SOME t'
    end


  fun locate (neighbors as (a,b,c)) (t: triangle) =
    if a = t then SOME 0
    else if b = t then SOME 1
    else if c = t then SOME 2
    else NONE


  fun hasEdge (vertices as (a,b,c)) (u,v) =
    (u = a orelse u = b orelse u = c)
    andalso
    (v = a orelse v = b orelse v = c)


  fun sortTriangleCCW mesh (Tri {vertices=(v1,v2,v3), neighbors=(t1,t2,t3)}) =
    let
      fun p v = vdata mesh v
      val (v2, v3) =
        if G.Point.counterClockwise (p v1, p v2, p v3) then
          (v2, v3)
        else
          (v3, v2)

      fun checkHasEdge (u,v) t =
        t <> INVALID_ID andalso hasEdge (verticesOfTriangle mesh t) (u,v)

      val (t1,t2,t3) =
        if checkHasEdge (v1,v3) t1 then
          (t1,t2,t3)
        else if checkHasEdge (v1,v3) t2 then
          (t2,t1,t3)
        else
          (t3,t1,t2)

      val (t2,t3) =
        if checkHasEdge (v2,v1) t2 then
          (t2,t3)
        else
          (t3,t2)

    in
      Tri {vertices=(v1,v2,v3), neighbors=(t1,t2,t3)}
    end


  (** A simplex is an oriented triangle, which essentially just selects an
    * edge of the triangle (the integer indicates which edge with the value
    * 0, 1, or 2). The orientation allows us to define operations such as
    * "across" which returns the simplex on the other side of the
    * distinguished edge.
    *)
  type simplex = triangle * int


  fun triangleOfSimplex ((t, _): simplex) = t


  fun orientedTriangleData mesh (t, i) =
    let
      val Tri {vertices=(a,b,c), neighbors=(d,e,f)} =
        tdata mesh t
    in
      case i of
        0 => Tri {vertices=(a,b,c), neighbors=(d,e,f)}
      | 1 => Tri {vertices=(b,c,a), neighbors=(e,f,d)}
      | _ => Tri {vertices=(c,a,b), neighbors=(f,d,e)}
    end


  fun across mesh ((t, i): simplex) : simplex option =
    case neighbor (neighborsOfTriangle mesh t) i of
      SOME t' =>
        (case locate (neighborsOfTriangle mesh t') t of
          SOME i' => SOME (t', i')
        | NONE => NONE)
    | NONE => NONE


  fun fastNeighbor (neighbors as (a,b,c)) i =
    case i of
      0 => a
    | 1 => b
    | _ => c

  fun fastLocate (neighbors as (a,b,c)) (t: triangle) =
    if a = t then 0
    else if b = t then 1
    else 2

  fun fastAcross mesh ((t, i): simplex) =
    let
      val t' = fastNeighbor (neighborsOfTriangle mesh t) i
    in
      (t', fastLocate (neighborsOfTriangle mesh t') t)
    end

  fun mod3 i =
    if i > 2 then i-3 else i

  fun rotateClockwise ((t, i): simplex) : simplex =
    (t, mod3 (i+1))

  fun pointOutside mesh ((t, i): simplex) pt =
    let
      val vs = verticesOfTriangle mesh t
      val p1 = vdata mesh (vertex vs (mod3 (i+2)))
      val p2 = pt
      val p3 = vdata mesh (vertex vs i)
    in
      G.Point.counterClockwise (p1, p2, p3)
    end

  fun outside mesh (simp: simplex) v =
    pointOutside mesh simp (vdata mesh v)

  fun pointInCircle mesh ((t, _): simplex) pt =
    let
      val (a,b,c) = verticesOfTriangle mesh t
      val p1 = vdata mesh a
      val p2 = vdata mesh b
      val p3 = vdata mesh c
    in
      G.Point.inCircle (p1, p2, p3) pt
    end

  fun inCircle mesh simp v =
    pointInCircle mesh simp (vdata mesh v)

  fun firstVertex mesh ((t, i): simplex) =
    vertex (verticesOfTriangle mesh t) i


  (** ========================================================================
    * traversal and cavities
    *)

  fun findPoint mesh pt current =
    if pointOutside mesh current pt then
      findPoint mesh pt (fastAcross mesh current)
    else
    let val current = rotateClockwise current in
    if pointOutside mesh current pt then
      findPoint mesh pt (fastAcross mesh current)
    else
    let val current = rotateClockwise current in
    if pointOutside mesh current pt then
      findPoint mesh pt (fastAcross mesh current)
    else
      current
    end end


  (** find: mesh -> vertex -> simplex -> simplex *)
  fun find mesh v current =
    findPoint mesh (vdata mesh v) current


  type cavity = triangle * (simplex list)


  fun loopPerimeter mesh center pt (b: 'a) (f: 'a * vertex -> 'a) =
    let
      fun loop b t =
        if not (pointInCircle mesh t pt) then
          b
        else
          let
            val t = rotateClockwise t
            val b = loopAcross b t
            val b = f (b, firstVertex mesh t)
            val t = rotateClockwise t
            val b = loopAcross b t
          in
            b
          end

      and loopAcross b t =
        case across mesh t of
          SOME t' => loop b t'
        | NONE => b

      (* val center = findPoint mesh pt findStart *)

      val t = (center, 0)

      val b = f (b, firstVertex mesh t)
      val b = loopAcross b t

      val t = rotateClockwise t
      val b = f (b, firstVertex mesh t)
      val b = loopAcross b t

      val t = rotateClockwise t
      val b = f (b, firstVertex mesh t)
      val b = loopAcross b t
    in
      b
    end


  fun findCavityAndPerimeter mesh findStart (pt: Geometry2D.point) =
    let
      fun loop (simps, verts) t =
        if not (pointInCircle mesh t pt) then
          (simps, verts)
        else
          let
            val simps = t :: simps
            val t = rotateClockwise t
            val (simps, verts) = loopAcross (simps, verts) t
            val verts = firstVertex mesh t :: verts
            val t = rotateClockwise t
            val (simps, verts) = loopAcross (simps, verts) t
          in
            (simps, verts)
          end

      and loopAcross (simps, verts) t =
        case across mesh t of
          SOME t' => loop (simps, verts) t'
        | NONE => (simps, verts)

      val center = findPoint mesh pt findStart

      val t = center
      val (simps, verts) = ([], [])

      val verts = firstVertex mesh t :: verts
      val (simps, verts) = loopAcross (simps, verts) t

      val t = rotateClockwise t
      val verts = firstVertex mesh t :: verts
      val (simps, verts) = loopAcross (simps, verts) t

      val t = rotateClockwise t
      val verts = firstVertex mesh t :: verts
      val (simps, verts) = loopAcross (simps, verts) t

      val cavity = (triangleOfSimplex center, List.rev simps)
    in
      (cavity, verts)
    end


  fun findCavity mesh center (pt: Geometry2D.point) =
    let
      fun loop simps t =
        if not (pointInCircle mesh t pt) then
          simps
        else
          let
            val simps = t :: simps
            val t = rotateClockwise t
            val simps = loopAcross simps t
            val t = rotateClockwise t
            val simps = loopAcross simps t
          in
            simps
          end

      and loopAcross simps t =
        case across mesh t of
          SOME t' => loop simps t'
        | NONE => simps

      (* val center = findPoint mesh pt findStart *)

      val t = (center, 0)
      val simps = []

      val simps = loopAcross simps t
      val t = rotateClockwise t
      val simps = loopAcross simps t
      val t = rotateClockwise t
      val simps = loopAcross simps t

      val cavity = (center, List.rev simps)
    in
      cavity
    end


  (** ========================================================================
    * split triangle
    *)

  exception FailedReplaceNeighbor

  fun replaceNeighbor (Mesh {neighborsOfTriangle, ...}) t (old, new) =
    if t < 0 then () else
    let
      val (a,b,c) = nth neighborsOfTriangle t
      val newNeighbors =
        if old = a then (new, b, c)
        else if old = b then (a, new, c)
        else if old = c then (a, b, new)
        else raise FailedReplaceNeighbor
    in
      upd neighborsOfTriangle t newNeighbors
    end


  fun updateTriangle
    (Mesh {verticesOfTriangle, neighborsOfTriangle, ...}) t (Tri {vertices, neighbors})
    =
    ( upd verticesOfTriangle t vertices
    ; upd neighborsOfTriangle t neighbors
    )


  (** split triangle t by putting a new vertex v at point p inside. This creates
    * two new triangles, which will have ids t1 and t2. This function modifies
    * the mesh by editing triangle data (t, ta0, ta1) and vertex data (v).
    *
    *           BEFORE:                    AFTER:
    *             v1                         v1
    *             |\                         |\\
    *             |   \    t1                | \ \    t1
    *             |      \                   |  \   \
    *             |         \                |   \  t  \
    *         t2  |    t     v3          t2  |ta0 v --- v3
    *             |         /                |   / ta1 /
    *             |      /                   |  /   /
    *             |   /    t3                | / /    t3
    *             |/                         |//
    *             v2                         v2
    *)
  fun doSplit (mesh as Mesh {vdata, triangleOfVertex, ...}) t (v, p) (ta0, ta1) =
    let
      val Tri {vertices=(v1,v2,v3), neighbors=(t1,t2,t3)} = tdata mesh t
      val newdata_t =
        Tri {vertices=(v1,v,v3), neighbors=(t1,ta0,ta1)}
      val newdata_ta0 =
        Tri {vertices=(v2,v,v1), neighbors=(t2,ta1,t)}
      val newdata_ta1 =
        Tri {vertices=(v3,v,v2), neighbors=(t3,t,ta0)}
    in
      upd vdata v p;
      upd triangleOfVertex v t;
      if nth triangleOfVertex v2 <> t then ()
      else upd triangleOfVertex v2 ta0;
      updateTriangle mesh t newdata_t;
      updateTriangle mesh ta0 newdata_ta0;
      updateTriangle mesh ta1 newdata_ta1;
      replaceNeighbor mesh t2 (t,ta0);
      replaceNeighbor mesh t3 (t,ta1)
    end


  fun split (Mesh {verticesOfTriangle, neighborsOfTriangle, vdata, triangleOfVertex}) (t: triangle) p =
    let
      val n = Seq.length vdata
      val m = Seq.length neighborsOfTriangle

      (** allocate new with dummy values *)
      val vdata' = Seq.append (vdata, Seq.singleton (nth vdata 0))
      val verticesOfTriangle' = Seq.append (verticesOfTriangle, Seq.fromList [dummyTriple, dummyTriple])
      val neighborsOfTriangle' = Seq.append (neighborsOfTriangle, Seq.fromList [dummyTriple, dummyTriple])
      val triangleOfVertex' = Seq.append (triangleOfVertex, Seq.singleton t)
      val mesh' =
        Mesh { vdata=vdata'
             , verticesOfTriangle=verticesOfTriangle'
             , neighborsOfTriangle=neighborsOfTriangle'
             , triangleOfVertex=triangleOfVertex'
             }
    in
      (* print ("splitting " ^ Int.toString t ^ " into: " ^ String.concatWith " " (List.map Int.toString [t,m,m+1]) ^ "\n"); *)
      doSplit mesh' t (n, p) (m, m+1);
      mesh'
    end


  (** ========================================================================
    * flip simplex
    *)


  (** Flip the shared edge identified by the simplex.
    *
    *    BEFORE:                   AFTER:
    *            v3                         v3
    *           /|\                        / \
    *     t4  /  |  \  t3            t4  /     \  t3
    *       /    |    \                /    t    \
    *     v4  t1 |  t  v2            v4 --------- v2
    *       \    |    /                \    t1   /
    *     t5  \  |  /  t2            t5  \     /  t2
    *           \|/                        \ /
    *            v1                         v1
    *)
  fun doFlip (mesh as Mesh {triangleOfVertex, ...}) (simp: simplex) =
    let
      val Tri {vertices=(v1,v2,v3), neighbors=(t1,t2,t3)} =
        orientedTriangleData mesh simp
      val Tri {vertices=(v3_,v4,v1_), neighbors=(t,t4,t5)} =
        orientedTriangleData mesh (fastAcross mesh simp)

      (* val _ =
        print ("flipping " ^ Int.toString t ^ " and " ^ Int.toString t1 ^ "\n") *)

      (** sanity check *)
      (* val _ =
        if v3 = v3_ andalso v1 = v1_ andalso t = triangleOfSimplex simp then ()
        else raise Fail "effed up flip" *)

      val newdata_t =
        Tri {vertices=(v2,v3,v4), neighbors=(t1,t3,t4)}

      val newdata_t1 =
        Tri {vertices=(v1,v2,v4), neighbors=(t5,t2,t)}

      fun replaceTriangleOfVertex v (old, new) =
        if nth triangleOfVertex v <> old then ()
        else upd triangleOfVertex v new
    in
      updateTriangle mesh t newdata_t;
      updateTriangle mesh t1 newdata_t1;
      replaceTriangleOfVertex v1 (t, t1);
      replaceTriangleOfVertex v3 (t1, t);
      replaceNeighbor mesh t2 (t, t1);
      replaceNeighbor mesh t4 (t1, t)
    end

  fun flip (Mesh {verticesOfTriangle,neighborsOfTriangle,vdata,triangleOfVertex}) simp =
    let
      (** make a copy *)
      val mesh' =
        Mesh {verticesOfTriangle = Seq.map (fn x => x) verticesOfTriangle,
              neighborsOfTriangle = Seq.map (fn x => x) neighborsOfTriangle,
              vdata = Seq.map (fn x => x) vdata,
              triangleOfVertex = Seq.map (fn x => x) triangleOfVertex}
    in
      doFlip mesh' simp;
      mesh'
    end

  (** ========================================================================
    * loop to find cavity, and do rip-and-tent
    *)

(*
  fun ripAndTentCavity mesh center (v, pt) (ta0, ta1) =
    let
      val (_, simps) = findCavity mesh center pt
    in
      doSplit mesh center (v, pt) (ta0, ta1);
      List.app (doFlip mesh) simps
    end
*)


  fun ripAndTentCavity mesh center (v, pt) (ta0, ta1) =
    let
      fun loop t =
        if not (pointInCircle mesh t pt) then
          ()
        else
          let
            val t1 = across mesh (rotateClockwise t)
            val t2 = across mesh (rotateClockwise (rotateClockwise t))
          in
            doFlip mesh t;
            maybeLoop t1;
            maybeLoop t2
          end

      and maybeLoop t =
        case t of
          SOME t => loop t
        | NONE => ()

      val t = (center, 0)
      val t1 = across mesh t
      val t2 = across mesh (rotateClockwise t)
      val t3 = across mesh (rotateClockwise (rotateClockwise t))
    in
      doSplit mesh center (v, pt) (ta0, ta1);
      maybeLoop t1;
      maybeLoop t2;
      maybeLoop t3
    end


(*
  fun ripAndTentCavity mesh center (v, pt) (ta0, ta1) =
    let
      fun maybePush x xs =
        case x of
          SOME x => (if pointInCircle mesh x pt then x :: xs else xs)
        | NONE => xs

      fun loop ts =
        case ts of
          [] => ()
        | t :: ts =>
            let
              val t1 = across mesh (rotateClockwise t)
              val t2 = across mesh (rotateClockwise (rotateClockwise t))
            in
              doFlip mesh t;
              loop (maybePush t1 (maybePush t2 ts))
            end

      val t = (center, 0)
      val t1 = across mesh t
      val t2 = across mesh (rotateClockwise t)
      val t3 = across mesh (rotateClockwise (rotateClockwise t))
    in
      doSplit mesh center (v, pt) (ta0, ta1);
      loop (maybePush t1 (maybePush t2 (maybePush t3 [])))
    end
*)

  (** ========================================================================
    * purely functional rip-and-tent on cavities (returns new mesh)
    *)


  fun ripAndTentOne ((t, simps): cavity, pt: G.point) mesh =
    (* List.foldl (fn (s: simplex, m: mesh) => flip m s) (split mesh t pt) simps *)
    let
      val mesh' = split mesh t pt
    in
      List.app (doFlip mesh') simps;
      mesh'
    end


  fun ripAndTent cavities (Mesh {verticesOfTriangle, neighborsOfTriangle, vdata, triangleOfVertex}) =
    let
      val numVerts = Seq.length vdata
      val numTriangles = Seq.length verticesOfTriangle
      val numNewVerts = Seq.length cavities
      val numNewTriangles = 2 * numNewVerts

      val vdata' = Seq.tabulate (fn i =>
          if i < numVerts then nth vdata i else dummyPt)
        (numVerts + numNewVerts)
      val verticesOfTriangle' = Seq.tabulate (fn i =>
          if i < numTriangles then nth verticesOfTriangle i else dummyTriple)
        (numTriangles + numNewTriangles)
      val neighborsOfTriangle' = Seq.tabulate (fn i =>
          if i < numTriangles then nth neighborsOfTriangle i else dummyTriple)
        (numTriangles + numNewTriangles)
      val triangleOfVertex' = Seq.tabulate (fn i =>
          if i < numVerts then nth triangleOfVertex i else INVALID_ID)
        (numVerts + numNewVerts)
      val mesh' =
        Mesh { vdata=vdata'
             , verticesOfTriangle=verticesOfTriangle'
             , neighborsOfTriangle=neighborsOfTriangle'
             , triangleOfVertex=triangleOfVertex'
             }
    in
      ForkJoin.parfor 100 (0, Seq.length cavities) (fn i =>
        let
          val (cavity as (center, simps), pt) = nth cavities i
          val ta0 = numTriangles + 2*i
          val ta1 = ta0 + 1
        in
          doSplit mesh' center (numVerts+i, pt) (ta0, ta1);
          List.app (doFlip mesh') simps
        end);

      mesh'
    end


  (** ========================================================================
    * generating an initial boundary
    *)

  fun initialMeshWithBoundaryCircle
    {numVertices, numBoundaryVertices}
    {center, radius}
    =
    let
      val pi = Math.pi
      val n = Real.fromInt numBoundaryVertices

      val numNonBoundaryVertices = numVertices - numBoundaryVertices
      val numNonBoundaryTriangles = 2 * numNonBoundaryVertices
      val numBoundaryTriangles = numBoundaryVertices-2
      val numTriangles = numNonBoundaryTriangles + numBoundaryTriangles

      fun boundaryPoint i =
        let
          val ri = Real.fromInt i
          val x = radius * Math.cos (2.0 * pi * (ri / n))
          val y = radius * Math.sin (2.0 * pi * (ri / n))
          val offset: G.point = (x,y)
        in
          G.Vector.add (center, offset)
        end

      fun vertexPoint i =
        if i < numNonBoundaryVertices then (0.0, 0.0)
        else boundaryPoint (i - numNonBoundaryVertices)

      fun verticesOfTriangle i =
        if i < numNonBoundaryTriangles then
          (INVALID_ID, INVALID_ID, INVALID_ID)
        else
          let
            val j = i - numNonBoundaryTriangles + numNonBoundaryVertices
          in
            (j+1, j+2, numNonBoundaryVertices)
          end

      fun neighborsOfTriangle i =
        if i < numNonBoundaryTriangles then
          (INVALID_ID, INVALID_ID, INVALID_ID)
        else
          ( if i = numNonBoundaryTriangles then INVALID_ID else i-1
          , INVALID_ID
          , if i = numTriangles-1 then INVALID_ID else i+1
          )

      fun triangleOfVertex i =
        if i < numNonBoundaryVertices then
          INVALID_ID
        else
          case (i-numNonBoundaryVertices) of
            0 => numNonBoundaryTriangles
          | 1 => numNonBoundaryTriangles
          | j => numNonBoundaryTriangles + j - 2
    in
      Mesh
        { vdata = Seq.tabulate vertexPoint numVertices
        , triangleOfVertex = Seq.tabulate triangleOfVertex numVertices
        , verticesOfTriangle = Seq.tabulate verticesOfTriangle numTriangles
        , neighborsOfTriangle = Seq.tabulate neighborsOfTriangle numTriangles
        }
    end

  (** ========================================================================
    * parsing from file and string representation
    *)

  fun sortMeshCCW (mesh as Mesh {vdata, triangleOfVertex, ...}) =
    let
      val numTriangles = numTriangles mesh
      val verticesOfTriangle = AS.full (ForkJoin.alloc numTriangles)
      val neighborsOfTriangle = AS.full (ForkJoin.alloc numTriangles)
      val mesh' =
        Mesh { vdata = vdata
             , verticesOfTriangle = verticesOfTriangle
             , neighborsOfTriangle = neighborsOfTriangle
             , triangleOfVertex = triangleOfVertex
             }
    in
      ForkJoin.parfor 1000 (0, numTriangles) (fn i =>
        updateTriangle mesh' i (sortTriangleCCW mesh (tdata mesh i)));

      mesh
    end

  fun writeMax a i x =
    let
      fun loop old =
        if x <= old then () else
        let
          val old' = Concurrency.casArray (a, i) (old, x)
        in
          if old' = old then ()
          else loop old'
        end
    in
      loop (Array.sub (a, i))
    end


  fun parseFile filename =
    let
      val chars = ReadFile.contentsSeq filename

      fun isNewline i = (Seq.nth chars i = #"\n")

      val nlPos =
        AS.full (SeqBasis.filter 10000 (0, Seq.length chars) (fn i => i) isNewline)
      val numLines = Seq.length nlPos + 1
      fun lineStart i =
        if i = 0 then 0 else 1 + Seq.nth nlPos (i-1)
      fun lineEnd i =
        if i = Seq.length nlPos then Seq.length chars else Seq.nth nlPos i
      fun line i = Seq.subseq chars (lineStart i, lineEnd i - lineStart i)

      val _ =
        if numLines >= 3 then ()
        else raise Fail ("Topology2D: read mesh: missing or incomplete header")

      val _ =
        if Parse.parseString (line 0) = "Mesh" then ()
        else raise Fail ("expected Mesh header")

      fun tryParse parser test thing lineNum =
        let
          fun whoops msg =
            raise Fail ("Topology2D: line "
                        ^ Int.toString (lineNum+1)
                        ^ ": error while parsing " ^ thing
                        ^ (case msg of NONE => "" | SOME msg => ": " ^ msg))
        in
          case (parser (line lineNum) handle exn => whoops (SOME (exnMessage exn))) of
            SOME x => if test x then x else whoops (SOME "test failed")
          | NONE => whoops NONE
        end

      fun tryParseInt thing lineNum =
        tryParse Parse.parseInt (fn x => x >= 0) thing lineNum
      fun tryParseReal thing lineNum =
        tryParse Parse.parseReal (fn x => true) thing lineNum


      val numVertices = tryParseInt "num vertices" 1
      val numTriangles = tryParseInt "num triangles" 2

      fun validVid x = (0 <= x andalso x < numVertices)
      fun validTid x = (x = INVALID_ID orelse (0 <= x andalso x< numTriangles))
      fun validTriangle (Tri {vertices=(a,b,c), neighbors=(d,e,f)}) =
        validVid a andalso validVid b andalso validVid c
        andalso
        validTid d andalso validTid e andalso validTid f


      fun ff range test = FindFirst.findFirstSerial range test
      fun ss x (i, j) = Seq.subseq x (i, j-i)

      fun vertexParser line =
        let
          fun isSpace i = Char.isSpace (Seq.nth line i)
          val spaceIdx = valOf (ff (0, Seq.length line) isSpace)
        in
          SOME
            ( valOf (Parse.parseReal (Seq.take line spaceIdx))
              handle Option => raise Fail "bad first value"
            , valOf (Parse.parseReal (Seq.drop line (spaceIdx+1)))
              handle Option => raise Fail "bad second value"
            )
        end

      fun neighborsParser restOfLine =
        let
          (* val _ = print ("parsing neighbors: " ^ Parse.parseString restOfLine ^ "\n") *)
          fun isSpace i = Char.isSpace (Seq.nth restOfLine i)
          val n = Seq.length restOfLine
          val spPos = AS.full (SeqBasis.filter 10000 (0, n) (fn i => i) isSpace)
          val numNbrs = Seq.length spPos + 1
          (* val _ = print ("num neighbors: " ^ Int.toString numNbrs ^ "\n") *)
          fun nbrStart i =
            if i = 0 then 0 else 1 + Seq.nth spPos (i-1)
          fun nbrEnd i =
            if i = Seq.length spPos then Seq.length restOfLine else Seq.nth spPos i
          fun nbr i =
            if i >= numNbrs then INVALID_ID else
            ((*print ("nbr " ^ Int.toString i ^ " start " ^ Int.toString (nbrStart i) ^ " end " ^ Int.toString (nbrEnd i) ^ "\n");
             print ("nbrstring: \"" ^ Parse.parseString (ss restOfLine (nbrStart i, nbrEnd i)) ^ "\"\n");*)
            valOf (Parse.parseInt (ss restOfLine (nbrStart i, nbrEnd i)))
            handle Option => raise Fail ("bad neighbor"))
        in
          (nbr 0, nbr 1, nbr 2)
        end

      fun triangleParser line =
        let
          fun isSpace i = Char.isSpace (Seq.nth line i)
          val n = Seq.length line
          val sp1 = valOf (ff (0, n) isSpace)
          val sp2 = valOf (ff (sp1+1, n) isSpace)
          val sp3 = Option.getOpt (ff (sp2+1, n) isSpace, n)
          val verts =
            ( valOf (Parse.parseInt (ss line (0, sp1)))
              handle Option => raise Fail "bad first vertex"
            , valOf (Parse.parseInt (ss line (sp1+1, sp2)))
              handle Option => raise Fail "bad second vertex"
            , valOf (Parse.parseInt (ss line (sp2+1, sp3)))
              handle Option => raise Fail "bad third vertex"
            )
          val nbrs =
            if sp3 = n then (INVALID_ID,INVALID_ID,INVALID_ID)
            else neighborsParser (ss line (sp3+1, n))
        in
          SOME (Tri {vertices=verts, neighbors=nbrs})
        end

      fun tryParseVertex lineNum =
        tryParse vertexParser (fn _ => true) "vertex" lineNum

      fun tryParseTriangle lineNum =
        tryParse triangleParser validTriangle "triangle" lineNum

      val _ =
        if numLines >= numVertices + numTriangles + 3 then ()
        else raise Fail ("Topology2D: not enough vertices and/or triangles to parse")

      val vertices = AS.full (SeqBasis.tabulate 1000 (0, numVertices)
        (fn i => tryParseVertex (3+i)))

      val verticesOfTriangle = AS.full (ForkJoin.alloc numTriangles)
      val neighborsOfTriangle = AS.full (ForkJoin.alloc numTriangles)
      val triangleOfVertex = ForkJoin.alloc numVertices
      val _ = ForkJoin.parfor 10000 (0, numVertices)
        (fn i => Array.update (triangleOfVertex, i, ~1))

      val _ = ForkJoin.parfor 1000 (0, numTriangles) (fn i =>
        let
          val Tri {vertices=(a,b,c), neighbors} =
            tryParseTriangle (3+numVertices+i)
        in
          upd verticesOfTriangle i (a,b,c);
          upd neighborsOfTriangle i neighbors;
          writeMax triangleOfVertex a i;
          writeMax triangleOfVertex b i;
          writeMax triangleOfVertex c i
        end)
    in
      sortMeshCCW (Mesh
        { vdata = vertices
        , verticesOfTriangle = verticesOfTriangle
        , neighborsOfTriangle = neighborsOfTriangle
        , triangleOfVertex = AS.full triangleOfVertex
        })
    end


  fun toString (mesh as Mesh {vdata=vertices, ...}) =
    let
      val nv = numVertices mesh
      val nt = numTriangles mesh

      fun ptos (x,y) = Real.toString x ^ " " ^ Real.toString y

      fun ttos (Tri {vertices=(a,b,c), neighbors=(d,e,f)}) =
        String.concatWith " " (List.map Int.toString [a,b,c,d,e,f])
    in
      String.concatWith "\n"
        ([ "Mesh"
        , Int.toString nv
        , Int.toString nt
        ]
        @
        List.tabulate (nv, ptos o Seq.nth vertices)
        @
        List.tabulate (nt, ttos o tdata mesh))
    end

end

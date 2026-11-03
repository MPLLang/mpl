structure MeshToImage:
sig
  val toImage:
    { mesh: Topology2D.mesh
    , resolution: int
    , cavities: (Geometry2D.point * Topology2D.cavity) Seq.t option
    , background: Color.color
    }
    -> PPM.image
end =
struct

  structure T = Topology2D
  structure G = Geometry2D

  fun inRange (a, b) x =
    Real.min (a, b) <= x andalso x <= Real.max (a, b)

  fun xIntercept (x0,y0) (x1,y1) y =
    if not (inRange (y0, y1) y) orelse Real.== (y0, y1) then
      NONE
    else
      let
        val x = x0 + (y - y0) * ((x1-x0)/(y1-y0))
      in
        if inRange (x0, x1) x then SOME x else NONE
      end

  fun rocmp (xo, yo) =
    case (xo, yo) of
      (SOME x, SOME y) => Real.compare (x, y)
    | (NONE, SOME _) => GREATER
    | (SOME _, NONE) => LESS
    | _ => EQUAL

  fun sort3 cmp (a, b, c) =
    let
      fun lt (x, y) = case cmp (x, y) of LESS => true | _ => false
      val (a, b) = if lt (a, b) then (a, b) else (b, a)
      val (a, c) = if lt (a, c) then (a, c) else (c, a)
      val (b, c) = if lt (b, c) then (b, c) else (c, b)
    in
      (a, b, c)
    end

  fun toImage {mesh, resolution, cavities, background} =
    let
      val points = T.getPoints mesh

      val width = resolution
      val height = resolution

      val niceGray = Color.hsva {h=0.0, s=0.0, v=0.88, a=1.0}
      (* val white = Color.hsva {h=0.0, s=0.0, v=1.0, a=1.0} *)
      val black = Color.hsva {h=0.0, s=0.0, v=0.0, a=1.0}
      val red = Color.hsva {h=0.0, s=1.0, v=1.0, a=1.0}

      val niceRed = Color.hsva {h = 0.0, s = 0.55, v = 0.95, a = 0.55}
      val niceBlue = Color.hsva {h = 240.0, s = 0.55, v = 0.95, a = 0.55}

      fun alphaGray a =
        {red = 0.5, blue = 0.5, green = 0.5, alpha = a}
        (* Color.hsva {h = 0.0, s = 0.0, v = 0.7, a = a} *)

      fun alphaRed a =
        {red = 1.0, blue = 0.0, green = 0.0, alpha = a}

      val image =
        { width = width
        , height = height
        , data = Seq.tabulate (fn _ => background) (width*height)
        }

      fun set (i, j) x =
        if 0 <= i andalso i < height andalso
           0 <= j andalso j < width
        then ArraySlice.update (#data image, i*width + j, x)
        else ()

      fun setxy (x, y) z =
        set (resolution - y - 1, x) z

      fun modify (i, j) f =
        if 0 <= i andalso i < height andalso
           0 <= j andalso j < width
        then
          let
            val k = i*width + j
            val a = #data image
          in
            ArraySlice.update (a, k, f (ArraySlice.sub (a, k)))
          end
        else ()

      fun modifyxy (x, y) f =
        modify (resolution - y - 1, x) f

      fun overlay (x, y) color =
        modifyxy (x, y) (fn bg => Color.overlayColor {fg = color, bg = bg})

      val r = Real.fromInt resolution
      fun px x = Real.floor (x * r + 0.5)

      fun vpos v = T.vdata mesh v

      fun ipart x = Real.floor x
      fun fpart x = x - Real.realFloor x
      fun rfpart x = 1.0 - fpart x

      (** input points should be in range [0,1] *)
      fun aaLine colorFn (x0, y0) (x1, y1) =
        if x1 < x0 then aaLine colorFn (x1, y1) (x0, y0) else
        let
          (** scale to resolution *)
          val (x0, y0, x1, y1) = (r*x0 + 0.5, r*y0 + 0.5, r*x1 + 0.5, r*y1 + 0.5)

          fun plot (x, y, c) =
            overlay (x, y) (colorFn c)

          val dx = x1-x0
          val dy = y1-y0
          val yxSlope = dy / dx
          val xySlope = dx / dy
          (* val xhop = Real.fromInt (Real.sign dx) *)
          (* val yhop = Real.fromInt (Real.sign dy) *)

          (* fun y x = x0 + (x-x0) * slope  *)

          (** (x,y) = current point on the line *)
          fun normalLoop (x, y) =
            if x > x1 then () else
            ( plot (ipart x, ipart y    , rfpart y)
            ; plot (ipart x, ipart y + 1,  fpart y)
            ; normalLoop (x + 1.0, y + yxSlope)
            )

          fun steepUpLoop (x, y) =
            if y > y1 then () else
            ( plot (ipart x    , ipart y, rfpart x)
            ; plot (ipart x + 1, ipart y,  fpart x)
            ; steepUpLoop (x + xySlope, y + 1.0)
            )

          fun steepDownLoop (x, y) =
            if y < y1 then () else
            ( plot (ipart x    , ipart y, rfpart x)
            ; plot (ipart x + 1, ipart y,  fpart x)
            ; steepDownLoop (x - xySlope, y - 1.0)
            )
        in
          if Real.abs dx > Real.abs dy then
            normalLoop (x0, y0)
          else if y1 > y0 then
            steepUpLoop (x0, y0)
          else
            steepDownLoop (x0, y0)
        end

      fun adjust (x, y) = (r*x + 0.5, r*y + 0.5)

      fun fillTriangle color (p0, p1, p2) =
        let
          val (p0, p1, p2) = (adjust p0, adjust p1, adjust p2)

          (** min and max corners of bounding box *)
          val (xlo, ylo) = List.foldl G.Point.minCoords p0 [p1, p2]
          val (xhi, yhi) = List.foldl G.Point.maxCoords p0 [p1, p2]

          fun horizontalIntersect y =
            let
              val xa = xIntercept p0 p1 y
              val xb = xIntercept p1 p2 y
              val xc = xIntercept p0 p2 y
            in
              case sort3 rocmp (xa, xb, xc) of
                (SOME xa, SOME xb, NONE) => SOME (xa, xb)
              | _ => NONE
              (* | (SOME xa, NONE, NONE) => (xa, xa)
              | _ => raise Fail "MeshToImage.horizontalIntersect bug" *)
            end

          fun loop y =
            if y >= yhi then () else
            let
              val yy = ipart y
            in
              (case horizontalIntersect y of
                SOME (xleft, xright) =>
                  Util.for (ipart xleft, ipart xright + 1)
                  (fn xx => overlay (xx, yy) color)
              | NONE => ());

              loop (y+1.0)
            end
        in
          loop (Real.realCeil ylo)
        end

    in
      (* draw all triangle edges as straight red lines *)
      ForkJoin.parfor 1000 (0, T.numTriangles mesh) (fn i =>
        let
          (** cut off anything that is outside the image (not important other than
            * a little faster this way).
            *)
          (* fun constrain (x, y) =
            (Real.min (1.0, Real.max (0.0, x)), Real.min (1.0, Real.max (0.0, y))) *)
          (* fun vpos v = constrain (T.vdata mesh v) *)

          fun doLineIf b (u, v) =
            if b then aaLine alphaGray (vpos u) (vpos v) else ()

          val T.Tri {vertices=(u,v,w), neighbors=(a,b,c)} = T.tdata mesh i
        in
          (* skip "invalid" triangles *)
          if u < 0 orelse v < 0 orelse w < 0 then ()
          else
            (** This ensures that each line segment is only drawn once. The person
              * responsible for drawing it is the triangle with larger id.
              *)
            ( doLineIf (i > a) (w, u)
            ; doLineIf (i > b) (u, v)
            ; doLineIf (i > c) (v, w)
            )
        end);

      (* maybe fill in cavities *)
      case cavities of NONE => () | SOME cavs =>
      ForkJoin.parfor 100 (0, Seq.length cavs) (fn i =>
        let
          val (pt, (center, simps)) = Seq.nth cavs i
          val triangles = center :: List.map (fn (t, _) => t) simps

          val perimeter =
            List.map (T.vdata mesh)
            ((let val (u,v,w) = T.verticesOfTriangle mesh center
            in [u,v,w]
            end)
            @
            (List.map (fn s => T.firstVertex mesh (T.rotateClockwise s)) simps))

          fun fillTri t =
            let
              val (v0,v1,v2) = T.verticesOfTriangle mesh t
              val (p0,p1,p2) = (T.vdata mesh v0, T.vdata mesh v1, T.vdata mesh v2)
            in
              fillTriangle niceBlue (p0, p1, p2)
            end
        in
          List.app fillTri triangles;
          List.app (aaLine alphaRed pt) perimeter
        end);

      (* mark input points as a pixel *)
      ForkJoin.parfor 10000 (0, Seq.length points) (fn i =>
        let
          val (x, y) = Seq.nth points i
          val (x, y) = (px x, px y)
          fun b spot = setxy spot black
        in
          (* skip "invalid" vertices *)
          if T.triangleOfVertex mesh i < 0 then ()
          else
            ( b (x-1, y)
            ; b (x, y-1)
            ; b (x, y)
            ; b (x, y+1)
            ; b (x+1, y)
            )
        end);

      { width = #width image
      , height = #height image
      , data = Seq.map Color.colorToPixel (#data image)
      }
    end

end

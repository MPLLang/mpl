structure Geometry2D =
struct

  type point = real * real

  fun rtos x = Real.toString x

  fun toString (x, y) =
    String.concat ["(", rtos x, ",", rtos y, ")"]

  fun samePoint (x1, y1) (x2, y2) =
    Real.== (x1, x2) andalso Real.== (y1, y2)

  fun sq (x : real) = x*x

  fun distance ((x1,y1) : point) ((x2,y2) : point) =
    Math.sqrt (sq (x2-x1) + sq (y2-y1))

  fun quadrant ((cx, cy) : point) (x, y) =
    if y < cy
    then (if x < cx then 2 else 3)
    else (if x < cx then 1 else 0)
  (* *)

  structure Vector =
  struct
    type t = real * real

    val toString = toString

    fun add ((x1, y1), (x2, y2)) : t = (x1+x2, y1+y2)
    fun sub ((x1, y1), (x2, y2)) : t = (x1-x2, y1-y2)

    fun dot ((x1, y1), (x2, y2)) : real = x1*x2 + y1*y2
    fun cross ((x1, y1), (x2, y2)) : real = x1*y2 - y1*x2

    fun scaleBy a (x, y) : t = (a*x, a*y) 

    fun length (x, y) = Math.sqrt (x*x + y*y)

    fun angle (u, v) = Math.atan2 (cross (u, v), dot (u, v))
  end

  structure Point =
  struct
    type t = point

    val toString = toString

    val add = Vector.add
    val sub = Vector.sub

    fun minCoords ((a,b),(c,d)) =
      (Real.min (a,c), Real.min (b,d))

    fun maxCoords ((a,b),(c,d)) =
      (Real.max (a,c), Real.max (b,d))

    fun triArea (a, b, c) =
      Vector.cross (sub (b, a), sub (c, a))

    fun counterClockwise (a, b, c) =
      triArea (a, b, c) > 0.0

    (* Returns angle `r` inside the triangle formed by three points:
     *         b
     *        / \
     *       / r \
     *      a     c
     *)
    fun triAngle (a, b, c) =
      Vector.angle (sub (a, b), sub (c, b))


    fun inCircle (a, b, c) d =
      let
        fun onParabola ((x, y): point) : Geometry3D.Vector.t =
          (x, y, x*x + y*y)
        val ad = onParabola (Vector.sub (a, d))
        val bd = onParabola (Vector.sub (b, d))
        val cd = onParabola (Vector.sub (c, d))
      in
        Geometry3D.Vector.dot (Geometry3D.Vector.cross (ad, bd), cd) > 0.0
      end

  end

end

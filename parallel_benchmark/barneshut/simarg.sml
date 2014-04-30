functor SimArg (C: sig val theta : real val epsilon : real * real * real end) =
struct
  type v = real * real * real
  type m = v * v * v

  structure Vector : VECTOR =
  struct
    type t = v
    type m = m

    fun map f (x, y, z) = (f x, f y, f z)
    fun fold f u (x, y, z) = f (z, f (y, f (x, u)))

    fun add ((x1, y1, z1), (x2, y2, z2)) : t = (x1 + x2, y1 + y2, z1 + z2)

    fun sub ((x1, y1, z1), (x2, y2, z2)) : t = (x1 - x2, y1 - y2, z1 - z2)

    val == = Real.==
    infix 4 ==
    fun eq ((x1, y1, z1), (x2, y2, z2)) = x1 == x2 andalso y1 == y2 andalso z1 == z2
    val op< = Real.<
    fun lt ((x1, y1, z1), (x2, y2, z2)) = x1 < x2 andalso y1 < y2 andalso z1 < z2
    val op<= = Real.<=
    fun le ((x1, y1, z1), (x2, y2, z2)) = x1 <= x2 andalso y1 <= y2 andalso z1 <= z2

    fun scale (s, (x1, y1, z1)) : t = (x1 * s, y1 * s , z1 * s)
    fun point ((x1, y1, z1), (x2, y2, z2)) : t = ((x1 * x2, y1 * y2, z1 * z2))
    fun outer ((x1, y1, z1), (x2, y2, z2)) : m = ((x1 * x2, x1 * y2, x1 * z2), 
                                                  (y1 * x2, y1 * y2, y1 * z2), 
                                                  (z1 * x2, z1 * y2, z1 * z2))
    fun dot ((x1, y1, z1), (x2, y2, z2)) : real = x1 * x2 + y1 * y2 + z1 * z2 

    fun abs v = map Real.abs v

    val min = Real.min
    fun minExt ((x1, y1, z1), (x2, y2, z2)) = (min (x1, x2), min (y1, y2), min (z1, z2))
    val max = Real.max
    fun maxExt ((x1, y1, z1), (x2, y2, z2)) = (max (x1, x2), max (y1, y2), max (z1, z2))


    val op+ = add
    val op- = sub
    val op== = eq 
    val op< = lt
    val op<= = le

    fun cube x = (x, x, x)
    val zero = cube 0.0
    val negInf = cube Real.negInf
    val posInf = cube Real.posInf
  end

  structure Matrix : MATRIX = 
  struct
    type t = m
    type v = v

    val op+ = Vector.+
    val op- = Vector.-
    fun add ((a1, a2, a3), (b1, b2, b3)) = (a1 + b1, a2 + b2, a3 + b3)
    fun sub ((a1, a2, a3), (b1, b2, b3)) = (a1 - b1, a2 - b2, a3 - b3)

    val op* = Vector.scale
    fun scale (s, (r1, r2, r3)) = (s * r1, s * r2, s * r3)

    val op* = Vector.dot
    fun vec ((r1, r2, r3), v) = (r1 * v, r2 * v, r3 * v)

    val zero = ((0.0, 0.0, 0.0), (0.0, 0.0, 0.0), (0.0, 0.0, 0.0))
    val I = ((1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0))

    val op+ = add
    val op- = add
  end

  exception Sim of string

  datatype 'a tree = Node of 'a * 'a tree list

  val op== = Real.==
  fun max3 (a, b, c) = Real.max (a, Real.max (b, c))

  type box =       { position : Vector.t, dimension : Vector.t }
  type pointmass = { mass : real, position : Vector.t }
  type particle =  { position : Vector.t, velocity : Vector.t, mass : real }

  (* how good is this simulation *)
  val theta = C.theta

  (* how close are two points before we give up *)
  val epsilon = C.epsilon

  val fold = MLton.Parallel.ForkJoin.reduce

  fun rtos x = if x < 0.0 then "-" ^ (Real.fmt (StringCvt.FIX (SOME 6)) (~x))
               else Real.fmt (StringCvt.FIX (SOME 6)) x
  fun vtos (x, y, z) = concat ["(", rtos x, ", ", rtos y, ", ", rtos z, ")"]
  val vscale = 1.0
  fun particleToString { position = (position as (x, y, z)), velocity as (vx, vy, vz), mass } = 
      concat ["#{ p=", vtos position, " v=", vtos velocity, "}\n", 
              rtos x, " ", rtos y, " ", rtos z, "\n",
              "#set arrow from ", rtos x, ",", rtos y, ",", rtos z, 
              " to ", rtos (vscale * vx + x), ",", rtos (vscale * vy + y),
              ",", rtos (vscale * vz + z), "\n"  ]

  fun boxToString w { position = (position as (x, y, z)), dimension as (dx, dy, dz) } = 
      concat ["#set arrow from ", rtos x, ",", rtos ((y + dy) / 2.0), ",", rtos ((z + dz) / 2.0), 
              " to ", rtos (x + dx), ",", rtos ((y + dy) / 2.0), ",", rtos ((z + dz) / 2.0), 
              " lw ", Real.toString w, " nohead\n",
              "#set arrow from ", rtos ((x + dx) / 2.0), ",", rtos y, ",", rtos ((z + dz) / 2.0), 
              " to ", rtos ((x + dx) / 2.0), ",", rtos (y + dy), ",", rtos ((z + dz) / 2.0), 
              " lw ", Real.toString w, " nohead\n",
              "#set arrow from ", rtos ((x + dx) / 2.0), ",", rtos ((y + dy) / 2.0), ",", rtos z, 
              " to ", rtos ((x + dx) / 2.0), ",", rtos ((y + dy) / 2.0), ",", rtos (z + dz), 
              " lw ", Real.toString w, " nohead\n"]


end

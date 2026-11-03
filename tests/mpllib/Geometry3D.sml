structure Geometry3D =
struct

  type point = real * real * real

  structure Vector =
  struct
    type t = real * real * real

    fun dot ((a1, a2, a3), (b1, b2, b3)) : real =
      a1*b1 + a2*b2 + a3*b3

    fun cross ((a1, a2, a3), (b1, b2, b3)) : t =
      (a2*b3 - a3*b2, a3*b1 - a1*b3, a1*b2 - a2*b1)

    fun add ((a1, a2, a3), (b1, b2, b3)) : t =
      (a1+b1, a2+b2, a3+b3)

    fun sub ((a1, a2, a3), (b1, b2, b3)) : t =
      (a1-b1, a2-b2, a3-b3)
  end

end

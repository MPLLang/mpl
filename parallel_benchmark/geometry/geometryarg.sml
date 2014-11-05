structure TreePoints =
struct
  type point = real * real

  datatype t = Branch of int * t * int * t
             | Leaf of point list

end

structure GeometryArgSeq : GEOMETRYARG =
struct
  type point = TreePoints.point

  exception Geometry

  structure Vector : VECTOR =
  struct
    type t = point

    fun map f (x, y) = (f x, f y)
    fun fold f u (x, y) = f (y, f (x, u))

    fun add ((x1, y1), (x2, y2)) : t = (x1 + x2, y1 + y2)

    fun sub ((x1, y1), (x2, y2)) : t = (x1 - x2, y1 - y2)

    val == = Real.==
    infix 4 ==
    fun eq ((x1, y1), (x2, y2)) = x1 == x2 andalso y1 == y2
    val op< = Real.<
    fun lt ((x1, y1), (x2, y2)) = x1 < x2 andalso y1 < y2
    val op<= = Real.<=
    fun le ((x1, y1), (x2, y2)) = x1 <= x2 andalso y1 <= y2

    fun scale (s, (x1, y1)) : t = (x1 * s, y1 * s)
    fun point ((x1, y1), (x2, y2)) : t = ((x1 * x2, y1 * y2))
    fun dot ((x1, y1), (x2, y2)) : real = x1 * x2 + y1 * y2
    fun cross ((x0, y0),((x1, y1), (x2, y2))) : real = 
        (x1 - x0) * (y2 - y0) - (y1 - y0) * (x2 - x0)

    fun abs v = map Real.abs v

    val min = Real.min
    fun minExt ((x1, y1), (x2, y2)) = (min (x1, x2), min (y1, y2))
    val max = Real.max
    fun maxExt ((x1, y1), (x2, y2)) = (max (x1, x2), max (y1, y2))

    val op+ = add
    val op- = sub
    val op== = eq 
    val op< = lt
    val op<= = le

    fun cube x = (x, x)
    val zero = cube 0.0
    val negInf = cube Real.negInf
    val posInf = cube Real.posInf
  end

  datatype t = datatype TreePoints.t

  val empty = Leaf nil
  fun singleton p = Leaf [p]

  fun size (Leaf a) = length a
    | size (Branch (l, _, m, _)) = l + m

  fun append maxSeq (ps1 as Leaf a, ps2 as Leaf b) =
      let 
        val l = size ps1
      in
        if l = 0 then
          ps2
        else
          let 
            val m = size ps2
          in
            if m = 0 then 
              ps1
            else if l + m > maxSeq then
              Branch (l, ps1, m, ps2)
            else
              (Leaf (a @ b))
          end
      end
    | append _ (ps1, ps2) =
      Branch (size ps1, ps1, size ps2, ps2)

  fun fromList maxSeq a = 
      let
        fun loop (i, j) =
            let
              val len = i - j
            in
              if len < maxSeq then 
                (len, Leaf (List.take (List.drop (a, i), j - i)))
              else
                let
                  val l = len div 2
                  val ((l, ps1), (m, ps2)) = (loop (i, i + l), loop (i + l, j))
                in
                  (l + m, Branch (l, ps1, m, ps2))
                end
            end
      in
        #2 (loop (0, length a))
      end

  fun sub (Leaf a, i) = List.nth (a, i)
    | sub (Branch (l, ps1, m, ps2), i) =
      if i < l then sub (ps1, i)
      else sub (ps2, i - l)

  fun fold _ f inj g u =
      let
        fun loop (Leaf a) = 
            inj (foldl g u a)
          | loop (Branch (_, ps1, _, ps2)) = 
            f (loop ps1, loop ps2)
      in
        fn ps => loop ps
      end

  fun fork (f, g) = (f (), g ())

  fun rtos x = if x < 0.0 then "-" ^ (Real.fmt (StringCvt.FIX (SOME 6)) (~x))
               else Real.fmt (StringCvt.FIX (SOME 6)) x
  fun ptos (x, y) = concat [rtos x, ",", rtos y]

  fun printArg (Leaf a) = print ("[ " ^ (foldr (fn (p, s) => ptos p ^ ", " ^ s) " ]" a))
    | printArg (Branch (_, ps1, _, ps2)) = (print "[ "; printArg ps1; 
                                            print " "; printArg ps2; print " ]")

end

structure GeometryArgPar : GEOMETRYARG =
struct
    open GeometryArgSeq

    datatype t = datatype TreePoints.t

    val fork = MLton.Parallel.ForkJoin.fork

    fun fold maxSeq f inj g u =
        let 
          fun loop (Branch (_, ps1, _, ps2)) = 
              let
                val v1v2 = fork (fn () => loop ps1,
                                 fn () => loop ps2)
              in
                f v1v2
              end
            | loop (Leaf a) = inj (foldl g u a)
        in
          fn ps => loop ps
        end

 end

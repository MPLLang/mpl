open IO
open Graphics

val _ = openwindow NONE (800, 800)

(*
fun qhull pts =
    let fun cross_product ((x, y), ((x1, y1), (x2, y2))) =
            (x1 - x) * (y2 - y) - (y1 - y) * (x2 - x)
        fun partition first pts ln =
            let val ((x1, y1), (x2, y2)) = ln
                fun p_int pts ptsl ptsr (ml, mlcp) (mr, mrcp) =
                    case pts of
                        [] => (ptsl, ptsr, ml, mr)
                      | p::pts' =>
                        let val cp = cross_product (p, ln)
                        in
                            if cp < 0 then
                                if cp < mlcp then
                                    p_int pts' (p::ptsl) ptsr (p, cp) (mr, mrcp)
                                else
                                    p_int pts' (p::ptsl) ptsr (ml, mlcp) (mr, mrcp)
                            else if cp > 0 then
                                if cp > mrcp then
                                    p_int pts' ptsl (p::ptsr) (ml, mlcp) (p, cp)
                                else
                                    p_int pts' ptsl (p::ptsr) (ml, mlcp) (mr, mrcp)
                            else
                                p_int pts' ptsl ptsr (ml, mlcp) (mr, mrcp)
                        end
                val (l, r, ml, mr) = p_int pts [] [] ((0, 0), 0) ((0, 0), 0)
                (*val _ = print ((Int.toString (List.length l)) ^ " points left, " ^
                               (Int.toString (List.length r)) ^ " points right\n") *)
                (* val (pts1, pts2) =
                    MLton.Parallel.ForkJoin.fork
                        ((fn () => qhull_int l (x2, y2) (x1, y1) ml),
                         (fn () => qhull_int r (x1, y1) (x2, y2) mr)) *)
            in
                if first then
                    let val (pts1, pts2) =
                            MLton.Parallel.ForkJoin.fork
                                ((fn () => qhull_int l (x1, y1) (x2, y2) ml),
                                 (fn () => qhull_int r (x2, y2) (x1, y1) mr))
                    in
                        (pts1 @ [(x2, y2)] @ pts2)
                    end
                else
                    ((qhull_int l (x2, y2) (x1, y1) ml) @ [(x2, y2)])
            end
        and qhull_int pts (x1, y1) (x2, y2) (xmax, ymax) =
            if List.length pts = 0 then []
            else if List.length pts = 1 then [(xmax, ymax)]
            else
            let val (pts1, pts2) =
                    MLton.Parallel.ForkJoin.fork
                        ((fn () => partition false pts ((x1, y1), (xmax, ymax))),
                         (fn () => partition false pts ((xmax, ymax), (x2, y2))))
            in
                (pts1 @ pts2)
            end
        val minint = ~1000000
        val maxint = 1000000
        val (minx, maxx) = List.foldl
                           (fn ((x, y), ((minx, miny), (maxx, maxy))) =>
                               ((if x < minx then (x, y) else (minx, miny)),
                                (if x > maxx then (x, y) else (maxx, maxy))))
                           ((maxint, 0), (minint, 0))
                           pts
    in
        if List.length pts < 3 then pts else
        partition true pts (minx, maxx)
    end
*)

structure QHull = QuickHull (GeometryArgPar)
fun toList a =
    List.map (fn (x, y) => (Real.floor x, Real.floor y))
             (GeometryArgPar.fold 1000000 op@ (fn x => x) op:: [] a)
fun fromList l =
    GeometryArgPar.fromList 1000000
                            (List.map (fn (x, y) =>
                                          (Real.fromInt x, Real.fromInt y))
                                      l)
fun qhull pts =
    toList (QHull.hull 1000000 pts)

fun add p a =
    GeometryArgPar.append 1000000 (a, fromList [p])

(*
fun toList a =
    Array.foldl op:: [] a

fun fromList l = Array.fromList l

fun qhull a =
    let fun fib n =
            if n <= 1 then 1 else (fib (n - 1)) + (fib (n - 2))
        fun x ((a, _), b) = (a, b)
        fun y ((_, a), b) = (a, b)
        fun qhull_idx s e =
        if e - s < 100000 then
            let val slice = ArraySlice.slice (a, s, SOME (e + 1 - s))
            in
                (ArraySlice.foldl (Int.min o x) 1000 slice,
                 ArraySlice.foldl (Int.max o y) 0 slice,
                 ArraySlice.foldl (Int.max o x) 0 slice,
                 ArraySlice.foldl (Int.min o y) 1000 slice)
            end
(*
            ArraySlice.foldl
                (fn ((x, y), (l, t, r, b)) =>
                    (Int.min (x, l),
                     Int.max (y, t),
                     Int.max (x, r),
                     Int.min (y, b)))
                (1000, 0, 0, 1000)
                ()
*)
        else
            let val pivot = s + Int.div (e - s, 2)
                val ((l1, t1, r1, b1), (l2, t2, r2, b2)) =
                    MLton.Parallel.ForkJoin.fork
                        ((fn () => qhull_idx s pivot),
                         (fn () => qhull_idx (pivot + 1) e))
            in
                (Int.min (l1, l2),
                 Int.max (t1, t2),
                 Int.max (r1, r2),
                 Int.min (b1, b2))
            end
    val (l, t, r, b) = qhull_idx 0 ((Array.length a) - 1)
    in
        [(l, t), (r, t), (r, b), (l, b)]
    end

fun add p a =
    Array.tabulate (Array.length a,
                    fn i => if i = 0 then p else Array.sub (a, i - 1))
*)

(*
fun drawpoint x y =
    Graphics.drawrectangle x y 2 2
*)

fun drawpoint newpt x y =
    (Graphics.drawpoint x y;
     (case newpt of
          NONE => ()
        | SOME (x0, y0) => if x = x0 andalso y = y0
                           then print ("point (" ^ (Int.toString x) ^ ", " ^
                                       (Int.toString y) ^ ")\n")
                           else ()))

fun drawbigpoint x y =
    Graphics.drawrectangle x y 4 4

fun drawlines pts =
    case pts of
      (x1, y1)::(x2, y2)::t =>
      (IO.Graphics.drawline x1 y1 x2 y2;
       drawlines ((x2, y2)::t))
     | _ => ()

fun dedup pts =
    case pts of
     [] => []
     | x::t => x::(List.filter (fn y => x <> y) t)

fun click () =
    case IO.Graphics.maskevent (MLX.Mask.make [MLX.Mask.buttonpress]) of
        MLX.Button (_, _, _, _, _, x, y, _, _, _, _, _) =>
        (IO.Graphics.maskevent (MLX.Mask.make [MLX.Mask.buttonrelease]);
         drawpoint (SOME (x, y)) x y;
         (x, y))
      | _ => click ()

fun loop clfut pts hullfs ctr =
    let val _ = IO.yield ()
        val hullfs =
            if ctr = 0 then
                List.foldl
                    (fn (((x, y), hullf), r) =>
                        case MLton.Parallel.FutureSuspend.poll hullf of
                            SOME hull =>
                            ((if List.length hull > 0 then
                                  (drawlines (hull @ [List.hd hull]);
                                   print ("hull (" ^ (Int.toString x) ^ ", " ^
                                          (Int.toString y) ^ ")\n"))
                              else ());
                             r)
                          | NONE => ((x, y), hullf)::r)
                    []
                    hullfs
            else hullfs
        val (clfut, hullfs) =
            case MLton.Parallel.FutureSuspend.poll clfut of
                SOME (x, y) =>
                let val pts' = add (x, y) pts
                    val f = MLton.Parallel.FutureSuspend.future
                                (fn () => qhull pts')
                in
                    (MLton.Parallel.FutureSuspend.futureLat (true, click),
                     ((x, y), f)::hullfs)
                end
              | NONE => (clfut,  hullfs)
    in
        loop clfut pts hullfs ((ctr + 1) mod 1000)
    end

val _ = MLton.Random.srand (case MLton.Random.seed () of
                                SOME s => s
                              | NONE => 0w0)

fun randint () =
    (* Random.randomInt 800 *)
    (Word.toInt (Word.mod (MLton.Random.rand (), 0w750))) + 25
fun randang () =
    let val b = Word.toInt (Word.mod (MLton.Random.rand (), 0w3000))
        val r = Real.fromInt b
    in
        (r / 3000.0) * 2.0 * Math.pi
    end
val r = 400.0
val cx = 400.0
val cy = 400.0
fun randpt () =
    let val th = randang ()
        val x = cx + r * Math.cos th
        val y = cy + r * Math.sin th
    in
        (Real.floor x, Real.floor y)
    end

fun randpoints n =
    if n = 0 then [] else
    (randpt ())::(randpoints (n - 1))

val ptsl = randpoints 2000000
val pts = fromList ptsl (* Array.tabulate (3500000, randpt) *)
val f = MLton.Parallel.FutureSuspend.future (fn () => (print "starting\n";
                                                       qhull pts
                                                       before (print "done\n")))
val _ = MLton.Parallel.ForkJoin.forkLat true
            ((fn () => ()),
             (fn () =>
                 ((* List.app (fn (x, y) => drawpoint NONE x y) ptsl; *)
                  loop
                      (MLton.Parallel.FutureSuspend.futureLat (true, click))
                      pts [((0, 0), f)] 0)))

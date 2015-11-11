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
             (GeometryArgPar.fold 1000 op@ (fn x => x) op:: [] a)
fun fromList l =
    GeometryArgPar.fromList 1000
                            (List.map (fn (x, y) =>
                                          (Real.fromInt x, Real.fromInt y))
                                      l)

fun qhull pts = toList (QHull.hull 1000 pts)

(*
fun drawpoint x y =
    Graphics.drawrectangle x y 2 2
*)

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

fun drawpoint newpt x y =
    (IO.Graphics.drawpoint x y;
     case newpt of
         NONE => ()
       | SOME (x0, y0) =>
         if x = x0 andalso y = y0 then
             print ("point (" ^ (Int.toString x) ^ ", " ^
                    (Int.toString y) ^ ")\n")
         else ()
    )

fun loop prev pts hull hullf ctr =
    let val _ = MLX.usleep 10
        val b = button ()
        val (newhull, hull, hullf) =
            if ctr = 0 then
            case hullf of
                SOME f => (case MLton.Parallel.FutureSuspend.poll f of
                               SOME h => (true, h, NONE)
                             | NONE => (false, hull, SOME f))
              | NONE => (false, hull, NONE)
            else (false, hull, hullf)
        val (newpts, pts, hullf) =
            if b andalso not prev then
                let val (x, y) = mousepos ()
                    val pts' = GeometryArgPar.append
                                   100
                                   ((GeometryArgPar.singleton (Real.fromInt x,
                                                              Real.fromInt y)),
                                    pts)
                    val f = MLton.Parallel.FutureSuspend.future
                                (fn () => (print "starting hull\n";
                                           qhull pts'
                                           before print "finished hull\n"))
                in
                    (SOME (x, y), pts', SOME f)
                end
            else
                (NONE, pts, hullf)
    in
        if newhull orelse isSome newpts then
            (clear ();
             (if List.length hull > 0 then
                  drawlines (hull @ [List.hd hull])
              else ());
             (if newhull then print "hull\n" else ());
             List.app (fn (x0, y0) => drawpoint newpts x0 y0) (toList pts);
             loop b pts hull hullf ((ctr + 1) mod 1000))
        else
            loop b pts hull hullf ((ctr + 1) mod 1000)
    end

val _ = MLton.Random.srand (case MLton.Random.seed () of
                                SOME s => s
                              | NONE => 0w0)

fun randpoints n =
    let fun randint () =
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
    in
        if n = 0 then [] else
        (randpt ())::(randpoints (n - 1))
    end

val pts = fromList (randpoints 2000000)
(* val _ = List.app (fn (x, y) => drawpoint x y) pts *)
(* val hull = qhull pts *)
(*
val _ = drawlines (hull @ [List.hd hull])
val _ = print "done\n"
*)

val f = MLton.Parallel.FutureSuspend.future (fn () => (print "starting\n";
                                                       qhull pts
                                                       before (print "done\n")))
val _ = MLton.Parallel.ForkJoin.forkLat true
            ((fn () => ()),
             (fn () =>
                 (List.app (fn (x, y) => drawpoint NONE x y) (toList pts);
                  loop false pts [] (SOME f) 0)))

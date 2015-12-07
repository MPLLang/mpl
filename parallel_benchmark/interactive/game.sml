open IO
open Graphics

val _ = openwindow NONE (800, 800)

fun dist ((x1, y1), (x2, y2)) =
    Real.round (Math.sqrt
                    (Real.fromInt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))))

fun linepts ((x1, y1), (x2, y2)) =
    let val d = dist ((x1, y1), (x2, y2))
        fun linept t =
            ((Real.round (Real.* (Real.fromInt (x2 - x1), t))) + x1,
             (Real.round (Real.* (Real.fromInt (y2 - y1), t))) + y1)
    in
        (List.tabulate (d * 2,
                        (fn i => linept ((Real.fromInt i) /
                                         (Real.fromInt (d * 2))))))
    end
fun updatemat m ((x1, y1), (x2, y2)) =
    let fun setpt ((x, y), m) =
            ((* print ("(" ^ (Int.toString x) ^ ", " ^ (Int.toString y) ^ ")\n"); *)
            if x < 0 orelse x > 799 orelse y < 0 orelse y > 799 then
                m
            else
                let val col = Vector.sub (m, Int.div (x, 2))
                in
                    Vector.update (m, Int.div (x, 2),
                                   Vector.update (col, Int.div (y, 2), true))
                end
            )
    in
        List.foldl setpt m (linepts ((x1, y1), (x2, y2)))
    end

fun isset m (x, y) =
    if x < 0 orelse x > 799 orelse y < 0 orelse y > 799 then
        false
    else
        Vector.sub (Vector.sub (m, Int.div (x, 2)), Int.div (y, 2))

(*
fun insert lines ((x1, y1), (x2, y2)) =
    ((x1, y1), (x2, y2))::lines

fun colldet lines ((x1, y1), (x2, y2)) =
    let fun intersect ((x1, y1), (x2, y2)) ((x1', y1'), (x2', y2')) =
            let val (x1, y1, x2, y2) =
                    (Real.fromInt x1, Real.fromInt y1,
                     Real.fromInt x2, Real.fromInt y2)
                val (x1', y1', x2', y2') =
                    (Real.fromInt x1', Real.fromInt y1',
                     Real.fromInt x2', Real.fromInt y2')
                val m = (y2 - y1) / (x2 - x1)
                val m' = (y2' - y1') / (x2' - x1')
                val x = (m' * x2' - m * x2 + y2 - y2') / (m' - m)
                val y = m * x + y2 - m * x2
                val _ = print
                            ("(" ^ (Real.toString x) ^ ", " ^ (Real.toString y) ^ ")\n")
            in
                (x >= x1 andalso x <= x2 andalso y >= y1 andalso y <= y2)
                andalso
                (x >= x1' andalso x <= x2' andalso y >= y1' andalso y <= y2')
            end
    in
        List.exists (intersect ((x1, y1), (x2, y2))) lines
    end

val empty = []

*)

datatype moves = Forward | Left | Right

(*
fun newxy Forward (lx, ly) (x, y) = (x - lx + x, y - ly + y)
  | newxy Left (lx, ly) (x, y)    = (x - (y - ly), y + (x - lx))
  | newxy Right (lx, ly) (x, y)   = (x + (y - ly), y - (x - ly))
*)

fun newxy Forward (lx, ly) (x, y) = (x + 10, y + 10)
  | newxy Left (lx, ly) (x, y)    = (x - 10, y + 10)
  | newxy Right (lx, ly) (x, y)   = (x + 10, y - 10)


fun predict m (lx, ly) (x, y) plan =
    case plan of
        [] => (m, (lx, ly), (x, y))
      | p::t =>
        let val (x', y') = newxy p (lx, ly) (x, y)
            val m' = updatemat m ((x, y), (x', y'))
        in
            predict m' (x, y) (x', y') t
        end

fun minimax d m player (lpx, lpy) (px, py) (lcx, lcy) (cx, cy) =
    let fun newxy Forward (lx, ly) (x, y) = (x - lx + x, y - ly + y)
          | newxy Left (lx, ly) (x, y)    = (x - (y - ly), y + (x - lx))
          | newxy Right (lx, ly) (x, y)   = (x + (y - ly), y - (x - ly))
        fun nextcall mv =
            let val (px', py') =
                    if player then newxy mv (lpx, lpy) (px, py)
                    else (px, py)
                val (cx', cy') =
                    if player then (cx, cy)
                    else newxy mv (lcx, lcy) (cx, cy)
                val m' = updatemat (updatemat m ((lcx, lcy), (cx, cy)))
                                   ((lpx, lpy), (px, py))
            in
                minimax (d - 1) m' (not player) (px, py) (px', py')
                        (cx, cy) (cx', cy')
            end
        val (lpx', lpy') =
            (lpx + Int.div(px - lpx, 2), lpy + Int.div(py - lpy, 2))
        val (lcx', lcy') =
            (lcx + Int.div(cx - lcx, 2), lcy + Int.div(cy - lcy, 2))
        val plose =
            List.exists (isset m) (linepts ((lpx', lpy'), (px, py)))
        val close =
            List.exists (isset m) (linepts ((lcx', lcy'), (cx, cy)))
        fun fork (f, g) =
            if d < 5 then (f (), g ())
            else MLton.Parallel.ForkJoin.fork (f, g)
        (* val _ = print ("plan " ^ (Int.toString d) ^ "\n") *)
    in
        if d = 0 then ([], 0) else
        if plose then
            if player then ([], ~1) else ([], 1)
        else if close then
            if player then ([], 1) else ([], ~1)
        else
            let val ((fp, fs), ((lp, ls), (rp, rs))) =
                    fork ((fn () => nextcall Forward),
                          (fn () => fork ((fn () => nextcall Left),
                                          (fn () => nextcall Right))))
            in
                if ls < fs then
                    if rs < ls then (Right::rp, 0 - rs) else (Left::lp, 0 - ls)
                else
                    if rs < fs then (Right::rp, 0 - rs) else (Forward::fp, 0 - fs)
            end
    end

val _ = MLX.usleep 5000000

val m = Vector.tabulate (400, (fn _ => Vector.tabulate (400, fn _ => false)))
val (lx, ly) = mousepos ()
val (x, y) = mousepos ()
val (lcx, lcy) = (0, 0)
val (cx, cy) = (10, 10)

fun newfut plan d =
    let val (m, (lx', ly'), (x', y')) =
            predict m (lcx, lcy) (cx, cy) plan
    in
        MLton.Parallel.FutureSuspend.future
            (fn () => #1 (minimax d m false (lx, ly) (x, y)
                                  (lx', ly') (x', y')))
    end

fun newfuts plan =
    [newfut plan 5]
(*
     newfut plan 8,
     newfut plan 10]
*)

fun loop m ctr (lcx, lcy) (cx, cy) plan futs (lx, ly) =
    let val (x, y) = mousepos ()
        val pmove = dist ((lx, ly), (x, y)) > 5
    in
        if pmove then
        let val plose =
            if pmove then
                let val (lx', ly') =
                        (lx + Int.div(x - lx, 2), ly + Int.div(y - ly, 2))
                in
                    List.exists (isset m) (List.tl (linepts ((lx', ly'), (x, y))))
                end
            else false
        val close =
            if dist ((lcx, lcy), (cx, cy)) > 5 then
                let val (lcx', lcy') =
                        (lcx + Int.div(cx - lcx, 2), lcy + Int.div(cy - lcy, 2))
                in
                    List.exists (isset m) (linepts ((lcx', lcy'), (cx, cy)))
                end
            else false
        fun firstfut fs =
            case fs of
                [] => NONE
              | f::fs' =>
                (case MLton.Parallel.FutureSuspend.poll f of
                     NONE => firstfut fs'
                   | SOME plan' => SOME plan')
        val (plan', futs', (cx', cy'), ctr') =
            if ctr = 0 then
            case plan of
                [] =>
                (case firstfut futs of
                     NONE => (print "no plans ready\n";
                              ([], futs, (cx, cy), 0))
                   | SOME plan' =>
                     (print "new plan\n";
                      (plan', newfuts plan', (cx, cy), 0)))
                | m::p =>
                  let val (cx', cy') = newxy m (lcx, lcy) (cx, cy)
                      val _ = print ("(" ^ (Int.toString cx') ^ ", " ^ (Int.toString cy') ^ ")\n")
                  in
                      (p, futs, (cx', cy'), 0)
                  end
            else (plan, futs, (cx, cy), ctr - 1)
        val m' = updatemat (updatemat m ((lx, ly), (x, y)))
                           ((lcx, lcy), (cx, cy))
    in
            if plose then (print "You lose!"; MLX.usleep 5000000)
            else if close then (print "You win!"; MLX.usleep 5000000)
            else
                (setforeground (MLX.fromints 255 0 0);
                 drawline lx ly x y;
                 setforeground (MLX.fromints 0 0 255);
                 drawline lcx lcy cx cy;
                 MLX.usleep 1000;
                 loop (if pmove then m' else m) ctr' (cx, cy) (cx', cy') plan'
                      futs' (if pmove then (x, y) else (lx, ly)))
        end
        else
            (MLX.usleep 1000;
             loop m ctr (lcx, lcy) (cx, cy) plan futs (lx, ly))
    end

val _ = loop m 0 (0, 0) (10, 10) [] (newfuts []) (lx, ly)

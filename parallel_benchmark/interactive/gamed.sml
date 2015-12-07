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
fun updatemat m (x, y) =
    if x < 0 orelse x > 799 orelse y < 0 orelse y > 799 then
        m
    else
        let val col = Vector.sub (m, Int.div (x, 1))
        in
            Vector.update (m, Int.div (x, 1),
                           Vector.update (col, Int.div (y, 1), true))
        end

fun isset m (x, y) =
    if x < 0 orelse x > 799 orelse y < 0 orelse y > 799 then
        false
    else
        Vector.sub (Vector.sub (m, Int.div (x, 1)), Int.div (y, 1))

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

datatype moves = Up | Down | Left | Right

(*
fun newxy Forward (lx, ly) (x, y) = (x - lx + x, y - ly + y)
  | newxy Left (lx, ly) (x, y)    = (x - (y - ly), y + (x - lx))
  | newxy Right (lx, ly) (x, y)   = (x + (y - ly), y - (x - ly))
*)

fun newxyn n Up (x, y)    = (x, y - n)
  | newxyn n Down (x, y)  = (x, y + n)
  | newxyn n Left (x, y)  = (x - n, y)
  | newxyn n Right (x, y) = (x + n, y)

val newxy = newxyn 1

fun predict m (x, y) plan =
    case plan of
        [] => (m, (x, y))
      | p::t =>
        let val (x', y') = newxy p (x, y)
            val m' = List.foldl
                         (fn (i, m) => updatemat m (newxyn i p (x, y)))
                         m
                         [0(*, 1, 2, 3, 4, 5, 6, 7, 8, 9 *)]
        in
            predict m' (x', y') t
        end

fun lose m (x, y) =
    isset m (x, y) orelse
    x < 0 orelse
    x > 800 orelse
    y < 0 orelse
    y > 800

fun minimax d m player (px, py) (cx, cy) =
    let fun nextcall mv =
            let val (px', py') =
                    if player then newxy mv (px, py)
                    else (px, py)
                val (cx', cy') =
                    if player then (cx, cy)
                    else newxy mv (cx, cy)
                val m' =
                    if player then
                        List.foldl
                            (fn (i, m) => updatemat m (newxyn i mv (px, py)))
                            m
                            [0 (* , 1, 2, 3, 4, 5, 6, 7, 8, 9 *)]
                    else
                        List.foldl
                            (fn (i, m) => updatemat m (newxyn i mv (cx, cy)))
                            m
                            [0 (* , 1, 2, 3, 4, 5, 6, 7, 8, 9 *)]
            in
                minimax (d - 1) m' (not player) (px', py') (cx', cy')
            end
        val plose = lose m (px, py)
        val close = lose m (cx, cy)
        fun fork (f, g) =
            if d < 5 then (f (), g ())
            else MLton.Parallel.ForkJoin.fork (f, g)
        (* val _ = print ("plan " ^ (Int.toString d) ^ "\n") *)
    in
        if player andalso close then (print "close\n"; ([], 1)) else
        if (not player) andalso plose then (print "plose\n"; ([], 1))
        else if d = 0 then ([], 0)
        else
(*
            let val (p, s) = nextcall (if player then Up else Down)
            in
                ((if player then p else Down::p), ~s)
            end
*)
            let val (((up, us), (dp, ds)), ((lp, ls), (rp, rs))) =
                    fork ((fn () => fork ((fn () => nextcall Up),
                                          (fn () => nextcall Down))),
                          (fn () => fork ((fn () => nextcall Left),
                                          (fn () => nextcall Right))))
                val _ =
                    if d = 1 then
                        (print ("Up: " ^ (Int.toString us) ^ "\n");
                         print ("Down: " ^ (Int.toString ds) ^ "\n");
                         print ("Left: " ^ (Int.toString ls) ^ "\n");
                         print ("Right: " ^ (Int.toString rs) ^ "\n"))
                    else ()
            in
                List.foldl
                    (fn ((p, s), (rp, rs)) =>
                                  if s > rs then (p, s) else (rp, rs))
                    ((if player then up else Up::up), ~us)
                    [((if player then dp else Down::dp), ~ds),
                     ((if player then lp else Left::lp), ~ls),
                     ((if player then rp else Right::rp), ~rs)]
            end
    end

val _ = MLX.usleep 1000000

fun newfut plan m (px, py) (cx, cy) d =
    let val (m, (x', y')) =
            predict m (cx, cy) plan
        val _ = print ("running minimax starting at (" ^ (Int.toString x') ^
                       ", " ^ (Int.toString y') ^ ")\n")
    in
        MLton.Parallel.FutureSuspend.future
            (fn () => #1 (minimax d m false (px + 1, py + 1) (x', y')))
    end

fun newfuts plan m (px, py) (cx, cy) =
    [newfut plan m (px, py) (cx, cy) 1 (*,
     newfut plan (px, py) (cx, cy) 8,
     newfut plan (px, py) (cx, cy) 10 *)]

val newxy2 = newxyn 1

fun loop m (cx, cy) plan futs (lx, ly) dir onplan =
    let val dir' =
            case keyoption () of
                NONE => dir
              | SOME #"w" => Up
              | SOME #"a" => Left
              | SOME #"d" => Right
              | SOME #"s" => Down
              | _ => dir
        val (x, y) = newxy2 dir (lx, ly)
        val plose = lose m (x, y)
        val close = lose m (cx, cy)
        val m' = updatemat (updatemat m (lx, ly)) (cx, cy)
        fun firstfut fs =
            case fs of
                [] => NONE
              | f::fs' =>
                (case MLton.Parallel.FutureSuspend.poll f of
                     NONE => firstfut fs'
                   | SOME plan' => SOME plan')
        val (plan', futs', (cx', cy'), onplan') =
            case plan of
                [] =>
                (case firstfut futs of
                     NONE => (print "no plans ready\n";
                              ([Right], futs, newxy2 Right (cx, cy), 0))
                   | SOME plan' =>
                     (print "new plan\n";
                      case plan' of
                          mv::p =>
                          let val _ = print ((
                                          case mv of
                                              Up => "Up"
                                            | Down => "Down"
                                            | Left => "Left"
                                            | Right => "Right") ^ "\n")
                              val (cx', cy') = newxy2 mv (cx, cy)
                          in
                              ([], newfuts [mv] m' (x, y) (cx, cy),
                               (cx', cy'), 0)
                          end
                        | [] =>
                          let val (cx', cy') = newxy2 Right (cx, cy)
                          in
                              ([Right], newfuts [Right] m' (x, y) (cx, cy),
                               (cx', cy'), 0)
                          end))
                | mv::p =>
                  let val (cx', cy') = newxy2 mv (cx, cy)
                      val _ = print ("(" ^ (Int.toString cx') ^ ", " ^ (Int.toString cy') ^ ")\n")
                  in
                      if onplan = 10 then
                          ([], newfuts [mv] m' (x, y) (cx, cy), (cx', cy'), 0)
                      else
                          ([mv], futs, (cx', cy'), onplan + 1)
                  end
    in
            if plose then (print "You lose!"; MLX.usleep 5000000)
            else if close then (print "You win!"; MLX.usleep 5000000)
            else
                (setforeground (MLX.fromints 255 0 0);
                 drawline lx ly x y;
                 setforeground (MLX.fromints 0 0 255);
                 drawpoint cx cy;
                 MLX.usleep 10000;
                 loop m' (cx', cy') plan'
                      futs' (x, y) dir' onplan')
    end

val m = Vector.tabulate (800, (fn _ => Vector.tabulate (800, fn _ => false)))
val (x, y) = (790, 790)
val (cx, cy) = (10, 10)
val m' = updatemat (updatemat m (x, y)) (cx, cy)

val _ = loop m (10, 10) [Down] (newfuts [Down] m' (x, y) (cx, cy)) (x, y) Up 0

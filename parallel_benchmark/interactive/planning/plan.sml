val _ = Basic.finalizePriorities ()
val _ = Basic.init ()

structure W = Gui
structure V = Vector

structure CP = RRT
structure DP = AStar


val w = W.init ()
val (width, height) = W.dim w

val threshold = 1.0
val short_plan_interval = 5000
val long_plan_interval = 10000

val sp_prio = Priority.top
val lp_prio = Priority.bot

fun dist (x1, y1) (x2, y2) =
    let val (x1, y1) = (Real.fromInt x1, Real.fromInt y1)
        val (x2, y2) = (Real.fromInt x2, Real.fromInt y2)
    in
        Math.sqrt ((Math.pow (x1 - x2, 2.0)) + (Math.pow (y1 - y2, 2.0)))
    end
fun new_short_plan start goal sp =
    let val (width, height) = W.dim w
    in
        ignore (Thread.spawn
                    (fn _ => (let val p = CP.plan
                                             width height (W.obst w)
                                             start goal
                              in
                                  sp := SOME p;
                                  print ("new short plan of length " ^
                                         (Int.toString (List.length p)) ^ "\n")
                              end
                    ))
                    sp_prio)
    end
fun new_long_plan start goal lp =
    let fun nearestpt w (x, y) =
            #1 (V.foldli (fn (i, (x', y'), (j, d)) =>
                             let val d' = dist (x, y) (x', y')
                             in
                                 if d' < d then
                                     (i, d')
                                 else
                                     (j, d)
                             end)
                         (~1, Real.posInf)
                         (W.pts w))
            handle Subscript => (print "55\n"; raise Subscript)
        fun ptdist w p1 p2 =
            dist (V.sub (W.pts w, p1)) (V.sub (W.pts w, p2))
            handle Subscript => (print "41\n"; raise Subscript)
        val start = nearestpt w start
        val g = nearestpt w goal
        val (width, height) = W.dim w
    in
        ignore (Thread.spawn
                    (fn _ =>
                        let val path = DP.plan (W.map w) start g
                                               (ptdist w g)
                            val path' =
                                case path of
                                    SOME p =>
                                    SOME ((List.map
                                              (fn i => V.sub (W.pts w, i))
                                              p) @ [goal])
                                  | NONE => NONE
                        in
                            lp := path';
                            (case path' of
                                 SOME p =>
                                 print ("new long plan of length " ^
                                        (Int.toString (List.length p)) ^ "\n")
                               | NONE => ())
                        end)
                    lp_prio)
    end

fun loop (w, short_goal, long_goal, short_plan, long_plan,
          next_short_plan, next_long_plan) =
    let val pos = W.pos w
        val t = W.time w
        fun dir (x1, y1) (x2, y2) =
            (x2 - x1, y2 - y1)
        fun pteq (x, y) (SOME (x', y')) = x = x' andalso y = y'
          | pteq _ NONE = false
        val (short_goal, short_plan, next_short_plan) =
            case (short_goal, !long_plan) of
                (_, NONE) => (short_goal, short_plan, next_short_plan)
              | (NONE, SOME (pt'::_)) =>
                let val nsp = ref NONE
                in
                    print "no short plan\n";
                    new_short_plan pos pt' nsp;
                    (SOME pt', nsp, t + short_plan_interval)
                end
              | (SOME _, SOME (pt'::_)) =>
                (SOME pt', short_plan, next_short_plan)
              | _ =>
                (short_goal, short_plan, next_short_plan)
        val (w, short_goal, short_plan, next_short_plan) =
            case !short_plan of
                SOME (pt::sp') =>
                (if dist pos pt < threshold then
                     case (sp', !long_plan) of
                         ([], SOME []) =>
                         (w, short_goal, short_plan, next_short_plan)
                       | ([], SOME (pt'::lp')) =>
                         let val nsp = ref NONE
                         in
                             print "end of short plan\n";
                             long_plan := SOME lp';
                             new_short_plan pos pt' nsp;
                             (w, SOME pt', nsp, t + short_plan_interval)
                         end
                       | (pt'::sp', _) =>
                         (short_plan := SOME sp';
                          (w, SOME pt', short_plan, next_short_plan))
                 else
                     ((* print "moving\n"; *)
                       (if W.sense w (dir pos pt, 3.1) then
                            (* close to collision, replan *)
                            (print "too close\n";
                             (case short_goal of
                                 SOME g => new_short_plan pos g short_plan
                               | NONE => ());
                             w)
                        else
                            W.move w (dir pos pt)),
                       short_goal,
                       short_plan, next_short_plan)
                )
              | _ => (w, short_goal, short_plan, next_short_plan)
        val w = case !short_plan of
                    SOME p => W.register_short_plan w p
                  | NONE => w
        val w = case !long_plan of
                    SOME p => W.register_long_plan w p
                  | NONE => w
        val next_short_plan =
            if t >= next_short_plan then
                (print "timeout\n";
                 (case short_goal of
                      SOME g => new_short_plan pos g short_plan
                    | NONE => ());
                 t + short_plan_interval)
            else
                next_short_plan
        val next_long_plan =
            if t >= next_long_plan then
                (new_long_plan (case short_goal of SOME g => g | NONE => pos)
                               long_goal long_plan;
                 t + long_plan_interval)
            else
                next_long_plan
    in
        if dist pos long_goal < threshold then
            ()
        else
            loop (w, short_goal, long_goal, short_plan, long_plan,
                  next_short_plan, next_long_plan)
    end

(* val _ = print "planning\n" *)

              (*
val gran = 100
fun pt i =
    let val y = (Int.div (i, Int.div (width, gran))) * gran
        val x = (Int.mod (i, Int.div (width, gran))) * gran
    in
        (x, y)
    end
val pts = (Int.div (width, gran)) * (Int.div (height, gran))
fun dist p1 p2 =
    let val (x1, y1) = pt p1
        val (x2, y2) = pt p2
        val (x1, y1) = (Real.fromInt x1, Real.fromInt y1)
        val (x2, y2) = (Real.fromInt x2, Real.fromInt y2)
    in
        Math.sqrt ((Math.pow (x1 - x2, 2.0)) + (Math.pow (y1 - y2, 2.0)))
    end
fun revpt (x, y) =
    (Int.div (y, gran)) * (Int.div (width, gran)) +
    (Int.div (x, gran))
              *)
(*
fun ptdist w p1 p2 =
    dist (V.sub (W.pts w, p1)) (V.sub (W.pts w, p2))
    handle Subscript => (print "41\n"; raise Subscript)
fun optdist w p p2 =
    dist p (V.sub (W.pts w, p2))


val _ = print ("Goal: " ^ (Int.toString goal) ^ "\n")
val path =
    case AStar.astar (W.map w) (nearestpt w (W.pos w)) goal (ptdist w goal) of
        SOME path => path
      | NONE => loop ()
val path = List.map (fn i => V.sub (W.pts w, i)) path

(* val path = RRT.rrt 1 (width, height, W.obst w, W.pos w, (360, 138)) *)

val _ = print "found path\n"

fun print_path p =
    case p of
        [] => ()
      | (x, y)::t =>
        (print ("(" ^ (Int.toString x) ^ ", " ^ (Int.toString y) ^ ")\n");
         print_path t)
val _ = print_path path

fun move w p =
    case p of
        [] => loop ()
      | (x', y')::t =>
        let val (x, y) = W.pos w
        in
            case W.move w (x' - x, y' - y) of
                NONE => (print "can't move\n";
                         loop ())
              | SOME w' => (print "move\n";
                            ignore (Posix.Process.sleep (Time.fromSeconds 1));
                            move w' t)
        end
val _ = move w path
*)

val t = W.time w
val start = W.pos w
val short_goal = NONE
val long_goal = (360, 138)
val short_plan = ref NONE
val long_plan = ref NONE
val _ = new_long_plan start long_goal long_plan
val next_short_plan = t + short_plan_interval
val next_long_plan = t + long_plan_interval

val _ = loop (w, short_goal, long_goal, short_plan, long_plan,
          next_short_plan, next_long_plan)

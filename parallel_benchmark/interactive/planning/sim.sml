val _ = Basic.finalizePriorities ()
val _ = Basic.init ()

                   (*
val width = 100
val height = 100
fun obst ((x1, y1), (x2, y2)) =
    if x1 = x2 andalso x1 <> 50 then
        false
    else
        let val (x1, y1) = (Real.fromInt x1, Real.fromInt y1)
            val (x2, y2) = (Real.fromInt x2, Real.fromInt y2)
            val m = Real./ (y2 - y1, x2 - x1)
            val b = y1 - m * x1
            val y = m * 50.0 + b
        in
            y < 48.0 orelse y > 52.0
        end

structure GS = ChunkedTreeSequence
(*
struct
fun singleton x = [x]
end
*)

val path = rrt 8 (width, height, GS.singleton obst, (0, 0), (100, 0))

fun print_path p =
    case p of
        [] => ()
      | (x, y)::t =>
        (print ("(" ^ (Int.toString x) ^ ", " ^ (Int.toString y) ^ ")\n");
         print_path t)

val () = print_path path
*)

val points = [(0, 0), (2, 0), (1, 1), (3, 0), (4, 0), (3, 1), (4, 1)]
fun dist p1 p2 =
    let val (x1, y1) = List.nth (points, p1)
        val (x2, y2) = List.nth (points, p2)
        val (x1, y1) = (Real.fromInt x1, Real.fromInt y1)
        val (x2, y2) = (Real.fromInt x2, Real.fromInt y2)
    in
        Math.sqrt ((Math.pow (x1 - x2, 2.0)) + (Math.pow (y1 - y2, 2.0)))
    end
val start = 0
val goal = 6
fun h p =
    dist p goal
val g =
    Vector.fromList
        [Vector.fromList [(2.0, 1), (dist 0 2, 2)],
         Vector.fromList [(2.0, 0), (dist 1 2, 2), (1.0, 3)],
         Vector.fromList [(dist 0 2, 0), (dist 1 2, 1)],
         Vector.fromList [(1.0, 1), (1.0, 4), (1.0, 5)],
         Vector.fromList [(1.0, 3), (1.0, 6)],
         Vector.fromList [(1.0, 3), (1.0, 6)],
         Vector.fromList [(1.0, 4), (1.0, 5)]]
val l =
    case AStar.astar g start goal h of
        SOME l => l
      | NONE => (print "None!\n"; raise Match)

fun print_list l =
    case l of
        [] => ()
      | p::t => (print ((Int.toString p) ^ "\n");
                 print_list t)
val () = print_list l

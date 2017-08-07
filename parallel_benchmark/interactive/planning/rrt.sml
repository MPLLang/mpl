structure RRT =
struct

datatype point = Root
               | Node of int * int * point

val seed = case MLton.Random.seed () of
               SOME b => b
             | NONE => 0w42
val () = MLton.Random.srand seed

val pick_goal_prob = 0.1
val extend_dist = 10.0

structure GS = ChunkedTreeSequence
(*
struct
type 'a seq = 'a list
fun singleton x = [x]
val reduce = List.foldl
val map = List.map
fun append (a, b) = a @ b
end
*)

type rrt = (int * int * point) GS.seq
type path = (int * int) list

datatype result = Nothing
                | Points of rrt
                | Found of path

fun rrt mult (width, height, obst_list, (xs, ys), (xg, yg)) =
    let fun randint n =
            (Word.toInt (Word.mod (MLton.Random.rand (), Word.fromInt n)))
        fun randpt () =
            if randint (Real.round (1.0 / pick_goal_prob)) = 0 then
                (* Pick goal *)
                (xg, yg)
            else
                (randint width, randint height)
        fun dist ((x1, y1), (x2, y2)) =
            let val x1f = Real.fromInt x1
                val y1f = Real.fromInt y1
                val x2f = Real.fromInt x2
                val y2f = Real.fromInt y2
            in
                Math.sqrt (Math.pow (x1f - x2f, 2.0) +
                           Math.pow (y1f - y2f, 2.0))
            end
        fun collide path =
            let val cseq = GS.map (fn ob => ob path) obst_list
            in
                GS.reduce (fn (a, b) => a orelse b) false cseq
            end
        fun path_to_list path =
            case path of
                Root => []
              | Node (x, y, p) =>
                (x, y)::(path_to_list p)
        val maxint = case Int.maxInt of
                         SOME n => n
                       | NONE => 1000000
        fun new_pt tree =
            let fun nearest (x, y) =
                    GS.reduce (fn ((p1 as (x1, y1, _)), (p2 as (x2, y2, _))) =>
                                  if Real.<= (dist ((x, y), (x1, y1)),
                                              dist ((x, y), (x2, y2)))
                                  then p1
                                  else p2)
                              (maxint, maxint, Root)
                              tree
                fun extend ((xf, yf), (xt, yt)) =
                    let val dist = (dist ((xf, yf), (xt, yt)))
                    in
                        if dist <= extend_dist then
                            (xt, yt)
                        else
                            let val ratio = dist / extend_dist
                                val dx = Real.fromInt (xt - xf)
                                val dy = Real.fromInt (yt - yf)
                                val edx = Real.round (dx / ratio)
                                val edy = Real.round (dy / ratio)
                            in
                                (xf + edx, yf + edy)
                            end
                    end
                val (x, y) = randpt ()
                val (xt, yt, path) = nearest (x, y)
                                     handle Subscript => (print "49\n"; raise Subscript)
                val (xe, ye) = extend ((xt, yt), (x, y))
            in
                if collide ((xe, ye), (xt, yt)) then
                    Nothing
                else
                    if collide ((xe, ye), (xg, yg)) then
                        Points (GS.singleton (xe, ye, Node (xt, yt, path)))
                    else
                        Found ((xg, yg)::(xe, ye)::(xt, yt)::(path_to_list path))
            end
        fun forkn n tree () =
            case n of
                0 => Nothing
              | 1 => new_pt tree
              | n => let val n1 = Int.div (n, 2)
                         val n2 = n - n1
                         val rt = Thread.spawn (forkn n2 tree)
                                               (Basic.currentPrio ())
                         val lr = forkn n1 tree ()
                         val rr = Thread.sync rt
                     in
                         case (lr, rr) of
                             (Nothing, _) => rr
                           | (_, Nothing) => lr
                           | (Points lp, Points rp) =>
                             Points (GS.append (lp, rp))
                           | (Found p, _) => Found p
                           | (_, Found p) => Found p
                     end
        fun rrt_round mult tree =
            case forkn mult tree () of
                Nothing => rrt_round mult tree
              | Points pts => rrt_round mult (GS.append (tree, pts))
              | Found path => (List.rev path)
    in
        rrt_round mult (GS.singleton (xs, ys, Root))
    end

fun plan width height obst_list (xs, ys) (xg, yg) =
    rrt 4 (width, height, obst_list, (xs, ys), (xg, yg))

end

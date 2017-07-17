type obst = (int * int) * (int * int) -> bool
(* Foreground-only * ((x, y), (x, y)) -> Collision *)

datatype point = Root
               | Node of int * int * point

structure GS = ChunkedTreeSequence

type rrt = (int * int * point) GS.seq

fun rrt (width, height, obst_list, (xg, yg)) =
    let fun randint n =
            (Word.toInt (Word.mod (MLton.Random.rand (), Word.fromInt n)))
        fun randpt () =
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
        fun rrt_rec tree =
            let fun nearest (x, y) =
                    GS.reduce (fn ((p1 as (x1, y1, _)), (p2 as (x2, y2, _))) =>
                                  if Real.<= (dist ((x, y), (x1, y1)),
                                              dist ((x, y), (x2, y2)))
                                  then p1
                                  else p2)
                              (maxint, maxint, Root)
                              tree
                val (x, y) = randpt ()
                val (xt, yt, path) = nearest (x, y)
            in
                if collide ((x, y), (xt, yt)) then
                    rrt_rec tree
                else
                    if collide ((x, y), (xg, yg)) then
                        rrt_rec
                            (GS.append (tree,
                                       (GS.singleton (x, y, Node (xt, yt, path)))))
                    else
                        (xg, yg)::(x, y)::(xt, yt)::(path_to_list path)
            end
    in
        rrt_rec (GS.singleton (Int.div (width, 2), Int.div (height, 2), Root))
    end

type obst = (int * int) * (int * int) -> bool
(* Foreground-only * ((x, y), (x, y)) -> Collision *)

datatype point = Root
               | Node of int * int * point

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
                Math.sqrt (Math.pow (x1f - x2f, 2.) +
                           Math.pow (y1f - y2f, 2.))
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
        fun rrt_rec tree =
            let fun nearest (x, y) =
                    GS.argmax (fn ((x1, y1, _), (x2, y2, _)) =>
                                  Real.compare (dist ((x, y), (x1, y1)),
                                                dist ((x, y), (x2, y2))))
                              tree
                val (x, y) = randpt ()
                val (xt, yt, path) = nearest (x, y)
            in
                if collide ((x, y), (xy, yt)) then
                    rrt_rec tree
                else
                    if collide ((x, y), (xg, yg)) then
                        rrt_rec (GS.add tree (x, y, Node (xt, yt, path)))
                    else
                        (xg, yg)::(x, y)::(xt, yt)::(path_to_list path)
                        (xg, yg, Node (x, y, Node (xt, yt, path)))
            end

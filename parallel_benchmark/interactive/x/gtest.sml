open Graphics

val _ = openwindow NONE (300, 300)

fun loop () =
    let val (x, y) = mousepos ()
        val _ = clear ()
        val _ = drawtext NONE 0 300
                         ("(" ^ (Int.toString x) ^ ", " ^ (Int.toString y) ^ ")")
        val _ = case keyoption () of
                    NONE => ()
                  | SOME c => drawtext NONE 250 300
                                       (String.str c)
        val _ = MLX.usleep 1000
    in

        loop ()
    end

val _ = loop ()

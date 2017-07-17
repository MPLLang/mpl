structure Primitives =
struct

fun par (f, g) =
    let val gt = Thread.spawn g (Basic.currentPrio ())
        val fr = f ()
        val gr = Thread.sync gt
    in
        (fr, gr)
    end
end

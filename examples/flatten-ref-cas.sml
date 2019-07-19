val n = valOf (Int.fromString (hd (CommandLine.arguments())))
val xrs = List.tabulate (n, fn i => (i, ref (~1)))
val _ = List.app (fn (x, r) => (MLton.Parallel.compareAndSwap r (~1, x); ())) xrs

fun good (x, r) =
  !r = x

val correct = List.foldl (fn (a,b) => a andalso b) true (List.map good xrs)
val _ = print ("good? " ^ (if correct then "yes" else "no") ^ "\n")

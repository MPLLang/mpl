val n = 100

val l = List.tabulate (n, fn i => (Word.fromInt i, ref (0wxbeefface)))
val _ = List.app (fn (x, r) => (MLton.Parallel.compareAndSwap r (0wxbeefface, x); ())) l

val a = Array.tabulate (n, fn i => 0wxbeefface)
fun loop i =
  if i >= n then ()
  else (MLton.Parallel.arrayCompareAndSwap (a, i) (0wxbeefface, Word.fromInt i); loop (i+1))
val _ = loop 0

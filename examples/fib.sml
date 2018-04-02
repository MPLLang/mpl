fun serial n =
  if n <= 1 then n else serial (n-1) + serial (n-2)

fun fib n =
  if n <= 20 then serial n
  else
    let
      val (x,y) = ForkJoin.fork (fn _ => fib (n-1), fn _ => fib (n-2))
    in
      x + y
    end

val result = fib 39
val _ = print (Int.toString result ^ "\n")

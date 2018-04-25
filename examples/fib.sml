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

val t0 = Time.now ()
val result = fib 39
val t1 = Time.now ()
val _ = print (Int.toString result ^ "\n")
val _ = print (LargeInt.toString (Time.toMilliseconds (Time.- (t1, t0))) ^ " ms\n")

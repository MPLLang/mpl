fun fib n = 
  if n <= 2 then
    n
  else
    let 
      val (x,y) = ForkJoin.fork (fn () => fib (n-1), fn () => fib (n-2))
    in 
      x + y
    end

val result = fib 39
val _ = print (Int.toString result ^ "\n")

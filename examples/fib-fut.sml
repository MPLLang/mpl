val size = 39
val grain = 20
val par = ForkJoin.fork

(* implemented by schedulers/spoonhower *)
structure Future = FutureSuspend

fun par (f, g) =
  let
    val gfut = Future.future g
    val fr = f ()
    val gr = Future.touch gfut
  in
    (fr, gr)
  end

fun fastfib n =
  let
    fun iter prev curr remaining =
      if remaining = 0
      then curr
      else iter curr (prev + curr) (remaining-1)
  in
    if n = 0 then 0
    else iter 0 1 (n-1)
  end

fun fib n =
  if n <= 1 then n else fib (n-1) + fib (n-2)

fun parfib n =
  if n <= grain then fib n
  else
    let
      val (x,y) = par (fn _ => parfib (n-1), fn _ => parfib (n-2))
    in
      x + y
    end

val t0 = Time.now ()
val result = parfib size
val t1 = Time.now ()

val _ = print (LargeInt.toString (Time.toMilliseconds (Time.- (t1, t0))) ^ " ms\n")

val desired = fastfib size
val _ = print (if result = desired
               then "correct\n"
               else "incorrect. got " ^ Int.toString result ^
                    " but should be " ^ Int.toString desired ^ "\n")

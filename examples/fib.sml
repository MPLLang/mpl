fun sfib n =
  if n <= 1 then n else sfib (n-1) + sfib (n-2)

fun fib n =
  if n <= 20 then sfib n
  else
    let
      val (x,y) = ForkJoin.fork (fn _ => fib (n-1), fn _ => fib (n-2))
    in
      x + y
    end

val nstr = List.hd (CommandLine.arguments ()) handle _ => "39"
val n =
  case Int.fromString nstr of
    SOME x => x
  | NONE => raise Fail ("cannot parse integer from " ^ nstr)

val _ = print ("fib " ^ Int.toString n ^ "\n")

val t0 = Time.now ()
val result = fib n
val t1 = Time.now ()

val _ = print (Int.toString result ^ "\n")
val _ = print (LargeInt.toString (Time.toMilliseconds (Time.- (t1, t0))) ^ " ms\n")

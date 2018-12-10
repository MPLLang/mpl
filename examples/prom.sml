val n =
  case Int.fromString (List.hd (CommandLine.arguments ())) of
    NONE => 5
  | SOME n => n

val ("hello", "world") = ForkJoin.fork (fn () => "hello", fn () => "world")

val a = Array.array (1000, [])

fun prom () =
  let
    val _ = print "left side\n"
    (* force accurate allocation *)
    val v = Vector.tabulate (1000, fn i => if i = 42 then List.tabulate (n, fn i => i) else [])
  in
    ( print "promoting...\n"
    ; Array.update (a, 42, Vector.sub (v, 42))
    )
  end

val _ = ForkJoin.fork ( fn () => (print "left side\n"; prom ())
                      , fn () => print "right side\n"
                      )

val _ =
  if List.length (Array.sub (a, 42)) = n then
    print "success\n"
  else
    print "failure\n"

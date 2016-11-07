open IO
(* val fork = MLton.Parallel.ForkJoin.fork *)

val start = Time.now ()

val stdin = ref (TextIO.getInstream TextIO.stdIn)

fun inputLine () = (* TextIO.inputLine TextIO.stdIn *)
    let fun iL_int line =
            case IO.input1 (!stdin) of
                NONE => NONE
              | SOME (c, is') =>
                (stdin := is';
                 if c = #"\n" then
                     SOME line
                 else
                     iL_int (line ^ (str c)))
    in
        iL_int ""
    end

fun fib n =
    if n <= 1 then 1
    else
        let
            val fork = if n <= 20 then
                           (fn (f1, f2) => (f1 (), f2 ()))
                       else
                           (* (print ("fib" ^ (Int.toString n) ^ "\n"); *)
                           MLton.Parallel.ForkJoin.fork
            val (a, b) = fork (fn () => fib (n - 1),
                               fn () => fib (n - 2))
        in
            (a + b)
            handle Overflow =>
                   OS.Process.exit OS.Process.success
        end

fun fibdo n =
    let val f = fib n
        val _ = print ("fib(" ^ (Int.toString n) ^ ") = " ^ (Int.toString f) ^ "\n")
        val finish = Time.now ()
        val diff = Time.-(finish, start)
        val diffi = LargeInt.toInt (Time.toMilliseconds diff)
    in
        print ("exectime " ^ (Int.toString diffi) ^ "\n");
        OS.Process.exit OS.Process.success
    end

fun inploop () =
    (print "starting inploop\n";
    case inputLine () of
        NONE => OS.Process.exit OS.Process.success
      | SOME l =>
        (print ("Hi, " ^ l ^ "\n");
         inploop ()))

(*val _ = fibdo 36 *)
val _ = print "Starting\n"
val _ = MLton.Parallel.ForkJoin.fork ((fn () => fibdo 45), inploop)
(* val _ = loop fibs fibs *)

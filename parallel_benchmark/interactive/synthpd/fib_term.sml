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

fun seqfib n =
    if n <= 1 then 1
    else
        let
            val (a, b) = (seqfib (n - 1), seqfib (n - 2))
        in
            (a + b)
            (* handle Overflow =>
                   OS.Process.exit OS.Process.success *)
        end

fun fib n =
    if n <= 1 then 1
    else if n <= 20 then seqfib n
    else
        let
(*
            val _ = if n >= 39 then
                        print ("fib " ^ (Int.toString n) ^ " on " ^
                               (Int.toString
                                    (MLton.Parallel.Basic.processorNumber ()))
                               ^ "\n")
                    else ()
*)
            val fork = MLton.Parallel.ForkJoin.fork
            val (a, b) = fork (fn () => fib (n - 1),
                               fn () => fib (n - 2))
        in
            (a + b)
            (* handle Overflow =>
                   OS.Process.exit OS.Process.success *)
        end

fun fibdo n =
    let val _ = print "starting fib\n"
        val f = fib n
        val _ = print ("fib(" ^ (Int.toString n) ^ ") = " ^ (Int.toString f) ^ "\n")
        val finish = Time.now ()
        val diff = Time.-(finish, start)
        val diffi = LargeInt.toInt (Time.toMilliseconds diff)
    in
        print ("exectime " ^ (aIntToString diffi) ^ "\n");
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
val _ = print "Starting fib_term\n"
val _ = MLton.Parallel.ForkJoin.fork ((fn () => MLton.Parallel.FutureFGBG.fg inploop),
                                      (fn () => fibdo 43)
                                      )
(* val _ = loop fibs fibs *)

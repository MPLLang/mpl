open IO
(* val fork = MLton.Parallel.ForkJoin.fork *)

val start = Time.now ()

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
    let val f = fib n
        val _ = print ("fib(" ^ (Int.toString n) ^ ") = " ^ (Int.toString f) ^ "\n")
        val finish = Time.now ()
        val diff = Time.-(finish, start)
        val diffi = LargeInt.toInt (Time.toMilliseconds diff)
    in
        print ("exectime " ^ (Int.toString diffi) ^ "\n");
        OS.Process.exit OS.Process.success
    end

val _ = fibdo 45

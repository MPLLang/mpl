val stdin = ref (TextIO.getInstream TextIO.stdIn)

fun inputLine () =
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
    else let val (r1, r2) = (seqfib (n-1), seqfib (n-2))
         in r1 + r2
         end

(* Compute and print Fibonacci of $n$  *)
fun fib n () =
  let fun fib' n () =
      if n <= 1 then 1
      else if n <= 20 then seqfib n
      else let val (r1, r2) = MLton.Parallel.ForkJoin.fork (fib' (n-1), fib' (n-2))
           in r1 + r2
           end
  in
      print "starting fib\n";
      print ((Int.toString (fib' n ())) ^ "\n")
  end

(* Read request from user and compute if needed. *)
fun fib_server () =
  let fun loop futures =
        let val _ = print "Enter a number.\n"
            val request = inputLine ()
        in case request of
             NONE => List.app MLton.Parallel.FutureSuspend.touch futures
           | SOME s =>
             (case Int.fromString s of
                  NONE => loop futures
                | SOME n =>
                  let val _ = print ("Computing fib(" ^ (Int.toString n) ^ ")\n")
                      val f = MLton.Parallel.FutureSuspend.futureLat (false,
                                                                      fib n)
                  in loop (f::futures) end)
        end
  in loop []
  end

val _ = print "Starting\n"

val _ = MLton.Parallel.Basic.event fib_server

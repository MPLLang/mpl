fun die message =
    (TextIO.output (TextIO.stdErr, message ^ "\n");
     OS.Process.exit OS.Process.failure)

fun getStringOption option args =
    (case args
      of arg1 :: arg2 :: args =>
	 if String.compare (arg1, option) = EQUAL
         then SOME arg2
	 else getStringOption option (arg2 :: args)
       | _ => NONE
    (* end case *))

fun getIntOption (option) (args) : int option =
    (case getStringOption option args
      of SOME arg => Int.fromString arg
       | NONE => NONE)

fun serialFib granularity n =
    if n <= 1
    then 1
    else
        let
            val (a, b) = (serialFib granularity (n - 1),
                          serialFib granularity (n - 2))
        in
            a + b
        end

fun parallelFib granularity n =
    if n <= 30
    then serialFib granularity n
    else
        let
            val (a, b) = MLton.Parallel.ForkJoin.fork
                             (fn () => parallelFib granularity (n - 1),
                              fn () => parallelFib granularity (n - 2))
        in
            a + b
        end

fun iterFib n =
    let
      fun loop i x y =
          if i > n-2 then x
          else loop (i+1) (x+y) x
    in
      loop 0 1 1
    end

fun main args =
    let
        val fib = case getStringOption "-mode" args
                   of SOME "parallel" => parallelFib
                    | SOME "serial" => serialFib
                    | SOME mode => die ("Unknown mode \"" ^ mode ^ "\"")
                    | NONE => serialFib

        val granularity = case getIntOption "-granularity" args
                           of SOME g => g
                            | NONE => 20

        val input = getIntOption "-input" args
    in
        case input
         of NONE =>
            let
                fun run n =
                    let
                        val start = Time.now ()
                        val r = fib granularity n
                        val diff = Time.- (Time.now (), start)
                        val r' = iterFib n
                        val () = if r <> r' then print ("expected " ^ Int.toString r' ^
                                                        " but got " ^ Int.toString r ^ " instead!\n")
                                 else ()
                    in
                        print (concat [" Fib(",
                                       Int.toString n, ")\t",
                                       LargeInt.toString (Time.toMilliseconds diff),
                                       " ms\n"])
                    end
            in
                app run [1,5,10,15,20,25,30,35,40]
            end
          | SOME n =>
            let
                val r = fib granularity n
            in
                print ("finished with " ^ (Int.toString r) ^ "\n")
            end
    end

val _ = main (CommandLine.arguments ())

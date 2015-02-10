fun fib fork n=
    if n <= 1 then 1
    else
      let
        val (a, b) = fork (fn () => fib fork (n - 1),
                           fn () => fib fork (n - 2))
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

fun main fork args =
    if null args then
      let
        fun run n =
            let
              val start = Time.now ()
              val r = fib fork n
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
    else
      let
        val n = valOf (Int.fromString (hd (args)))
        val r = fib fork n
        val r' = iterFib n
        val () = if r <> r' then print ("expected " ^ Int.toString r' ^
                                        " but got " ^ Int.toString r ^ " instead!\n")
                 else ()
      in
        print ("finished with " ^ (Int.toString r) ^ "\n")
      end

val () =
    case CommandLine.arguments ()
     of "serial"::args => (print "Running in serial mode\n";
                           main (fn (f, g) => (f (), g ())) args)
      | "parallel"::args => (print "Running in parallel mode\n";
                             main MLton.Parallel.ForkJoin.fork args)
      | argument::_ => (TextIO.output (TextIO.stdErr, "Invalid fork-mode \"" ^
                                                      argument ^
                                                      "\"\n");
                        OS.Process.exit OS.Process.failure)
      | [] => (TextIO.output (TextIO.stdErr, "Fork mode unspecified!\n");
               OS.Process.exit OS.Process.failure)

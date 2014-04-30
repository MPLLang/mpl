
val fork = MLton.Parallel.ForkJoin.fork
val policyName = MLton.Parallel.Basic.policyName
(*
fun fork (f, g) = (f (), g ()) 
val policyName = "serial"
*)

fun fib n =
    if n <= 1 then 1
    else
      let 
        val (a, b) = fork (fn () => fib (n - 1),
                           fn () => fib (n - 2))
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

val () = 
    if null (CommandLine.arguments ()) then
      let
        fun run n = 
            let 
              val start = Time.now ()
              val r = fib n
              val diff = Time.- (Time.now (), start)
              val r' = iterFib n
              val () = if r <> r' then print ("expected " ^ Int.toString r' ^
                                              " but got " ^ Int.toString r ^ " instead!\n")
                       else ()
            in
              print (concat [policyName, " Fib(",
                             Int.toString n, ")\t", 
                             LargeInt.toString (Time.toMilliseconds diff),
                             " ms\n"])
            end
      in
        app run [1,5,10,15,20,25,30,35,40]
      end
    else
      let
        val n = valOf (Int.fromString (hd (CommandLine.arguments ())))
        val r = fib n
        val r' = iterFib n
        val () = if r <> r' then print ("expected " ^ Int.toString r' ^
                                        " but got " ^ Int.toString r ^ " instead!\n")
                 else ()
      in
        print ("finished with " ^ (Int.toString r) ^ "\n")
      end



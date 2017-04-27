val SOME FILES = Int.fromString (List.nth(CommandLine.arguments (), 1))

fun fib n =
    if n <= 1 then 1
    else
        let
            val fork = if n <= 20 then
                           (fn (f1, f2) => (f1 (), f2 ()))
                       else
                           MLton.Parallel.ForkJoin.fork
            val (a, b) = fork (fn () => fib (n - 1),
                               fn () => fib (n - 2))
        in
            (a + b)
        end

fun forkn n f =
    let fun fork_int n i =
            if n = 0 then 0 else
            if n = 1 then f i else
            let val left = Int.div (n, 2)
                val right = n - left
                val (l, r) = MLton.Parallel.ForkJoin.fork
                    ((fn () => fork_int left i),
                     (fn () => fork_int right (i + left)))
            in
                (l + r) mod 1000000000
            end
    in
        fork_int n 0
    end

val start = Time.now ()
val result = forkn FILES (fn _ => fib 35)
val finish = Time.now ()
val diff = Time.-(finish, start)
val diffi = LargeInt.toInt (Time.toMilliseconds diff)
val _ = print ("result: " ^ (Int.toString result) ^ "\n")
val _ = print ("exectime " ^ (Int.toString diffi) ^ "\n")

val SOME FILES = Int.fromString (List.nth(CommandLine.arguments (), 1))

fun fib n =
    if n <= 1 then 1
    else (fib (n - 1)) + (fib (n - 2))

val start = Time.now ()
val result = List.foldl (fn (_, b) => ((fib 35) + b) mod 1000000000) 0
                        (List.tabulate (FILES, fn _ => 0))
                        (* streams *)
val finish = Time.now ()
val diff = Time.-(finish, start)
val diffi = LargeInt.toInt (Time.toMilliseconds diff)
val _ = print ("result: " ^ (Int.toString result) ^ "\n")
val _ = print ("exectime " ^ (Int.toString diffi) ^ "\n")

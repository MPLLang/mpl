val SOME FILES = Int.fromString (List.nth(CommandLine.arguments (), 1))
val SOME DELAY = Int.fromString (List.nth(CommandLine.arguments (), 3))
val LINES_PER_FILE = 1

fun fib n =
    if n <= 1 then 1
    else (fib (n - 1)) + (fib (n - 2))

(* val _ = forkn FILES generate *)
(*
val streams = List.tabulate
                  (FILES,
                   fn i => TextIO.openIn ("files/" ^ (Int.toString i)))
*)

fun sleep ms =
    let val start = Time.now ()
        fun loop () =
            let val now = Time.now ()
            in
                if Time.toMicroseconds (Time.- (now, start)) > ms then
                    ()
                else
                    loop ()
            end
    in
        loop ()
    end

fun inputLine s =
    (sleep (IntInf.fromInt DELAY); SOME "30")

fun doone _ =
    let fun f i =
            case inputLine () of
                SOME s =>
                (case Int.fromString s of
                     SOME n => fib n
                   | NONE => 0)
                | NONE => 0
        fun g (a, b) = ((f a) + b) mod 1000000000
        val idxs = List.tabulate (LINES_PER_FILE, fn i => i)
    in
        List.foldl g 0 idxs
    end

val start = Time.now ()
val result = List.foldl (fn (a, b) => ((doone a) + b) mod 1000000000) 0
                        (List.tabulate (FILES, fn _ => 0))
                        (* streams *)
val finish = Time.now ()
val diff = Time.-(finish, start)
val diffi = LargeInt.toInt (Time.toMilliseconds diff)
val _ = print ("result: " ^ (Int.toString result) ^ "\n")
val _ = print ("exectime " ^ (Int.toString diffi) ^ "\n")

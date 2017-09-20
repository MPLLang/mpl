val SOME FILES = Int.fromString (List.nth(CommandLine.arguments (), 1))
val SOME DELAY = Int.fromString (List.nth(CommandLine.arguments (), 3))
val LINES_PER_FILE = 1

structure B = Basic

fun inputLine () : string option =
    let val start = Time.now ()
        fun f () = (Time.toMicroseconds (Time.-(Time.now (), start))) >
                   IntInf.fromInt DELAY
            (* TextIO.canInput (is, 1) *)
    in
        if f () then SOME "30"
            (* TextIO.inputLine is *)
        else
            (* Performing the input would block *)
            ((* print "false\n"; *)
             B.suspend (fn t => B.addtoio (t, f)); SOME "30")
            (*  TextIO.inputLine is) *)
    end

(*
fun input1 (is: TextIO.instream) : char option =
    let fun f () =
            ((* print "in f\n"; *)
            (case TextIO.canInput (is, 1) of
                 NONE => false
               | SOME _ => true)
            (* before
            print "out f\n" *))
        (* val _ = print ("input1 on " ^ (Int.toString (B.processorNumber ())) ^ "\n") *)
    in
        if f () then
            (print "true\n";
             TextIO.input1 is)
        else
            (* Performing the input would block *)
            (print "false\n";
             B.suspend (fn t => B.addtoio (t, f));
             input1 is)
    end

fun inputLine s =
    let fun iL_int line =
            case input1 s of
                NONE => NONE
              | SOME c =>
                if c = #"\n" then
                     SOME line
                 else
                     iL_int (line ^ (str c))
    in
        iL_int ""
    end
*)

fun fib n =
    if n <= 1 then 1
    else
        let
            val fork = if n <= 25 then
                           (fn (f1, f2) => (f1 (), f2 ()))
                       else
                           ForkJoin.fork
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
                val (l, r) = ForkJoin.fork
                    ((fn () => fork_int left i),
                     (fn () => fork_int right (i + left)))
            in
                (l + r) mod 1000000000
            end
    in
        fork_int n 0
    end

val _ = MLton.Random.srand
            (case MLton.Random.seed () of
                 SOME s => s
               | NONE => 0w23984)

fun generate i =
    let val file = TextIO.openOut ("files/" ^ (Int.toString i))
        fun randInt _ =
            (Word.toInt (Word.mod (MLton.Random.rand (), 0w8))) + 30
        val vals = List.tabulate (LINES_PER_FILE, randInt)
        fun out n = TextIO.output (file, (Int.toString n) ^ "\n")
        val _ = List.map out vals
        val _ = TextIO.closeOut file
    in
       0
    end


(* val _ = forkn FILES generate *)
(*
val streams = Array.tabulate
                  (FILES,
                   fn i => TextIO.openIn ("files/" ^ (Int.toString i)))
*)
fun doone i =
    let (* val rdr = Array.sub (streams, i) *)
        (* val _ = print ("files/" ^ (Int.toString i) ^ "\n") *)
        fun f i =
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
val result = forkn FILES doone
val finish = Time.now ()
val diff = Time.-(finish, start)
val diffi = LargeInt.toInt (Time.toMilliseconds diff)
val _ = print ("result: " ^ (Int.toString result) ^ "\n")
val _ = print ("exectime " ^ (Int.toString diffi) ^ "\n")

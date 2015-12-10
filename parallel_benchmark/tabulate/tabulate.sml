structure AS = ArraySequence

exception Invariant

val fork = MLton.Parallel.ForkJoin.fork

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

fun serialFib n =
    if n <= 1
    then 1
    else
        let
            val (a, b) = (serialFib (n - 1), serialFib (n - 2))
        in
            a + b
        end

fun time f =
    let
        val ts = Time.now ()
        val r = f ()
        val te = Time.now ()
        val elapsed = Time.- (te, ts)
    in
        (r, elapsed)
    end

val timeToString = LargeInt.toString o Time.toMilliseconds
fun formatTimeString (total, gc) = String.concat ["# ",
                                                  timeToString total,
                                                  " ms (",
                                                  timeToString gc,
                                                  " ms in GC)\n"]

fun doit (arraySize : int) : unit =
    let
        val (r, elapsed) = time (fn () => AS.tabulate
                                              (fn i => serialFib (i mod 30))
                                              arraySize)
    in
        print (formatTimeString (elapsed, Time.zeroTime));
        print ((AS.toString Int.toString r) ^ "\n")
    end

fun main (args : string list) : unit =
    let
        val arraySize = case getIntOption "-array-size" args
                         of SOME v => v
                          | NONE => 1000
    in
        print (String.concat ["arraySize: ",
                              Int.toString arraySize,
                              "\n"]);
        doit arraySize
    end

val _ = main (CommandLine.arguments ())

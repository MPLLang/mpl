structure TS = TreeSequence

exception Invariant

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

fun doit (arraySize : int) (granularity : int) (iterations : int) : unit =
    let
        val (a, b) = Primitives.par (fn () => Array.tabulate (10, fn i => i),
                                     fn () => Array.tabulate (10, fn i => i))

        fun loop 1 = TS.tabulate (fn i => serialFib (i mod granularity))
                                 arraySize
          | loop n =
            let
                val k = TS.tabulate (fn i => serialFib (i mod granularity))
                                    arraySize
            in
                loop (n - 1)
            end

        val (r, elapsed) = time (fn () => loop iterations)
    in
        print (formatTimeString (elapsed, Time.zeroTime));
        print ((Int.toString (TS.nth r 0)) ^ "\n")
    end

fun main (args : string list) : unit =
    let
        val arraySize = case getIntOption "-array-size" args
                         of SOME v => v
                          | NONE => 1000

        val granularity = case getIntOption "-granularity" args
                           of SOME v => v
                            | NONE => 30

        val iterations = case getIntOption "-iterations" args
                          of SOME v => v
                           | NONE => 1
    in
        print (String.concat ["arraySize: ",
                              Int.toString arraySize,
                              " granularity: ",
                              Int.toString granularity,
                              " iterations: ",
                              Int.toString iterations,
                              "\n"]);
        doit arraySize granularity iterations
    end

val _ = main (CommandLine.arguments ())

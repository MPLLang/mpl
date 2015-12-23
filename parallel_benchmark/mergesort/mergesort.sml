structure ASG = ArraySequenceG

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

fun doit (arraySize : int) (g : int) : unit =
    let
        val (a, b) = Primitives.par (fn () => ASG.tabulate 1 (fn i => i) 10,
                                     fn () => ASG.tabulate 1 (fn i => i) 10)
        val arr = ASG.tabulate g (fn _ => MLton.Random.rand ()) arraySize
        val (r, elapsed) = time (fn () => ASG.sort g Word.compare arr)
    in
        print (formatTimeString (elapsed, Time.zeroTime));
        print ((Word.toString (ASG.nth r 0)) ^ "\n")
    end

fun main (args : string list) : unit =
    let
        val arraySize = case getIntOption "-array-size" args
                         of SOME v => v
                          | NONE => 1000

        val granularity = case getIntOption "-granularity" args
                         of SOME v => v
                          | NONE => 1000
    in
        print (String.concat ["arraySize: ",
                              Int.toString arraySize,
                              " granularity: ",
                              Int.toString granularity,
                              "\n"]);
        doit arraySize granularity
    end

val _ = main (CommandLine.arguments ())

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
    if n <= (Word.fromInt 1)
    then (Word.fromInt 1)
    else
        let
            val (a, b) = (serialFib (n - (Word.fromInt 1)),
                          serialFib (n - (Word.fromInt 2)))
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

fun doit (arraySize : int) (granularity : int) : unit =
    let
        val granularity = Word.fromInt granularity
        val arr = TS.tabulate (fn _ => MLton.Random.rand ()) arraySize

        val (r, elapsed) =
            time (fn () => TS.filter (fn x =>
                                         let
                                             val fib =
                                                 serialFib (x mod granularity)
                                         in
                                             (fib mod (Word.fromInt 2)) =
                                             (Word.fromInt 0)
                                         end)
                                     arr)
    in
        print (formatTimeString (elapsed, Time.zeroTime));
        print (String.concat [Word.toString (TS.nth r 0),
                              "\n"])
    end

fun main (args : string list) : unit =
    let
        val arraySize = case getIntOption "-array-size" args
                         of SOME v => v
                          | NONE => 1000

        val granularity = case getIntOption "-granularity" args
                           of SOME v => v
                            | NONE => 30
    in
        print (String.concat ["arraySize: ",
                              Int.toString arraySize,
                              " granularity: ",
                              Int.toString granularity,
                              "\n"]);
        doit arraySize granularity
    end

val _ = main (CommandLine.arguments ())

structure TSG = TreeSequenceG

exception Invariant

val fork = Primitives.par

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

fun qsort g cmp arr =
    let
        val size = TSG.length arr
    in
        if 0 = size
        then TSG.empty ()
        else if 1 = size
        then arr
        else
            let
                val pivot = TSG.nth arr 0
                val arr = TSG.drop g arr 1

                fun filterL () = TSG.filter
                                     g
                                     (fn x => case cmp (x, pivot)
                                               of EQUAL => true
                                                | LESS => true
                                                | GREATER => false)
                                     arr

                fun filterR () = TSG.filter
                                     g
                                     (fn x => case cmp (x, pivot)
                                               of EQUAL => false
                                                | LESS => false
                                                | GREATER => true)
                                     arr

                val (L, R) = if size <= g
                             then (filterL (), filterR ())
                             else fork (filterL, filterR)
            in
                TSG.append g (qsort g cmp L,
                              TSG.append g (TSG.singleton pivot,
                                            qsort g cmp R))
            end
    end

fun doit (arraySize : int) (granularity : int) : unit =
    let
        val arr = TSG.tabulate granularity
                               (fn _ => MLton.Random.rand ())
                               arraySize
        val (r, elapsed) = time (fn () => qsort granularity Word.compare arr)
    in
        print (formatTimeString (elapsed, Time.zeroTime));
        print ((Word.toString (TSG.nth r 0)) ^ "\n")
    end

fun main (args : string list) : unit =
    let
        val arraySize = case getIntOption "-array-size" args
                         of SOME v => v
                          | NONE => 1000

        val granularity = case getIntOption "-granularity" args
                           of SOME v => v
                            | NONE => 1
    in
        print (String.concat ["arraySize: ",
                              Int.toString arraySize,
                              " granularity: ",
                              Int.toString granularity,
                              "\n"]);
        doit arraySize granularity
    end

val _ = main (CommandLine.arguments ())

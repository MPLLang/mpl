structure TS = TreeSequence

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

fun qsort cmp arr =
    let
        val size = TS.length arr
    in
        if 0 = size
        then TS.empty ()
        else if 1 = size
        then arr
        else
            let
                val pivot = TS.nth arr 0
                val arr = TS.drop arr 1
                val (L, R) = fork (fn () => TS.filter
                                                (fn x => case cmp (x, pivot)
                                                          of EQUAL => true
                                                           | LESS => true
                                                           | GREATER => false)
                                                arr,
                                   fn () => TS.filter
                                                (fn x => case cmp (x, pivot)
                                                          of EQUAL => false
                                                           | LESS => false
                                                           | GREATER => true)
                                                arr)
            in
                TS.append (qsort cmp L,
                           TS.append (TS.singleton pivot,
                                      qsort cmp R))
            end
    end

fun doit (arraySize : int) : unit =
    let
        val arr = TS.tabulate (fn _ => MLton.Random.rand ()) arraySize
        val (r, elapsed) = time (fn () => qsort Word.compare arr)
    in
        print (formatTimeString (elapsed, Time.zeroTime));
        print ((Word.toString (TS.nth r 0)) ^ "\n")
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

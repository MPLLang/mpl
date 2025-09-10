structure CLA = CommandLineArgs
(* structure Seq = ArraySequence *)

val str = CLA.parseString "str" ""
(* val algo = CLA.parseString "algo" "" *)
val check = CLA.parseFlag "check"
val benchmark = CLA.parseFlag "benchmark"
val benchSize = CLA.parseInt "N" 10000000
val printResult = CLA.parseFlag "print"
val filename = CLA.parseString "file" ""
val rep = case (Int.fromString (CLA.parseString "repeat" "1")) of
               SOME(a) => a
             | NONE => 1

fun load filename =
  ReadFile.contents filename
  (* let val str = Util.readFile filename
  in CharVector.tabulate (Array.length str, fn i => Array.sub (str, i)) end *)

val str = if filename <> "" then load filename else str

val maker =
  PrefixDoublingSuffixArray.makeSuffixArray
  (*if algo = "DC3" then DC3SuffixArray.makeSuffixArray
  else if algo = "PD" then PrefixDoublingSuffixArray.makeSuffixArray
  else if algo = "BF" then BruteForceSuffixArray.makeSuffixArray
  else Util.exit "Unknown algorithm" *)

(* val _ = MLton.Rusage.measureGC true *)

(* fun totalGCTime () =
  let
    val n = Primitives.numberOfProcessors
    val time = ref Time.zeroTime
    val () = Primitives.for (0, n) (fn i =>
      (time := Time.+ (!time, Primitives.localGCTimeOfProc i);
       time := Time.+ (!time, Primitives.promoTimeOfProc i))
    )
  in
    !time
  end *)

fun runTrial str =
  let
    (* val _ = MLton.GC.collect () *)
    (* val gcTime0 = totalGCTime () *)
    val t0 = Time.now ()
    val result = maker str
    val t1 = Time.now ()
    (* val gcTime1 = totalGCTime () *)
    val elapsed = Time.toMilliseconds (Time.- (t1, t0))
    (* val gcTimeTotal = Time.toMilliseconds (Time.- (gcTime1, gcTime0)) *)
    val gcTimeTotal = 0
    val () = print ("GC: " ^ LargeInt.toString gcTimeTotal ^ " ms\t"
      ^ "Total: " ^ LargeInt.toString elapsed ^ " ms\n")
  in
    result
  end

val result = if str <> "" then runTrial str else Seq.empty ()

val () =
  if printResult then
    Util.for (0, Seq.length result) (fn i =>
      print (Int.toString (Seq.nth result i) ^ "\n")
    )
  else ()

fun checker str result =
  let
    val answer = BruteForceSuffixArray.makeSuffixArray str
  in
    if Seq.equal (op =) (result, answer)
      then print "Correct\n"
      else print "Incorrect\n"
  end

val _ = if str <> "" andalso check then checker str result else ()

fun runBenchmark () =
  let
    fun randChar seed = Char.chr (Util.hash seed mod 256)
    fun randString n = CharVector.tabulate (n, randChar)
    val _ = print ("N " ^ Int.toString benchSize ^ "\n")
    val (str, tm1) = Util.getTime (fn _ => randString benchSize)
    val _ = print ("generated input in " ^ Time.fmt 4 tm1 ^ "s\n")

    val result = Benchmark.run "running suffix array" (fn _ => maker str)

    val _ =
      if not check then () else
      let val (_, tm) =
            Util.getTime (fn _ => if check then checker str result else ())
      in print ("checking took " ^ Time.fmt 4 tm ^ "s\n")
      end
  in
    ()
  end

val _ = if benchmark then runBenchmark () else ()

val _ =
  if benchmark then GCStats.report ()
  else ()

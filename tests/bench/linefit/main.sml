structure CLA = CommandLineArgs
structure Seq = ArraySequence

(* chosen by subdirectory *)
structure LF = MkLineFit(OldDelayedSeq)

val n = CLA.parseInt "n" (1000 * 1000 * 100)
val doCheck = CLA.parseFlag "check"

fun gen i =
  (Real.fromInt i, Real.fromInt i)

val input =
  Seq.tabulate gen n

fun task () =
  LF.linefit input

fun check result =
  if not doCheck then () else
  let
    val (a, b) = result
    val correct =
      Real.< (Real.abs a      , 0.000001) andalso
      Real.< (Real.abs (b-1.0), 0.000001)
  in
    if correct then
      print ("correct? yes\n")
    else
      print ("correct? no\n")
  end

val result = Benchmark.run "linefit" task
val _ = check result

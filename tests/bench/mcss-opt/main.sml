structure CLA = CommandLineArgs
structure Seq = ArraySequence

structure M = MCSS

val n = CLA.parseInt "n" (1000 * 1000 * 100)

fun gen i =
  Real.fromInt (Word64.toInt (Word64.mod (Util.hash64 (Word64.fromInt i), 0w1000)) - 500) / 500.0

val input =
  Seq.tabulate gen n

fun task () =
  M.mcss input

val result = Benchmark.run "mcss" task
val _ = print ("result " ^ Real.toString result ^ "\n")

val _ = print ("input " ^ Util.summarizeArraySlice 12 Real.toString input ^ "\n")

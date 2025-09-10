structure CLA = CommandLineArgs

val n = CLA.parseInt "N" (100 * 1000 * 1000)
val _ = print ("N " ^ Int.toString n ^ "\n")

val _ = print ("generating " ^ Int.toString n ^ " random integers\n")

fun elem i =
  Word64.toInt (Word64.mod (Util.hash64 (Word64.fromInt i), Word64.fromInt n))
val input = ArraySlice.full (SeqBasis.tabulate 10000 (0, n) elem)

val result =
  Benchmark.run "running mergesort" (fn _ => Mergesort.sort Int.compare input)

val _ = print ("input " ^ Util.summarizeArraySlice 8 Int.toString input ^ "\n")
val _ = print ("result " ^ Util.summarizeArraySlice 8 Int.toString result ^ "\n")


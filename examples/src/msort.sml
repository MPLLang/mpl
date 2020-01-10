val n = CommandLineArgs.parseInt "N" (100*1000*1000)

val _ = print ("generating " ^ Int.toString n ^ " random integers\n")

fun elem i =
  Word64.toInt (Word64.mod (Util.hash64 (Word64.fromInt i), Word64.fromInt n))
val input = ArraySlice.full (SeqBasis.tabulate 10000 (0, n) elem)

val _ = print ("sorting\n")

val t0 = Time.now ()
val result = Mergesort.sort Int.compare input
val t1 = Time.now ()

val _ = print ("finished in " ^ Time.fmt 4 (Time.- (t1, t0)) ^ "s\n")

val _ = print ("result " ^ Util.summarizeArraySlice 8 Int.toString result ^ "\n")

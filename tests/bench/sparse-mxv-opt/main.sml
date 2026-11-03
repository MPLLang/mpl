structure CLA = CommandLineArgs
structure Seq = ArraySequence
structure DS = DelayedSeq

structure M = SparseMxV

val n = CLA.parseInt "n" (1000 * 1000 * 100)
val doCheck = CLA.parseFlag "check"

val rowLen = 100
val numRows = n div rowLen
val vec = Seq.tabulate (fn i => 1.0) numRows
fun gen i j =
  ((Util.hash (i * rowLen + j) mod numRows), 1.0)
val mat = Seq.tabulate (fn i => Seq.tabulate (gen i) rowLen) numRows

fun task () =
  M.sparseMxV mat vec

fun check result =
  if not doCheck then () else
  let
    fun closeEnough (a, b) = Real.< (Real.abs (a - b), 0.000001)
    val correct =
      DS.reduce (fn (a, b) => a andalso b) true
      (DS.tabulate
        (fn i => closeEnough (Seq.nth result i, Real.fromInt rowLen))
        numRows)
  in
    if correct then
      print ("correct? yes\n")
    else
      print ("correct? no\n")
  end

val result = Benchmark.run "sparse-mxv" task
val _ = check result

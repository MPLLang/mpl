structure CLA = CommandLineArgs
structure Seq = ArraySequence

structure Parens = MkParens(DelayedSeq)

val n = CLA.parseInt "n" (1000 * 1000 * 100)
val doCheck = CLA.parseFlag "check"

(* makes the sequence `()()()...` *)
fun gen i =
  if i mod 2 = 0 then Parens.Left else Parens.Right

val input = Seq.tabulate gen n

fun task () =
  Parens.parenMatch input

fun check result =
  if not doCheck then () else
  let
    val correct =
      (n mod 2 = 0 andalso result)
      orelse
      (n mod 2 = 1 andalso not result)
  in
    if correct then
      print ("correct? yes\n")
    else
      print ("correct? no\n")
  end

val result = Benchmark.run "parens" task
val _ = check result

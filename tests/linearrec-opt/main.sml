structure CLA = CommandLineArgs
structure Seq = ArraySequence

structure L = LinearRec

val n = CLA.parseInt "n" (1000 * 1000 * 100)

val _ = print ("n " ^ Int.toString n ^ "\n")

(* fun gen i =
  Real.fromInt ((Util.hash i) mod 1000 - 500) / 500.0 *)

val input =
  Seq.tabulate (fn i => (1.0, 1.0)) n

fun task () =
  L.linearRec input

val result = Benchmark.run "linear recurrence" task
val x = Seq.nth result (n-1)
val _ = print ("result " ^ Real.toString x ^ "\n")

structure CLA = CommandLineArgs

structure IntegrateAS = MkIntegrate(Seq)
structure IntegrateDS = MkIntegrate(DelayedSeq)

val n = CLA.parseInt "n" (1000 * 1000 * 100)
val impl = CLA.parseString "impl" "delayed-seq"
val doCheck = CLA.parseFlag "check"

val _ = print ("n      " ^ Int.toString n ^ "\n")
val _ = print ("impl   " ^ impl ^ "\n")
val _ = print ("check? " ^ (if doCheck then "yes" else "no") ^ "\n")

val range = (1.0, 1000.0)

val (f, correctAnswer) =
  (fn x => Math.sqrt (1.0 / x), 61.245553203367586639977870888654371)
(* (fn x => 1.0 / x,             6.9077552789821370520539743640530926) *)
(* (fn x => Math.sin (1.0 / x),  6.8264726355070070694576392250122662) *)

val task =
  case impl of
    "delayed-seq" => (fn () => IntegrateDS.integrate f range n)
  | "array-seq" => (fn () => IntegrateAS.integrate f range n)
  | _ =>
      Util.die
        ("unknown impl: " ^ impl ^ "; options are: delayed-seq, array-seq")

fun check result =
  if Util.closeEnough (result, correctAnswer) then
    print ("correct? yes\n")
  else
    print
      ("correct? no (error = "
       ^ Real.toString (Real.abs (result - correctAnswer)) ^ ")\n")

val result = Benchmark.run "integrate" task
val _ = print ("result " ^ Real.toString result ^ "\n")
val _ = check result

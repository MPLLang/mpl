structure CLA = CommandLineArgs

val n = CLA.parseInt "n" (1000 * 1000 * 100)
val doCheck = CLA.parseFlag "check"

val range = (1.0, 1000.0)

val (f, correctAnswer) =
  (fn x => Math.sqrt (1.0 / x), 61.245553203367586639977870888654371)
  (* (fn x => 1.0 / x,             6.9077552789821370520539743640530926) *)
  (* (fn x => Math.sin (1.0 / x),  6.8264726355070070694576392250122662) *)

fun task () =
  Integrate.integrate f range n

fun check result =
  if Util.closeEnough (result, correctAnswer) then
    print ("correct? yes\n")
  else
    print ("correct? no (error = "
           ^ Real.toString (Real.abs (result - correctAnswer))
           ^ ")\n")

val result = Benchmark.run "integrate" task
val _ = print ("result " ^ Real.toString result ^ "\n")
val _ = check result

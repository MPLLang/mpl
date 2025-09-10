structure CLA = CommandLineArgs

val n = CLA.parseInt "N" 1024
val _ =
  if Util.boundPow2 n = n then ()
  else Util.die "sidelength N must be a power of two"

val _ = print ("generating matrices of sidelength " ^ Int.toString n ^ "\n")
val input = TreeMatrix.tabulate n (fn (i, j) => 1.0)

val result =
  Benchmark.run "multiplying" (fn _ => TreeMatrix.multiply (input, input))

val doCheck = CLA.parseFlag "check"
val _ =
  if not doCheck then () else
  let
    val stuff = TreeMatrix.flatten result
    val correct =
      Array.length stuff = n * n
      andalso
      SeqBasis.reduce 1000 (fn (a, b) => a andalso b) true (0, Array.length stuff)
      (fn i => Util.closeEnough (Array.sub (stuff, i), Real.fromInt n))
  in
    print ("correct? ");
    if correct then print "yes" else print "no";
    print "\n"
  end


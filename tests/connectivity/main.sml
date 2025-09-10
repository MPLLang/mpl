structure CLA = CommandLineArgs
structure G = AdjacencyGraph(Int)

val source = CLA.parseInt "source" 0
val doCheck = CLA.parseFlag "check"

(*
val N = CLA.parseInt "N" 10000000
val D = CLA.parseInt "D" 10

val (graph, tm) = Util.getTime (fn _ => G.randSymmGraph N D)
val _ = print ("generated graph in " ^ Time.fmt 4 tm ^ "s\n")
val _ = print ("num vertices: " ^ Int.toString (G.numVertices graph) ^ "\n")
val _ = print ("num edges: " ^ Int.toString (G.numEdges graph) ^ "\n")
*)

val filename =
  case CLA.positional () of
    [x] => x
  | _ => Util.die "missing filename"

val (graph, tm) = Util.getTime (fn _ => G.parseFile filename)
val _ = print ("num vertices: " ^ Int.toString (G.numVertices graph) ^ "\n")
val _ = print ("num edges: " ^ Int.toString (G.numEdges graph) ^ "\n")

val (_, tm) = Util.getTime (fn _ =>
  if G.parityCheck graph then ()
  else TextIO.output (TextIO.stdErr,
    "WARNING: parity check failed; graph might not be symmetric " ^
    "or might have duplicate- or self-edges\n"))
val _ = print ("parity check in " ^ Time.fmt 4 tm ^ "s\n")


val b = (CommandLineArgs.parseReal "b" 0.3)

val P = Benchmark.run "running connectivity: " (fn _ => Connectivity.connectivity graph b)
(* val _ = print ("num-triangles = " ^ (Int.toString P) ^ "\n") *)
(* val _ = LDD.check_ldd graph (#1 P) (#2 P) *)
(* val _ = Benchmark.run "running connectivity" (fn _ => LDD.connectivity graph b) *)
(*
val numVisited =
  SeqBasis.reduce 10000 op+ 0 (0, Seq.length P)
    (fn i => if Seq.nth P i >= 0 then 1 else 0)
val _ = print ("visited " ^ Int.toString numVisited ^ "\n")

fun numHops P hops v =
  if hops > Seq.length P then ~2
  else if Seq.nth P v = ~1 then ~1
  else if Seq.nth P v = v then hops
  else numHops P (hops+1) (Seq.nth P v)

val maxHops =
  SeqBasis.reduce 100 Int.max ~3 (0, G.numVertices graph) (numHops P 0)
val _ = print ("max dist " ^ Int.toString maxHops ^ "\n")

fun check () =
  let
    val (P', serialTime) =
      Util.getTime (fn _ => SerialBFS.bfs graph source)

    val correct =
      Seq.length P = Seq.length P'
      andalso
      SeqBasis.reduce 10000 (fn (a, b) => a andalso b) true (0, Seq.length P)
        (fn i => numHops P 0 i = numHops P' 0 i)
  in
    print ("serial finished in " ^ Time.fmt 4 serialTime ^ "s\n");
    print ("correct? " ^ (if correct then "yes" else "no") ^ "\n")
  end

val _ = if doCheck then check () else ()

val _ = GCStats.report () *)

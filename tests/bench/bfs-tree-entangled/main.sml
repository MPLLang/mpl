structure CLA = CommandLineArgs
structure BFS = NondetBFS
structure G = BFS.G

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

(* val (_, tm) = Util.getTime (fn _ =>
  if G.parityCheck graph then ()
  else TextIO.output (TextIO.stdErr,
    "WARNING: parity check failed; graph might not be symmetric " ^
    "or might have duplicate- or self-edges\n"))
val _ = print ("parity check in " ^ Time.fmt 4 tm ^ "s\n") *)

val P = Benchmark.run "running bfs" (fn _ => BFS.bfs graph source)

val numVisited =
  SeqBasis.reduce 10000 op+ 0 (0, Seq.length P)
    (fn i => if Option.isSome (Seq.nth P i) then 1 else 0)
val _ = print ("visited " ^ Int.toString numVisited ^ "\n")

fun numHops P v =
  case Seq.nth P v of
    NONE => ~1
  | SOME parents => List.length parents

val maxHops =
  SeqBasis.reduce 100 Int.max ~3 (0, G.numVertices graph) (numHops P)
val _ = print ("max dist " ^ Int.toString maxHops ^ "\n")

fun check () =
  let
    val (P', serialTime) =
      Util.getTime (fn _ => SerialBFS.bfs graph source)

    val correct =
      Seq.length P = Seq.length P'
      andalso
      SeqBasis.reduce 10000 (fn (a, b) => a andalso b) true (0, Seq.length P)
        (fn i => numHops P i = numHops P' i)
  in
    print ("serial finished in " ^ Time.fmt 4 serialTime ^ "s\n");
    print ("correct? " ^ (if correct then "yes" else "no") ^ "\n")
  end

val _ = if doCheck then check () else ()


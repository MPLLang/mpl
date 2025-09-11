structure CLA = CommandLineArgs
structure Seq = ArraySequence
structure G = AdjacencyGraph(Int)

(* Set by subdirectory *)
structure BFS = MkBFS(OldDelayedSeq)

(* Generate an input
 * If -infile <file> is given, then will load file.
 * Otherwise, uses -n <num vertices> -d <degree> to generate a random graph. *)
val filename = CLA.parseString "infile" ""
val t0 = Time.now ()
val (graphspec, input) =
  if filename <> "" then
    (filename, G.parseFile filename)
  else
    let
      val n = CLA.parseInt "n" 1000000
      val d = CLA.parseInt "d" 10
    in
      ("random(" ^ Int.toString n ^ "," ^ Int.toString d ^ ")",
       G.randSymmGraph n d)
    end
val t1 = Time.now ()
val _ = print ("loaded graph in " ^ Time.fmt 4 (Time.- (t1, t0)) ^ "s\n")

val n = G.numVertices input
val source = CLA.parseInt "source" 0
val doCheck = CLA.parseFlag "check"

val _ = print ("graph " ^ graphspec ^ "\n")
val _ = print ("num-verts " ^ Int.toString n ^ "\n")
val _ = print ("num-edges " ^ Int.toString (G.numEdges input) ^ "\n")
val _ = print ("source " ^ Int.toString source ^ "\n")
val _ = print ("check " ^ (if doCheck then "true" else "false") ^ "\n")

fun task () =
  BFS.bfs input source

val P = Benchmark.run "running bfs" task

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
  SeqBasis.reduce 100 Int.max ~3 (0, G.numVertices input) (numHops P 0)
val _ = print ("max dist " ^ Int.toString maxHops ^ "\n")

fun check () =
  let
    val (P', serialTime) =
      Util.getTime (fn _ => SerialBFS.bfs input source)

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

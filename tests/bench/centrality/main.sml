structure CLA = CommandLineArgs
structure BC = BC
structure G = BC.G

val filename =
  case CLA.positional () of
    [x] => x
  | _ => Util.die "missing filename"

val source = CLA.parseInt "source" 0

val (graph, tm) = Util.getTime (fn _ => G.parseFile filename)
val _ = print ("num vertices: " ^ Int.toString (G.numVertices graph) ^ "\n")
val _ = print ("num edges: " ^ Int.toString (G.numEdges graph) ^ "\n")

val (_, tm) = Util.getTime (fn _ =>
  if G.parityCheck graph then ()
  else TextIO.output (TextIO.stdErr,
    "WARNING: parity check failed; graph might not be symmetric " ^
    "or might have duplicate- or self-edges\n"))
val _ = print ("parity check in " ^ Time.fmt 4 tm ^ "s\n")

val result = Benchmark.run "running centrality" (fn _ => BC.bc graph source)

val maxDep =
  SeqBasis.reduce 10000 Real.max 0.0 (0, Seq.length result) (Seq.nth result)
  (* DS.reduce Real.max 0.0 (DS.fromArraySeq result) *)
val _ = print ("maxdep " ^ Real.toString maxDep ^ "\n")

val totDep =
  SeqBasis.reduce 10000 op+ 0.0 (0, Seq.length result) (Seq.nth result)
  (* DS.reduce op+ 0.0 (DS.fromArraySeq result) *)
val _ = print ("avgdep " ^ Real.toString (totDep / Real.fromInt (Seq.length result)) ^ "\n")

val numVisited =
  SeqBasis.reduce 10000 op+ 0 (0, Seq.length result)
    (fn i => if Seq.nth result i < 0.0 then 0 else 1)
val _ = print ("visited " ^ Int.toString numVisited ^ "\n")

val outfile = CLA.parseString "outfile" ""
val _ =
  if outfile = "" then
    print ("use -outfile XXX to see result\n")
  else
    let
      val n = Seq.length result
      val file = TextIO.openOut outfile
      fun dump i =
        if i >= n then ()
        else (TextIO.output (file, Real.toString (Seq.nth result i));
              TextIO.output (file, "\n");
              dump (i+1))
    in
      dump 0;
      TextIO.closeOut file
    end


structure CLA = CommandLineArgs
structure G = AdjacencyGraph(Int)

val filename =
  case CLA.positional () of
    [x] => x
  | _ => Util.die "missing filename"

val graph = G.parseFile filename
val _ = print ("num vertices: " ^ Int.toString (G.numVertices graph) ^ "\n")
val _ = print ("num edges: " ^ Int.toString (G.numEdges graph) ^ "\n")

val outfile = CLA.parseString "outfile" ""

val _ =
  if outfile = "" then () else
  let
    val (_, tm) = Util.getTime (fn _ => G.writeAsBinaryFormat graph outfile)
  in
    print ("wrote graph (binary format) to " ^ outfile ^ " in " ^ Time.fmt 4 tm ^ "s\n")
  end

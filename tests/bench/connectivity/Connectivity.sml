structure Connectivity =
struct
  type 'a seq = 'a Seq.t

  structure G = AdjacencyGraph(Int)
  structure V = G.Vertex
  structure AS = ArraySlice

  type vertex = G.vertex

  (* review this code *)
  fun connectivity g b =
    let
      val n = G.numVertices g
      val (clusters, _) = LDD.ldd g b
      val (g', center_label) = AdjInt.contract clusters g
    in
      if (G.numEdges g') = 0 then clusters
      else
        let
          val l' = connectivity g' b
          (* val _ = print ("edges = " ^ (Int.toString (G.numEdges g')) ^ " vertices = " ^ (Int.toString (G.numVertices g')) ^ "\n") *)
          fun label u = center_label (Seq.nth clusters u)
        in
          Seq.tabulate label n
        end
    end
end
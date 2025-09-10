structure Spanner =
struct
  type 'a seq = 'a Seq.t
  structure G = AdjacencyGraph(Int)
  structure V = G.Vertex
  structure AS = ArraySlice

  type vertex = G.vertex

  fun spanner g k =
    let
      val n = G.numVertices g
      val b = (Math.ln (Real.fromInt n))/(Real.fromInt (2 * k))
      val (clusters, parents) = LDD.ldd g b
      fun is_center i = if i = (Seq.nth clusters i) then 1 else 0
      val compact_clusters = SeqBasis.scan 10000 Int.+ 0 (0, n) is_center
      val num_clusters = Array.sub (compact_clusters, n)
      val _ = print ("number of clusters = " ^ (Int.toString (num_clusters)) ^ "\n")
      fun center i = Array.sub (compact_clusters, (Seq.nth clusters i))
      val intra_edges = Seq.tabulate (fn i => (i, Seq.nth parents i)) n
      val hash_sim = Seq.tabulate (fn i => NONE) (num_clusters*num_clusters)
      fun icu u =
        let
          val cu = center u
          fun add_edge i =
            let
              val ci = center i
              val indexi = cu*num_clusters + ci
            in
              if (cu < ci) then AS.update (hash_sim, indexi, SOME(u, i))
              else ()
            end
        in
          Seq.foreach (G.neighbors g u) (fn (i, si) => add_edge si)
        end
      val _ = ForkJoin.parfor 10000 (0, n) (fn i => icu i)
      val inter_edges = AS.full(SeqBasis.tabFilter 10000 (0, num_clusters*num_clusters) (Seq.nth hash_sim))
    in
      Seq.append (intra_edges, inter_edges)
    end
end
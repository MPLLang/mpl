structure SerialBFS =
struct

  structure G = AdjacencyGraph(Int)

  fun bfs g s =
    let
      fun neighbors v = G.neighbors g v
      fun degree v = G.degree g v

      val n = G.numVertices g
      val m = G.numEdges g

      val queue = ForkJoin.alloc (m+1)
      val parents = Array.array (n, ~1)

      fun search (lo, hi) =
        if lo >= hi then lo else
        let
          val v = Array.sub (queue, lo)
          fun visit (hi', u) =
            if Array.sub (parents, u) >= 0 then hi'
            else ( Array.update (parents, u, v)
                 ; Array.update (queue, hi', u)
                 ; hi'+1
                 )
        in
          search (lo+1, Seq.iterate visit hi (neighbors v))
        end

      val _ = Array.update (parents, s, s)
      val _ = Array.update (queue, 0, s)
      val numVisited = search (0, 1)
    in
      ArraySlice.full parents
    end

end

functor MkBFS (Seq: SEQUENCE) =
struct

  structure G = AdjacencyGraph(Int)

  fun bfs graph source =
    let
      val N = G.numVertices graph
      val M = G.numEdges graph

      fun outEdges u =
        Seq.map (fn v => (u, v)) (Seq.fromArraySeq (G.neighbors graph u))

      val parents = ForkJoin.alloc N
      val _ = ForkJoin.parfor 10000 (0, N) (fn i =>
        Array.update (parents, i, ~1))

      fun isVisited v =
        Array.sub (parents, v) <> ~1

      fun visit (u, v) =
        if not (isVisited v) andalso
           (~1 = Concurrency.casArray (parents, v) (~1, u))
        then
          SOME v
        else
          NONE

      fun loop frontier totalVisited =
        if Seq.length frontier = 0 then
          totalVisited
        else
          let
            val allNeighbors = Seq.flatten (Seq.map outEdges frontier)
            val nextFrontier = Seq.mapOption visit allNeighbors
          in
            loop nextFrontier (totalVisited + Seq.length nextFrontier)
          end

      val _ = Array.update (parents, source, source)
      val initFrontier = Seq.singleton source
      val numVisited = loop initFrontier 1
    in
      ArraySlice.full parents
    end

end

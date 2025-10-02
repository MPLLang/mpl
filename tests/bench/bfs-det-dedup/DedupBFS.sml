structure DedupBFS =
struct
  type 'a seq = 'a Seq.t

  (* structure DS = DelayedSeq *)
  structure G = AdjacencyGraph(Int)
  structure V = G.Vertex

  type vertex = G.vertex

  val sub = Array.sub
  val upd = Array.update

  val vtoi = V.toInt
  val itov = V.fromInt

  (* fun ASsub s =
    let val (a, i, _) = ArraySlice.base s
    in sub (a, i+s)
    end *)

  val GRAIN = 10000

  fun strip s =
    let val (s', start, _) = ArraySlice.base s
    in if start = 0 then s' else raise Fail "strip base <> 0"
    end

  fun bfs {diropt: bool} (g : G.graph) (s : vertex) =
    let
      val n = G.numVertices g
      val parent = strip (Seq.tabulate (fn _ => ~1) n)

      (* Choose method of filtering the frontier: either frontier always
       * only consists of valid vertex ids, or it allows invalid vertices and
       * pretends that these vertices are isolated. *)
      fun degree v = G.degree g v
      fun filterFrontier s = Seq.filter (fn x => x <> itov (~1)) s
      (*
      fun degree v = if v < 0 then 0 else Graph.degree g v
      fun filterFrontier s = s
      *)

      val denseThreshold = G.numEdges g div 20

      fun sumOfOutDegrees frontier =
        SeqBasis.reduce 10000 op+ 0 (0, Seq.length frontier) (degree o Seq.nth frontier)
        (* DS.reduce op+ 0 (DS.map degree (DS.fromArraySeq frontier)) *)

      fun shouldProcessDense frontier =
        diropt andalso
        let
          val n = Seq.length frontier
          val m = sumOfOutDegrees frontier
        in
          n + m > denseThreshold
        end

      fun bottomUp frontier =
        raise Fail "DedupBFS: direction optimization not implemented yet"

      fun topDown frontier =
        let
          val nf = Seq.length frontier
          val offsets = SeqBasis.scan GRAIN op+ 0 (0, nf) (degree o Seq.nth frontier)
          val mf = sub (offsets, nf)
          val outNbrs: (vertex * vertex) array = ForkJoin.alloc mf

          fun visitNeighbors offset v nghs =
            Util.for (0, Seq.length nghs) (fn i =>
              let val u = Seq.nth nghs i
              in upd (outNbrs, offset+i, (v, u))
              end)

          fun visitMany offlo lo hi =
            if lo = hi then () else
            let
              val v = Seq.nth frontier offlo
              val voffset = sub (offsets, offlo)
              val k = Int.min (hi - lo, sub (offsets, offlo+1) - lo)
            in
              if k = 0 then visitMany (offlo+1) lo hi
              else ( visitNeighbors lo v (Seq.subseq (G.neighbors g v) (lo - voffset, k))
                   ; visitMany (offlo+1) (lo+k) hi
                   )
            end

          fun parVisitMany (offlo, offhi) (lo, hi) =
            if hi - lo <= GRAIN then
              visitMany offlo lo hi
            else
              let
                val mid = lo + (hi - lo) div 2
                val (i, j) = OffsetSearch.search mid offsets (offlo, offhi)
                val _ = ForkJoin.par
                  ( fn _ => parVisitMany (offlo, i) (lo, mid)
                  , fn _ => parVisitMany (j-1, offhi) (mid, hi)
                  )
              in
                ()
              end

          val vtow = Word64.fromInt o vtoi
          fun h1 w = Word64.>> (Util.hash64_2 w, 0w32)
          fun h2 w = Util.hash64_2 w

          (* populates outNbrs *)
          val _ = parVisitMany (0, nf + 1) (0, mf)
          val outNbrs = ArraySlice.full outNbrs
          val unvisited = Seq.filter (fn (_, u) => sub (parent, u) = ~1) outNbrs
          val deduped = Dedup.dedup
            (fn ((_, u1), (_, u2)) => u1 = u2)
            (fn (_, u) => h1 (vtow u))
            (fn (_, u) => h2 (vtow u))
            unvisited

          val nextFrontier =
            Seq.map (fn (v, u) => (upd (parent, vtoi u, v); u)) deduped
        in
          nextFrontier
        end

      fun search frontier =
        if Seq.length frontier = 0 then
          ()
        else if shouldProcessDense frontier then
          let
            val (nextFrontier, tm) = Util.getTime (fn _ => bottomUp frontier)
          in
            print ("dense  " ^ Time.fmt 4 tm ^ "\n");
            search nextFrontier
          end
        else
          let
            val (nextFrontier, tm) = Util.getTime (fn _ => topDown frontier)
          in
            print ("sparse " ^ Time.fmt 4 tm ^ "\n");
            search nextFrontier
          end

      val _ = upd (parent, vtoi s, s)
      val _ = search (Seq.fromList [s])
    in
      ArraySlice.full parent
    end

end

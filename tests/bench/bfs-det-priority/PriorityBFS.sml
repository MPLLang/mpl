structure PriorityBFS =
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
      val visited = strip (Seq.tabulate (fn _ => false) n)

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
        raise Fail "PriorityBFS: bottom up not implemented yet"

      fun topDown frontier =
        let
          val nf = Seq.length frontier
          val offsets = SeqBasis.scan GRAIN op+ 0 (0, nf) (degree o Seq.nth frontier)
          val mf = sub (offsets, nf)
          val outNbrs = ForkJoin.alloc mf

          (* Priority update, attempt set v as parent of u. Returns true only
           * if it is the first visit, to ensure that u appears at most once
           * in next frontier. *)
          fun tryVisit (u, v) =
            if sub (visited, u) then false else
            let
              val old = sub (parent, u)
              val isFirstVisit = (old = ~1)
            in
              if v <= old then
                false
              else if old = Concurrency.casArray (parent, u) (old, v) then
                isFirstVisit
              else
                tryVisit (u, v)
            end

          fun visitNeighbors offset v nghs =
            Util.for (0, Seq.length nghs) (fn i =>
              let val u = Seq.nth nghs i
              in if not (tryVisit (vtoi u, vtoi v))
                 then upd (outNbrs, offset + i, itov (~1))
                 else upd (outNbrs, offset + i, u)
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

          (* Either one of the following is correct, but the second one has
           * significantly better granularity control for graphs that have a
           * small number of vertices with huge degree. *)

          (* val _ = ParUtil.parfor 100 (0, nf) (fn i =>
            visitMany i (sub (offsets, i)) (sub (offsets, i+1))) *)

          val _ = parVisitMany (0, nf + 1) (0, mf)
          val nextFrontier = filterFrontier (ArraySlice.full outNbrs)
        in
          ForkJoin.parfor 5000 (0, Seq.length nextFrontier) (fn i =>
            let
              val v = Seq.nth nextFrontier i
            in
              upd (visited, v, true)
            end);

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
      val _ = upd (visited, vtoi s, true)
      val _ = search (Seq.fromList [s])
    in
      ArraySlice.full parent
    end

end

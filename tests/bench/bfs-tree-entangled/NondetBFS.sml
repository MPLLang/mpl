(* nondeterministic direction-optimized BFS, using CAS on outneighbors to
 * construct next frontier. *)
structure NondetBFS =
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

  fun tryUpdateSome (xs: 'a option array, i: int, old: 'a option, new: 'a option) =
    let
      val result = Concurrency.casArray (xs, i) (old, new)
    in
      if MLton.eq (old, result) then
        true
      else if Option.isSome result then
        false
      else
        tryUpdateSome (xs, i, result, new)
    end

  fun bfs (g : G.graph) (s : vertex) =
    let
      val n = G.numVertices g
      val parent = strip (Seq.tabulate (fn _ => NONE) n)

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

      fun shouldProcessDense frontier = false
        (* let
          val n = Seq.length frontier
          val m = sumOfOutDegrees frontier
        in
          n + m > denseThreshold
        end *)

      fun bottomUp frontier =
        let
          val flags = Seq.tabulate (fn _ => false) n
          val _ = Seq.foreach frontier (fn (_, v) =>
            ArraySlice.update (flags, v, true))
          fun inFrontier v = Seq.nth flags (vtoi v)

          fun processVertex v =
            case sub (parent, v) of
              SOME _ => NONE
            | NONE =>
                let
                  val nbrs = G.neighbors g (itov v)
                  val deg = ArraySlice.length nbrs
                  fun loop i =
                    if i >= deg then
                      NONE
                    else
                      let
                        val u = Seq.nth nbrs i
                      in
                        if inFrontier u then
                          let
                            val parentList = Option.valOf (sub (parent, u))
                          in
                            upd (parent, v, SOME (u :: parentList));
                            SOME v
                          end
                        else
                          loop (i+1)
                      end
                in
                  loop 0
                end
        in
          ArraySlice.full (SeqBasis.tabFilter 1000 (0, n) processVertex)
        end

      fun topDown frontier =
        let
          val nf = Seq.length frontier
          val offsets = SeqBasis.scan GRAIN op+ 0 (0, nf) (degree o Seq.nth frontier)
          val mf = sub (offsets, nf)
          val outNbrs = ForkJoin.alloc mf

          (* attempt to claim parent of u as v *)
          (* fun claim (u, v) =
            sub (parent, u) = ~1
            andalso
            ~1 = Concurrency.casArray (parent, u) (~1, v) *)

          fun visitNeighbors offset v nghs =
            Util.for (0, Seq.length nghs) (fn i =>
              let
                val u = Seq.nth nghs i
              in
                case sub (parent, vtoi u) of
                  SOME _ => upd (outNbrs, offset + i, itov (~1))
                | old as NONE =>
                    let
                      val parentList = Option.valOf (sub (parent, vtoi v))
                      val parentList' = SOME (v :: parentList)
                    in
                      if tryUpdateSome (parent, vtoi u, old, parentList') then
                        upd (outNbrs, offset + i, u)
                      else
                        upd (outNbrs, offset + i, itov (~1))
                    end
              end)

              (* let val u = Seq.nth nghs i
              in if not (claim (vtoi u, vtoi v))
                 then upd (outNbrs, offset + i, itov (~1))
                 else upd (outNbrs, offset + i, u)
              end) *)

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
        in
          filterFrontier (ArraySlice.full outNbrs)
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

      val _ = upd (parent, vtoi s, SOME [])
      val _ = search (Seq.fromList [s])
    in
      ArraySlice.full parent
    end

end

structure BC =
struct
  structure G = AdjacencyGraph(Int)
  structure V = G.Vertex

  type vertex = G.vertex
  type graph = G.graph

  val sub = Array.sub
  val upd = Array.update

  val vtoi = V.toInt
  val itov = V.fromInt

  (* fun ASsub s =
    let val (a, i, _) = ArraySlice.base s
    in sub (a, i+s)
    end *)

  val GRAIN = 10000

  fun sumOfOutDegrees g frontier =
    SeqBasis.reduce 10000 op+ 0 (0, Seq.length frontier) (G.degree g o Seq.nth frontier)
      (* DS.reduce op+ 0 (DS.map degree (DS.fromArraySeq frontier)) *)

  fun shouldProcessDense g frontier =
    let
      val n = Seq.length frontier
      val m = sumOfOutDegrees g frontier
    in
      n + m > (G.numEdges g) div 20
    end

  fun edgeMapDense
      { cond : (vertex * 'a) -> bool
      , func : (vertex * vertex * 'a * 'a) -> 'a option
      , eq : 'a * 'a -> bool
      , frontier
      , visitedRoundNums
      , state
      , roundNum
      , graph = g
      , shouldOutput
      } =
    let
      val N = G.numVertices g
      val M = G.numEdges g

      (* val visitedRoundNums = ForkJoin.alloc N
      val _ = ForkJoin.parfor 10000 (0, N) (fn i => upd (visitedRoundNums, i, ~1)) *)
      fun visitedRound v = sub (visitedRoundNums, v)
      fun isVisited v = visitedRound v <> ~1
      fun setVisited v r = upd (visitedRoundNums, v, r)

      (* val state : 'a array = ForkJoin.alloc N *)
      (* val _ = ForkJoin.parfor 10000 (0, N) (fn i => upd (state, i, initialState i)) *)
      fun getState v = sub (state, v)
      fun setState v s = upd (state, v, s)

      val flags = Seq.tabulate (fn _ => false) N
      val _ = Seq.foreach frontier (fn (_, v) =>
        ArraySlice.update (flags, v, true))
      fun inFrontier v = Seq.nth flags (vtoi v)

      fun processVertex v =
        if isVisited v then NONE else
        let
          val nbrs = G.neighbors g (itov v)
          val deg = ArraySlice.length nbrs
          fun loop visited sv i =
            if i >= deg then (visited, sv) else
            let
              val u = Seq.nth nbrs i
            in
              if not (inFrontier u) then loop visited sv (i+1) else
              case func (u, v, getState u, sv) of
                NONE => loop true sv (i+1)
              | SOME sv' => if cond (v, sv') then (true, sv') else loop true sv' (i+1)
            end
          val sv = getState v
          val (visited, sv') = loop false sv 0
        in
          if eq (sv, sv') then () else setState v sv';
          if not visited then NONE else (setVisited v roundNum; SOME v)
        end
    in
      if shouldOutput then
        ArraySlice.full (SeqBasis.tabFilter 1000 (0, N) processVertex)
        (* G.tabFilter 1000 (0, N) processVertex *)
      else
        (ForkJoin.parfor 1000 (0, N) (ignore o processVertex); Seq.empty ())
    end

  fun edgeMapSparse
      { cond : (vertex * 'a) -> bool
      , func : (vertex * vertex * 'a * 'a) -> 'a option
      , eq : 'a * 'a -> bool
      , frontier
      , visitedRoundNums
      , roundNum
      , state
      , graph = g
      , shouldOutput
      } =
    let
      val N = G.numVertices g
      val M = G.numEdges g

      fun degree v = G.degree g v
      fun filterFrontier s = Seq.filter (fn x => x <> itov (~1)) s

      (* val visitedRoundNums = ForkJoin.alloc N
      val _ = ForkJoin.parfor 10000 (0, N) (fn i => upd (visitedRoundNums, i, ~1)) *)
      fun visitedRound v = sub (visitedRoundNums, v)
      fun isVisited v = visitedRound v <> ~1
      fun setVisited v r = upd (visitedRoundNums, v, r)
      fun claimVisited v r =
        ~1 = Concurrency.casArray (visitedRoundNums, v) (~1, r)

      (* val state : 'a array = ForkJoin.alloc N *)
      (* val _ = ForkJoin.parfor 10000 (0, N) (fn i => upd (state, i, initialState i)) *)
      fun getState v = sub (state, v)
      fun setState v s = upd (state, v, s)

      (* repeatedly try to update the state of v by CASing the result of
       * computing func (u, v, su, sv) *)
      fun tryPushUpdateState (u, v, su, sv) =
        if cond (v, sv) then () else
        case func (u, v, su, sv) of
          NONE => ()
        | SOME desired =>
            let
              val sv' = Concurrency.casArray (state, v) (sv, desired)
            in
              if eq (sv', sv)
              then ()
              else tryPushUpdateState (u, v, su, sv')
            end

      val nf = Seq.length frontier
      val offsets = SeqBasis.scan GRAIN op+ 0 (0, nf) (degree o Seq.nth frontier)
      val mf = sub (offsets, nf)
      val outNbrs =
        if shouldOutput
        then ForkJoin.alloc mf
        else Array.fromList []

      fun writeOut i x = upd (outNbrs, i, x)
      fun checkWriteOut i x =
        if shouldOutput then writeOut i x else ()

      fun visitNeighbors offset u nghs =
        Util.for (0, Seq.length nghs) (fn i =>
          let
            val v = Seq.nth nghs i
            val r = visitedRound v
          in
            if 0 <= r andalso r < roundNum then
              (* v was visited on a previous round, so ignore it. *)
              checkWriteOut (offset+i) (itov (~1))
            else
              ( if shouldOutput then
                  (if r = ~1 andalso claimVisited v roundNum then
                    writeOut (offset+i) v
                  else
                    writeOut (offset+i) (itov (~1)))
                else
                  (if r = ~1 then setVisited v roundNum else ())

                (* regardless, we need to check for updating state. *)
              ; tryPushUpdateState (u, v, getState u, getState v)
              )
          end)

      fun visitMany offlo lo hi =
        if lo = hi then () else
        let
          val u = Seq.nth frontier offlo
          val voffset = sub (offsets, offlo)
          val k = Int.min (hi - lo, sub (offsets, offlo+1) - lo)
        in
          if k = 0 then visitMany (offlo+1) lo hi
          else ( visitNeighbors lo u (Seq.subseq (G.neighbors g u) (lo - voffset, k))
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

      (* val _ = ForkJoin.parfor 100 (0, nf) (fn i =>
        visitMany i (sub (offsets, i)) (sub (offsets, i+1))) *)

      val _ = parVisitMany (0, nf + 1) (0, mf)
    in
      filterFrontier (ArraySlice.full outNbrs)
    end

  fun edgeMap X =
    if shouldProcessDense (#graph X) (#frontier X) then
      let
        val (nextFrontier, tm) = Util.getTime (fn _ => edgeMapDense X)
      in
        print ("dense  " ^ Time.fmt 4 tm ^ "\n");
        nextFrontier
      end
    else
      let
        val (nextFrontier, tm) = Util.getTime (fn _ => edgeMapSparse X)
      in
        print ("sparse " ^ Time.fmt 4 tm ^ "\n");
        nextFrontier
      end

  fun bc graph source =
    let
      val g = graph

      val N = G.numVertices g
      val M = G.numEdges g

      fun initialNumPaths v =
        if v = source then 1 else 0

      val visitedRoundNums = ForkJoin.alloc N
      val _ = ForkJoin.parfor 10000 (0, N) (fn i => upd (visitedRoundNums, i, ~1))

      val numPathsArr = ForkJoin.alloc N
      val _ = ForkJoin.parfor 10000 (0, N) (fn i => upd (numPathsArr, i, initialNumPaths i))

      (* accumulate number of paths through v *)
      fun edgeFunc (u, v, uNumPaths, vNumPaths) =
        uNumPaths + vNumPaths

      fun forwardsEdgeMap roundNum frontier =
        edgeMap
        { cond = (fn _ => false) (* accumulate all edges *)
        , func = SOME o edgeFunc
        , eq = op=
        , frontier = frontier
        , visitedRoundNums = visitedRoundNums
        , roundNum = roundNum
        , state = numPathsArr
        , graph = g
        , shouldOutput = true
        }

      fun forwardsLoop pastFrontiers roundNum frontier =
        if Seq.length frontier = 0 then
          pastFrontiers
        else
          let
            val nextFrontier = forwardsEdgeMap roundNum frontier
          in
            forwardsLoop (frontier :: pastFrontiers) (roundNum+1) nextFrontier
          end

      val _ = upd (visitedRoundNums, source, 0)
      val frontiers = forwardsLoop [] 1 (Seq.fromList [source])

      val numPaths = ArraySlice.full numPathsArr

      (* val mnp = DS.reduce Int.max 0 (DS.fromArraySeq numPaths) *)
      val mnp = SeqBasis.reduce 10000 Int.max 0 (0, N) (Seq.nth numPaths)
      val _ = print ("max-num-paths " ^ Int.toString mnp ^ "\n")

      val lastFrontier = List.hd frontiers

      (* =====================================================================
       * second phase: search in reverse.
       *)

      val invNumPaths = ForkJoin.alloc N
      val _ = ForkJoin.parfor 10000 (0, N) (fn i =>
        upd (invNumPaths, i, 1.0 / Real.fromInt (sub (numPathsArr, i))))

      val visitedRoundNums = ForkJoin.alloc N
      val _ = ForkJoin.parfor 10000 (0, N) (fn i => upd (visitedRoundNums, i, ~1))

      val deps = ForkJoin.alloc N
      val _ = ForkJoin.parfor 10000 (0, N) (fn i => upd (deps, i, 0.0))

      fun edgeFunc (u, v, uDep, vDep) =
        vDep + uDep + sub (invNumPaths, u)

      fun backwardsEdgeMap roundNum frontier =
        edgeMap
        { cond = (fn _ => false) (* accumulate all edges *)
        , func = SOME o edgeFunc
        , eq = Real.==
        , frontier = frontier
        , visitedRoundNums = visitedRoundNums
        , roundNum = roundNum
        , state = deps
        , graph = g
        , shouldOutput = false
        }

      fun backwardsLoop frontiers roundNum =
        case frontiers of
          [] => ()
        | frontier :: frontiers' =>
            let
              val _ = Seq.foreach frontier (fn (_, v) =>
                upd (visitedRoundNums, v, roundNum))

              val _ = backwardsEdgeMap (roundNum+1) frontier
            in
              backwardsLoop frontiers' (roundNum+1)
            end

      val _ = backwardsLoop frontiers 0
    in
      Seq.tabulate (fn i => sub (deps, i) / sub (invNumPaths, i)) N
    end

end

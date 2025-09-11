structure LDD =
struct
  type 'a seq = 'a Seq.t
  structure G = AdjacencyGraph(Int)
  (* structure VS = G.VertexSubset *)
  structure V = G.Vertex
  structure AS = ArraySlice

  type vertex = G.vertex

  fun strip s =
    let val (s', start, _) = AS.base s
    in if start = 0 then s' else raise Fail "strip base <> 0"
    end

  fun partition n b =
    let
      val s = Shuffle.shuffle (Seq.tabulate (fn i => i) n) 0
      fun subseq s (i, j) = Seq.subseq s (i, j - i)
      fun distribute w acc =
        let
          val i = Real.fromInt (List.length acc)
          val bi = Real.floor (Math.pow (Math.e, b*i))
          val r = w + bi
        in
          if r >= n then (subseq s (w, n))::acc
          else distribute r ((subseq s (w, r))::acc)
        end
    in
      Seq.rev (Seq.fromList (distribute 0 []))
    end

  fun ldd g b =
    let
      val n = G.numVertices g
      val (pr, tm) = Util.getTime (fn _ => partition n b)
      val _ = print ("partition: " ^ Time.fmt 4 tm ^ "\n")
      val pr_len = Seq.length pr
      val visited = strip (Seq.tabulate (fn i => false) n)
      val cluster = Seq.tabulate (fn i => n + 1) n
      val parent = Seq.tabulate (fn i => n + 1) n

      fun item s i = AS.sub (s, i)
      fun set s i v = AS.update (s, i, v)

      fun initialize_cluster u =
        if (Seq.nth cluster u) > n then
          (set cluster u u;  Array.update (visited, u, true); SOME(u))
        else NONE

      fun updateseq (s, d) =
        if (Array.sub (visited, d)) then NONE
        else
          (Array.update(visited, d, true); set cluster d (item cluster s); set parent d s; (SOME d))

      fun updatepar (s, d) =
        if not (Concurrency.casArray (visited, d) (false, true)) then
          (set cluster d (item cluster s); set parent d s; (SOME d))
        else NONE

      val update = (updatepar, updateseq)

      fun cond u = not (Array.sub (visited, u))
      val deg = Int.div (G.numEdges g, G.numVertices g)
      val denseThreshold = G.numEdges g div (20*(1 + deg))

      fun ldd_helper fr i =
        if i >= pr_len then ()
        else
          let
            val (fr', tm) = Util.getTime (fn _ =>
              let
                val pri = Seq.nth pr i
                val new_clusters = SeqBasis.tabFilter 1000 (0, Seq.length pri) (fn i => initialize_cluster (Seq.nth pri i))
                (* val new_clusters = Seq.filter (fn v => (Seq.nth cluster v) > n) (Seq.nth pr i) *)
                (* val _ = Seq.foreach new_clusters initialize_cluster *)
                (* val fr_len = Seq.length fr *)
                val nc = AS.full new_clusters
                val app_frontier = AdjInt.append fr nc n
                (* fn i => if (i < fr_len) then Seq.nth fr i else Seq.nth nc (i - fr_len) *)
                (* val fr' = Seq.append (fr, nc) *)
              in
                (* (app_frontier, fr_len + (Seq.length nc)) *)
                app_frontier
              end)
            val _ = print ("round " ^ Int.toString i ^ ": new_clusters: " ^ Time.fmt 4 tm ^ "\n")
            val (fr'', tm) = Util.getTime (fn _ => AdjInt.edge_map g fr' update cond)
            (* val b = if (should_process_sparse g fr') then "sparse" else "dense" *)
            val _ = print ("round " ^ Int.toString i ^ " edge_map: " ^ Time.fmt 4 tm ^ "\n")
          in
            ldd_helper fr'' (i + 1)
          end
    in
      (ldd_helper (AdjInt.empty (denseThreshold)) 0;
      (cluster, parent))
    end

    fun check_ldd g c p =
      let
        val m = G.numEdges g
        val n = G.numVertices g
        val arr_set = strip (Seq.tabulate (fn i => false) n)
        val tups = Seq.tabulate (fn i => (i, Seq.nth c i)) (Seq.length c)
        fun outgoing cid =
          let
            val s = Seq.map (fn (i, j) => i) (Seq.filter (fn (i, j) => j = cid) tups)
            val _ = Seq.foreach s (fn (_ ,v) => Array.update (arr_set, v, true))
            fun greater_neighbors v = Seq.filter (fn u => v < u) (G.neighbors g v)
            val grt_neighbors = Seq.flatten (Seq.map  greater_neighbors s)
            val grt_out_neighbors = Seq.filter (fn v => not (Array.sub(arr_set, v))) grt_neighbors
            val _ = Seq.foreach s (fn (_ ,v) => Array.update (arr_set, v, false))
          in
            if (Seq.length s) = 0 then ~1
            else Seq.length grt_out_neighbors
          end

        fun check_helper i cc ce =
          if i >= n then (cc, ce)
          else
            let
              val num_outgoing = outgoing i
            in
              if num_outgoing = ~1 then check_helper (i + 1) cc ce
              else (check_helper (i + 1) (cc + 1) (ce + num_outgoing))
            end
        val (cc, ce) = check_helper 0 0 0
      in
        print ("num clusters = " ^ (Int.toString cc) ^ ", num edges = " ^ (Int.toString m) ^ ", inter-edges = " ^ (Int.toString ce) ^ "\n")
      end
    fun slts s = " " ^ Int.toString (Seq.length s) ^ " "
    fun print_seq si s se = (print (si ^ " "); Seq.foreach s (fn (_,v) => print ((Int.toString v)^ " ") ) ; print (" " ^ se ^ "\n"))
end

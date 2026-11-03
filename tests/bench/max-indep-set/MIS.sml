structure MIS =
struct

  structure G = AdjacencyGraph(Int)
  structure V = G.Vertex
  structure AS = ArraySlice

  fun zero_out_neighbors g roots pr =
    let
      fun updateseq (s, d) =
        if (Array.sub (pr, d) = 0) then NONE
        else
          (Array.update (pr, d, 0); SOME d)

      fun updatepar (s, d) =
        let
          val v = Array.sub (pr, d)
        in
          if (v = 0) then NONE
          else if (Concurrency.casArray (pr, d) (v, 0) = v) then (SOME d)
          else NONE
        end

      val update = (updatepar, updatepar)
      val removed = AdjInt.edge_map g roots update (fn d => Array.sub (pr, d) > 0)
    in
      removed
    end

  fun strip s =
    let val (s', start, _) = AS.base s
    in if start = 0 then s' else raise Fail "strip base <> 0"
    end

  fun mis g =
    let
      val (n, m) = (G.numVertices g, G.numEdges g)
      val deg = Int.div (m, n)
      val P = Shuffle.shuffle (Seq.tabulate (fn i => i) n) 0
      val pr = SeqBasis.tabulate 100 (0, n)
        (fn i => let
          val my_prio = Seq.nth P i
        in
          Seq.iterate (fn (acc, j) => acc + (if (Seq.nth P j) < my_prio then 1 else 0)) 0 (G.neighbors g i)
        end)
      val pr_seq = AS.full pr

      val denseThreshold = G.numEdges g div (20*(1 + deg))
      val sparse_rep = AS.full (SeqBasis.filter 10000 (0, n) (fn i => i) (fn i => Seq.nth pr_seq i = 0))
      val ind_set = Seq.tabulate (fn _ => false) n

      val roots = AdjInt.from_sparse_rep sparse_rep denseThreshold n
      fun loop_roots finished roots =
        if finished < n then
          let
            val _ = AdjInt.vertex_foreach g roots (fn u => if (Seq.nth ind_set u) then () else AS.update (ind_set, u, true))
            val removed = zero_out_neighbors g roots pr
            fun decrement_priority_par (s, d) =
              let
                val p = Seq.nth P s
                val q = Seq.nth P d
              in
                if p >= q then NONE
                else if (Concurrency.faaArray (pr, d) (~1) = 1) then SOME d
                else NONE
              end
            fun decrement_priority_seq (s, d) =
              let
                val p = Seq.nth P s
                val q = Seq.nth P d
                val prio = Array.sub (pr, d)
                val _ = if prio <= 0 orelse p >= q then ()
                        else Array.update (pr, d, prio-1)
              in
                if (prio = 1) then SOME d
                else NONE
              end
            val dec = (decrement_priority_par, decrement_priority_seq)
            val new_roots = AdjInt.edge_map g removed dec (fn d => Array.sub (pr, d) > 0)
          in
            loop_roots (finished + (AdjInt.size roots) + (AdjInt.size removed)) new_roots
          end
        else ()
    in
      loop_roots 0 roots;
      ind_set
    end

    fun verify_mis g ind_set =
      let
        val (n, m) = (G.numVertices g, G.numEdges g)
        val int_ind = Seq.tabulate (fn i => 0) n
        fun ok_f u =
          let
            val count = Seq.iterate (fn (acc, b) => if (Seq.nth ind_set b) then acc + 1 else acc) 0 (G.neighbors g u)
          in
            (Seq.nth ind_set u) orelse (not (count = 0))
          end
        val bool_ok = Seq.tabulate ok_f n
        val all_ok = Seq.reduce (fn (b, acc) => b andalso acc) true bool_ok
      in
        if all_ok then ()
        else print ("Invalid Independent Set\n")
      end
end

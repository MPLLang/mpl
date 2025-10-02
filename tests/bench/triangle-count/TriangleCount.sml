structure TriangleCount =
struct
  type 'a seq = 'a Seq.t

  structure G = AdjacencyGraph(Int)
  structure V = G.Vertex
  structure AS = ArraySlice

  type vertex = G.vertex
  exception Assert

  (* assumes l <= r and all elements accessible in [l, r) *)
  (* ensures that all elements left of the returned index are lesser *)
  fun bin_search k s (l, r) =
    if l = r then (l, false)
    else if (r - l) = 1 then (l, (Seq.nth s l) = k)
    else
      let
        val mid = l + Int.div (r - l - 1, 2)
      in
        case Int.compare (k, Seq.nth s mid) of
          EQUAL => (mid, true)
        | LESS => bin_search k s (l, mid)
        | GREATER => bin_search k s (mid + 1, r)
      end

  fun intersection_count s s' gran =
    let
      fun countseq1 s1 s2 =
        let
          val (n1, n2) = (Seq.length s1, Seq.length s2)
          fun helper l1 l2 acc =
            if (n1 <= l1) orelse (n2 <= l2) then acc
            else
              case Int.compare (Seq.nth s1 l1, Seq.nth s2 l2) of
                EQUAL => helper (l1 + 1) (l2 + 1) (acc + 1)
              | LESS => helper (l1 + 1) l2 acc
              | GREATER => helper l1 (l2 + 1) acc
        in
          helper 0 0 0
        end

      fun countseq2 s1 s2 =
        let
          val (n1, n2) = (Seq.length s1, Seq.length s2)
          fun helper l acc =
            if l >= n1 then acc
            else
              let
                val k = Seq.nth s1 l
                val (idx, found) = bin_search k s2 (0, n2)
                val bump = if found then 1 else 0
              in
                helper (l + 1) (acc + bump)
              end
        in
          if n2 = 0 then 0
          else helper 0 0
        end

      fun subs s i j = Seq.subseq s (i, j - i)
      fun countpar s1 s2 =
        let
          val (n1, n2) = (Seq.length s1, Seq.length s2)
          val nR = n1 + n2
        in
          if nR < gran then countseq1 s1 s2
          else if n2 < n1 then countpar s2 s1
          else if n1 < Int.div (gran, 64) then countseq2 s1 s2
          else
            let
              val mid1 = Int.div (n1, 2)
              val k1 = Seq.nth s1 mid1
              val (mid2, found) = bin_search k1 s2 (0, n2)
              val bump = if found then 1 else 0
              val (l, r) = ForkJoin.par (fn _ => countpar (subs s1 0 mid1) (subs s2 0 (mid2 + 1 - bump)),
                                fn _ => countpar (subs s1 (mid1 + 1) n1) (subs s2 (mid2 + bump) n2))
            in
              l + bump + r
            end
        end
      val r = countpar s s'
    in
      r
    end

  (* get common vertices greater than min_elt *)
  fun intersection_count_thresh s s' gran min_elt =
    let
      val (n1, n2) = (Seq.length s, Seq.length s')
      val (k1, _) = bin_search min_elt s (0, n1)
      val (k2, _) = bin_search min_elt s' (0, n2)
      fun subs s i j = Seq.subseq s (i, j - i)
    in
      if (k1 = n1) orelse (k2 = n2) then 0
      else intersection_count (subs s k1 n1) (subs s' k2 n2) gran
    end

  fun triangle_count g =
    let
      fun count u =
        let
          val ngbrs = G.neighbors g u
          val num_ngbrs = Seq.length ngbrs
          val (idx, _) = bin_search u ngbrs (0, num_ngbrs)
          val ngbrs = Seq.subseq ngbrs (idx, num_ngbrs - idx)
          val num_ngbrs = Seq.length ngbrs
          fun helpi i =
          let
            val v = Seq.nth ngbrs i
          in
            if u < v then
              intersection_count_thresh ngbrs (G.neighbors g v) 10000 v
            else
              0
          end
          val r = SeqBasis.reduce 100 Int.+ 0 (0, num_ngbrs) helpi
        in
          r
        end
      (* val tr_counts = Seq.tabulate count (G.numVertices g) *)
    in
      SeqBasis.reduce 100 op+ 0 (0, G.numVertices g) count
    end
end

structure LDD =
struct
  type 'a seq = 'a Seq.t
  structure G = AdjacencyGraph(Int)
  structure V = G.Vertex
  structure AS = ArraySlice
  structure DS = DelayedSeq

  type vertex = G.vertex

  fun strip s =
    let val (s', start, _) = AS.base s
    in if start = 0 then s' else raise Fail "strip base <> 0"
    end

  fun vertex_map g f h =
    AS.full (SeqBasis.tabFilter 10000 (0, G.numVertices g) (fn i => if h(i) then f(i) else NONE))

  (* inplace Knuth shuffle [l, r) *)
  fun seq_random_shuffle s l r seed =
    let
      fun item i = AS.sub (s, i)
      fun set (i, v) = AS.update (s, i, v)
      (* get a random idx in [l, i] *)
      fun rand_idx i = Int.mod (Util.hash (seed + i), i - l + 1) + l
      fun swap (i,j) =
        let
          val tmp = item i
        in
          set(i, item j); set(j, tmp)
        end
      fun shuffle_helper li =
        if r - li < 2  then ()
        else (swap (li, rand_idx li); shuffle_helper (li + 1))
    in
      shuffle_helper l
    end

  fun log2_up n = Real.ceil (Math.log10 (Real.fromInt n) / (Math.log10 2.0))

  fun bit_and (n, mask) = Word.toInt (Word.andb (Word.fromInt n, mask))
  fun range_check s n = Seq.length (Seq.filter (fn i => i >= n) s) = 0

  fun shuffle s (n : int) seed =
    if n < 1000 then
      let
        val cs = Seq.map (fn i => i) s
        val _ = seq_random_shuffle cs 0 n 0
      in
        cs
      end
    else
      let
        val l = log2_up n
        val bits = if n < Real.floor (Math.pow (2.0, 27.0)) then Int.div ((l - 7), 2)
                   else l - 17
        val num_buckets = Real.floor (Math.pow (2.0, Real.fromInt bits))
        val mask = Word.fromInt (num_buckets - 1)
        fun rand_pos i = bit_and (Util.hash (seed + i), mask)
        (* size of bucket_offsets = num_buckets + 1 *)
        val (s', bucket_offsets) = CountingSort.sort s rand_pos num_buckets
        fun bucket_shuffle i = seq_random_shuffle s' (Seq.nth bucket_offsets i) (Seq.nth bucket_offsets (i + 1)) seed
        val _ = ForkJoin.parfor 1 (0, num_buckets) bucket_shuffle
      in
        s'
      end

  fun partition n b =
    let
      val s = shuffle (Seq.tabulate (fn i => i) n) n 0
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

      fun update (s, d) =
        if not (Concurrency.casArray (visited, d) (false, true)) then
          (set cluster d (item cluster s); set parent d s; (SOME d))
        else NONE

      fun cond u = not (Array.sub (visited, u))

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
                val fr_len = Seq.length fr
                val nc = AS.full new_clusters
                val app_frontier = fn i => if (i < fr_len) then Seq.nth fr i else Seq.nth nc (i - fr_len)
                (* val fr' = Seq.append (fr, nc) *)
              in
                (app_frontier, fr_len + (Seq.length nc))
              end)
            val _ = print ("round " ^ Int.toString i ^ ": new_clusters: " ^ Time.fmt 4 tm ^ "\n")
            val (fr'', tm) = Util.getTime (fn _ => AdjInt.edge_map g fr' update cond)
            val _ = print ("round " ^ Int.toString i ^ ": edge_map: " ^ Time.fmt 4 tm ^ "\n")
          in
            ldd_helper fr'' (i + 1)
          end
    in
      (ldd_helper (Seq.empty ()) 0;
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

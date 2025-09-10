structure AdjInt =
struct
  type 'a seq = 'a Seq.t

  structure G = AdjacencyGraph(Int)
  structure AS = ArraySlice
  open G.VertexSubset

  fun to_seq g (vs, threshold) =
    case vs of
      SPARSE s => s
    | DENSE s => to_seq g (G.VertexSubset.dense_to_sparse vs, threshold)

  fun should_process_sparse g n  =
    let
      val denseThreshold = G.numEdges g div 20
      val deg = Int.div (G.numEdges g, G.numVertices g)
      val count = (1 + deg) * n
    in
      count <= denseThreshold
    end

  fun edge_map_dense g vertices f h =
    let
      val inFrontier = vertices
      val n = Seq.length vertices
      val res = Seq.tabulate (fn _ => 0) n

      fun processVertex v =
        if not (h v) then 0
        else
          let
            val neighbors = G.neighbors g v
            fun loop i =
              if i >= Seq.length neighbors then 0 else
              let val u = Seq.nth neighbors i
              in
                if not (Seq.nth inFrontier u = 1) then
                  loop (i+1)
                else
                  case f (u, v) of
                    NONE => loop (i+1)
                  | SOME x => (AS.update (res, x, 1); 1)
              end
          in
            loop 0
          end
      val count = SeqBasis.reduce 1000 op+ 0 (0, n) processVertex
    in
      (res, count)
    end

  fun edge_map_sparse g vertices f h =
    let
      val n = Seq.length vertices
      fun ui uidx = Seq.nth vertices uidx
      val r =
        SeqBasis.scan 1000 op+ 0 (0, n) (G.degree g o ui)
      val (offsets, totalOutDegree) = (AS.full r, Array.sub (r, n))
      val store = ForkJoin.alloc totalOutDegree
      val k = 100
      val numBlocks = 1 + (totalOutDegree-1) div k
      fun map_block i =
        let
          val lo = i*k
          val hi = Int.min((i+1)*k, totalOutDegree)
          val ulo =
            let
              val a = BinarySearch.search (Int.compare) offsets lo
            in
              if (Seq.nth offsets a) > lo then a - 1
              else a
            end
          fun map_seq idx (u, uidx) count =
            if idx >= hi then count
            else if idx >= (Seq.nth offsets (uidx + 1)) then map_seq idx (ui (uidx + 1), uidx + 1) count
            else
              let
                val v = Seq.nth (G.neighbors g u) (idx - (Seq.nth offsets uidx))
              in
                if (h v) then
                  case f (u, v) of
                    SOME x => (Array.update (store, lo + count, x); map_seq (idx + 1) (u, uidx) (count + 1))
                  | NONE => (map_seq (idx + 1) (u, uidx) count)
                else
                  (map_seq (idx + 1) (u, uidx) count)
              end
        in
          map_seq lo (ui ulo, ulo) 0
        end
      val counts = SeqBasis.tabulate 1 (0, numBlocks) map_block
      val outOff = SeqBasis.scan 10000 op+ 0 (0, numBlocks) (fn i => Array.sub (counts, i))
      val outSize = Array.sub (outOff, numBlocks)
      val result = ForkJoin.alloc outSize
    in
      ForkJoin.parfor (totalOutDegree div (Int.max (outSize, 1))) (0, numBlocks) (fn i =>
      let
        val soff = i * k
        val doff = Array.sub (outOff, i)
        val size = Array.sub (outOff, i+1) - doff
      in
        Util.for (0, size) (fn j =>
          Array.update (result, doff+j, Array.sub (store, soff+j)))
      end);
      (AS.full result)
    end

  fun edge_map g (vs, threshold) (fpar, f) h =
    case vs of
      SPARSE s =>
          from_sparse_rep (edge_map_sparse g s fpar h) threshold (G.numVertices g)
    | DENSE s =>
        let
          val (res, count) = edge_map_dense g s f h
        in
          from_dense_rep res (SOME count) threshold
        end

  fun vertex_foreach g (vs, threshold) f =
    case vs of
      SPARSE s =>
        Seq.foreach s (fn (i, u) => f u)
    | DENSE s =>
        Seq.foreach s (fn (i, b) => if (b = 1) then (f i) else ())

  fun vertex_map_ g (vs, threshold) f =
    case vs of
      SPARSE s =>
        let
          val s' =
            AS.full (SeqBasis.tabFilter 1000 (0, Seq.length s)
              (fn i =>
                let
                  val u = Seq.nth s i
                  val b = f u
                in
                  if b then SOME u
                  else NONE
                end
              ))
        in
          (from_sparse_rep s' threshold (G.numVertices g))
        end
    | DENSE s =>
        let
          val res =
            Seq.map (fn i => if (Seq.nth s i = 1) andalso f i then 1 else 0) s
        in
          from_dense_rep res NONE threshold
        end


  fun vertex_map g vs f needOut =
    if needOut then vertex_map_ g vs f
    else (vertex_foreach g vs; vs)

end

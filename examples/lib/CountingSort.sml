structure CountingSort :>
sig
  type 'a seq = 'a ArraySlice.slice

  val sort : 'a seq
          -> (int -> int)     (* bucket id of ith element *)
          -> int              (* number of buckets *)
          -> 'a seq * int seq (* sorted, bucket offsets *)
end =
struct

  structure A = Array
  structure AS = ArraySlice

  type 'a seq = 'a ArraySlice.slice

  val for = Util.for
  val loop = Util.loop
  val forBackwards = Util.forBackwards

  fun seqSortInternal In Out Keys Counts genOffsets =
    let
      val n = AS.length In
      val m = AS.length Counts
      (* val _ = print ("seqSortInternal n=" ^ Int.toString n ^ " m=" ^ Int.toString m ^ "\n") *)
      val sub = AS.sub
      val update = AS.update
    in
      for (0, m) (fn i => update (Counts,i,0));

      for (0, n) (fn i =>
        let
          val j = Keys i
          (* val _ = print ("update " ^ Int.toString j ^ "\n") *)
        in
          update (Counts, j, sub(Counts,j) + 1)
        end);

      (* print ("counts: " ^ Seq.toString Int.toString Counts ^ "\n"); *)

      loop (0, m) 0 (fn (s,i) =>
        let
          val t = sub(Counts, i)
        in
          update(Counts, i, s);
          s + t
        end);

      (* print ("counts: " ^ Seq.toString Int.toString Counts ^ "\n"); *)

      for (0, n) (fn i =>
        let
          val j = Keys(i)
          val k = sub(Counts, j)
        in
          update(Counts, j, k+1);
          update(Out, k, sub(In, i))
        end);

      if genOffsets then
        (forBackwards (0,m-1) (fn i =>
          update(Counts,i+1,sub(Counts,i)));
          update(Counts,0,0); 0)
      else
        loop (0, m) 0 (fn (s,i) =>
          let
            val t = sub(Counts, i)
          in
            (update(Counts, i, t - s); t)
          end)
    end

  fun seqSort(In, Keys, numBuckets) =
    let
      val Counts = AS.full(ForkJoin.alloc (numBuckets+1))
      val Out = AS.full(ForkJoin.alloc (AS.length In))
    in
      seqSortInternal In Out Keys (Seq.subseq Counts (0,numBuckets)) true;
      AS.update(Counts, numBuckets, AS.length In);
      (Out, Counts)
    end

  fun sort In Keys numBuckets =
    let
      val SeqThreshold = 8192
      val BlockFactor = 32
      val n = AS.length In
      (* pad to avoid false sharing *)
      val numBucketsPad = Int.max(numBuckets, 16)
      val sqrt = Real.floor(Math.sqrt(Real.fromInt n))
      val numBlocks = n div (numBuckets * BlockFactor)
    in
      if (numBlocks <= 1 orelse n < SeqThreshold) then
        seqSort(In, Keys, numBuckets)
      else let
        val blockSize = ((n-1) div numBlocks) + 1;
        val m = numBlocks * numBucketsPad
        val B = AS.full(ForkJoin.alloc(AS.length In))
        val Counts = AS.full(ForkJoin.alloc(m))
        val _ = ForkJoin.parfor 1 (0, numBlocks) (fn i =>
          let
            val start = Int.min(i * blockSize, n)
            val len = Int.min((i+1)* blockSize, n) - start
          in
            seqSortInternal
              (AS.subslice(In, start, SOME(len)))
              (AS.subslice(B, start, SOME(len)))
              (fn i => Keys(i+start))
              (AS.subslice(Counts,i*numBucketsPad,SOME(numBucketsPad)))
              false;
            ()
          end)
        val (sourceOffsets, _) = Seq.scan op+ 0 Counts
        val transCounts = SampleSort.transpose(Counts, numBlocks,
                                          numBucketsPad)
        val (destOffsets, _) = Seq.scan op+ 0 transCounts
        val C = SampleSort.transposeBlocks(B, sourceOffsets, destOffsets,
                                   Counts, numBlocks, numBucketsPad, n)
        val bucketOffsets =
          Seq.tabulate (fn i =>
              if (i = numBuckets) then n
              else AS.sub (destOffsets, i * numBlocks))
            (numBuckets+1)
      in
        (C, bucketOffsets)
      end
    end

end

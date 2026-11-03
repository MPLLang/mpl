structure StableMergeLowSpan:
sig
  type 'a seq = 'a ArraySlice.slice

  val writeMerge: ('a * 'a -> order) (* compare *)
                  -> 'a seq * 'a seq (* (sorted) sequences to merge *)
                  -> 'a seq (* output *)
                  -> unit

  val merge: ('a * 'a -> order) -> 'a seq * 'a seq -> 'a seq
end =
struct

  structure AS = ArraySlice
  type 'a seq = 'a AS.slice
  fun slice_idxs s (i, j) =
    AS.subslice (s, i, SOME (j - i))


  (* DoubleBinarySearch guarantees that it takes the _minimum_ number of
   * elements from the first argument. For stability, we want to take the
   * _maximum_ number of elements from s1; this is equivalent to taking the
   * minimum from s2. So, we can just swap the order of the arguments we
   * give to the search.
   *)
  fun split_count_take_max_left cmp (s1, s2) k =
    let val (i2, i1) = DoubleBinarySearch.split_count_slice cmp (s2, s1) k
    in (i1, i2)
    end


  val blockSizeFactor =
    CommandLineArgs.parseReal "MPLLib_StableMergeLowSpan_blockSizeFactor" 1000.0


  fun log2 x =
    Real64.Math.log10 (Real64.fromInt x) / Real64.Math.log10 2.0


  fun writeMerge cmp (s1, s2) output =
    let
      val n = AS.length s1 + AS.length s2
      val logn = if n <= 2 then 1.0 else log2 n
      val blockSize = Real64.ceil (blockSizeFactor * logn)
      val numBlocks = Util.ceilDiv n blockSize
    in
      ForkJoin.parfor 1 (0, numBlocks) (fn b =>
        let
          val start = blockSize * b
          val stop = Int.min (n, start + blockSize)

          val (i1, i2) = split_count_take_max_left cmp (s1, s2) start
          val (j1, j2) = split_count_take_max_left cmp (s1, s2) stop

          val piece1 = slice_idxs s1 (i1, j1)
          val piece2 = slice_idxs s2 (i2, j2)
          val piece_output = slice_idxs output (start, stop)
        in
          StableMerge.writeMerge cmp (piece1, piece2) piece_output
        end)
    end


  fun merge cmp (s1, s2) =
    let val out = AS.full (ForkJoin.alloc (AS.length s1 + AS.length s2))
    in writeMerge cmp (s1, s2) out; out
    end

end

(* Author: Guy Blelloch
 *
 * This file is basically the cache-oblivious sorting algorithm from:
 *
 * Low depth cache-oblivious algorithms.
 * Guy E. Blelloch, Phillip B. Gibbons and  Harsha Vardhan Simhadri.
 * Proc. ACM symposium on Parallelism in algorithms and architectures (SPAA), 2010
 *
 * The main difference is that it does not recurse (using quicksort instead)
 * and the merging with samples is sequential.
 *)

structure SampleSort :>
sig
  type 'a seq = 'a ArraySlice.slice

  (* transpose (matrix, numRows, numCols) *)
  val transpose : 'a seq * int * int -> 'a seq

  (* transposeBlocks (blockMatrix, srcOffsets, dstOffsets, counts, numRows, numCols, n) *)
  val transposeBlocks : 'a seq * int seq * int seq * int seq * int * int * int -> 'a seq

  val sort : ('a * 'a -> order) -> 'a seq -> 'a seq
end =
struct
  type 'a seq = 'a Seq.t

  structure A = Array
  structure AS = ArraySlice

  val sortInPlace = Quicksort.sortInPlace

  val sub = A.sub
  val update = A.update

  val par = ForkJoin.par
  val for = Util.for
  val parallelFor = ForkJoin.parfor

  fun for_l (lo, len) f = for (lo, lo + len) f

  fun matrixDandC baseCase (threshold, num_rows, num_cols) =
    let fun r(rs, rl, cs, cl) =
          if (rl*cl < threshold) then baseCase(rs, rl, cs, cl)
          else if (cl > rl) then
            (par (fn () => r(rs, rl, cs, cl div 2),
                  fn () => r(rs, rl, cs + (cl div 2), cl - (cl div 2))); ())
          else
            (par (fn () => r(rs, rl div 2, cs, cl),
                  fn () => r(rs + (rl div 2), rl - (rl div 2), cs, cl)); ())
    in r(0, num_rows, 0, num_cols) end

  (* transposes a matrix *)
  fun transpose(S, num_rows, num_cols) =
    let
      val seq_threshold = 8000
      val (SS, offset, n) = AS.base S
      val _ = if (AS.length S) <> (num_rows * num_cols) then raise Size else ()
      val R = ForkJoin.alloc (num_rows * num_cols)
      fun baseCase(row_start, row_len, col_start, col_len) =
          for_l (row_start, row_len) (fn i =>
            for_l (col_start, col_len) (fn j =>
               update(R, j * num_rows + i, sub(SS,(i*num_cols + j + offset)))))
    in (matrixDandC baseCase (seq_threshold, num_rows, num_cols);
        AS.full(R))
    end

  (* transposes a matrix of blocks given source and destination pairs *)
  fun transposeBlocks(S, source_offsets, dest_offsets, counts, num_rows, num_cols, n) =
    let
      val seq_threshold = 500
      val (SS, offset, n) = AS.base S
      val R = ForkJoin.alloc n
      fun baseCase(row_start, row_len, col_start, col_len) =
          for (row_start, row_start + row_len) (fn i =>
            for (col_start, col_start + col_len) (fn j => let
                   val pa = offset + AS.sub (source_offsets, i*num_cols + j)
                   val pb = Seq.nth dest_offsets (j*num_rows + i)
                   val l = Seq.nth counts (i*num_cols + j)
                 in for (0,l) (fn k => update(R,pb+k,sub(SS, pa + k))) end))
    in (matrixDandC baseCase (seq_threshold, num_rows, num_cols);
        AS.full(R))
    end

  (* merges a sequence of elements A with the samples S, putting counts in C *)
  fun mergeWithSamples cmp (A, S, C) =
    let
      val num_samples = AS.length S
      val n = AS.length A
      fun merge(i,j) =
        if (j = num_samples) then AS.update(C,j,n-i)
        else
          let fun merge'(i) = if (i < n andalso cmp(AS.sub (A, i), AS.sub (S, j)) = LESS)
                              then merge'(i+1)
                              else i
              val k = merge'(i)
              val _ = AS.update(C, j, k-i)
          in merge(k,j+1) end
  in merge(0,0) end

  fun sort cmp A =
   let
     val n = AS.length A

     (* parameters used in algorithm *)
     val bucket_quotient = 3
     val block_quotient = 2
     val sqrt = Real.floor(Math.sqrt(Real.fromInt n))
     val num_blocks = sqrt div block_quotient
     val block_size = ((n-1) div num_blocks) + 1
     val num_buckets = (sqrt div bucket_quotient) + 1
     val over_sample = 1 + ((n div num_buckets) div 500)
     val sample_size = num_buckets * over_sample
     val sample_stride = n div sample_size
     val m = num_blocks*num_buckets

     (* val _ = print ("num_blocks  " ^ Int.toString num_blocks ^ "\n")
     val _ = print ("num_buckets " ^ Int.toString num_buckets ^ "\n")
     val _ = print ("sample_size " ^ Int.toString sample_size ^ "\n")
     val _ = print ("over_sample " ^ Int.toString over_sample ^ "\n")
     val _ = print ("m " ^ Int.toString m ^ "\n") *)

     (* val t0 = Time.now () *)

     (* sort a sample of keys *)
     val sample = Seq.tabulate (fn i => AS.sub (A, i*sample_stride)) sample_size
     val _ = sortInPlace cmp sample

     (* val t1 = Time.now ()
     val _ = print ("sorted sample  " ^ Time.fmt 4 (Time.- (t1, t0)) ^ "\n") *)

     (* take a subsample *)
     val sub_sample = Seq.tabulate (fn i => AS.sub (sample, (i+1)*over_sample)) (num_buckets-1)

     (* val t2 = Time.now ()
     val _ = print ("subsample      " ^ Time.fmt 4 (Time.- (t2, t1)) ^ "\n") *)

     val counts = AS.full (ForkJoin.alloc m)
     val B = AS.full (ForkJoin.alloc n)

     (* sort each block and merge with the pivots, giving a count of the number
        of keys between each pivot in each block *)
     val _ =
     parallelFor 1 (0,num_blocks) (fn i =>
       let
         val start = i * block_size
         val len = Int.min((i+1)* block_size,n) - start
         (* copy into B to avoid changing A *)
         val _ = for (start, start+len) (fn j => AS.update(B, j, AS.sub (A, j)))
         val B' = Seq.subseq B (start, len)
         val _ = sortInPlace cmp B'
         val counts' = Seq.subseq counts (i*num_buckets, num_buckets)
         val _ = mergeWithSamples cmp (B', sub_sample, counts')
       in () end)

     (* val t3 = Time.now ()
     val _ = print ("sort blocks    " ^ Time.fmt 4 (Time.- (t3, t2)) ^ "\n") *)

     (*  scan across the counts to get offset of each source bucket within each block *)
     val (source_offsets,_) = Seq.scan op+ 0 counts

     (*  transpose and scan across the counts to get offset of each
         destination within each bucket *)
     val tcounts = transpose(counts,num_blocks,num_buckets)
     val (dest_offsets,_) = Seq.scan op+ 0 tcounts

     (*  move data to correct destination *)
     val C = transposeBlocks(B, source_offsets, dest_offsets,
                             counts, num_blocks, num_buckets, n)

     (* val t4 = Time.now ()
     val _ = print ("transpose data " ^ Time.fmt 4 (Time.- (t4, t3)) ^ "\n") *)

     (* get the start location of each bucket *)
    fun bucket_offset i =
      if (i = num_buckets) then n
      else AS.sub (dest_offsets, i * num_blocks)

     (* sort the buckets *)
     val _ =
     parallelFor 1 (0, num_buckets) (fn i =>
       let
         val start = bucket_offset i
         val len = bucket_offset (i+1) - start
         (* val start = AS.sub (bucket_offsets, i)
         val len = (AS.sub (bucket_offsets, i+1)) - start *)
         val _ = sortInPlace cmp (Seq.subseq C (start,len))
       in () end)

     (* val t5 = Time.now ()
     val _ = print ("sort buckets   " ^ Time.fmt 4 (Time.- (t5, t4)) ^ "\n") *)

   in C end
end


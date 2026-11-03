structure SeqifiedMerge:
sig
  val merge: ('a * 'a -> order) -> 'a Seq.t * 'a Seq.t -> 'a Seq.t
end =
struct

  val serialGrain = CommandLineArgs.parseInt "MPLLib_Merge_serialGrain" 4000

  val unsafe_at_leaves = CommandLineArgs.parseFlag
    "MPLLib_SeqifiedMerge_unsafe_at_leaves"

  fun merge_loop cmp (s1, s2) out =
    if Seq.length s1 = 0 then
      Seqifier.put (out, s2)
    else if Seqifier.length out <= serialGrain then
      if unsafe_at_leaves then
        (* this is semantically safe (it does not violate any of the internal
         * invariants of the Seqifier libary), but of course it appears to be
         * syntactically unsafe from the perspective of the library interface.
         * We are careful here to make sure we don't do anything really bad.
         *)
        ( Merge.writeMergeSerial cmp (s1, s2)
            (Seqifier.unsafe_view_contents out)
        ; Seqifier.unsafe_mark_put out
        )
      else
        (* this is completely safe, at a small performance cost, due to the
         * need to create an intermediate sequence for the result of the
         * `mergeSerial`.
         *)
        Seqifier.put (out, Merge.mergeSerial cmp (s1, s2))
    else
      let
        val n1 = Seq.length s1
        val n2 = Seq.length s2
        val mid1 = n1 div 2
        val pivot = Seq.nth s1 mid1
        val mid2 = BinarySearch.search cmp s2 pivot

        val (outl, out_tail) = Seqifier.split_at (out, mid1 + mid2)
        val (outm, outr) = Seqifier.split_at (out_tail, 1)
        val outm = Seqifier.put (outm, Seq.subseq s1 (mid1, 1))
        val l1 = Seq.take s1 mid1
        val r1 = Seq.drop s1 (mid1 + 1)
        val l2 = Seq.take s2 mid2
        val r2 = Seq.drop s2 mid2
        val (outl, outr) =
          ForkJoin.par (fn _ => merge_loop cmp (l1, l2) outl, fn _ =>
            merge_loop cmp (r1, r2) outr)
      in
        Seqifier.append (outl, Seqifier.append (outm, outr))
      end

  fun merge cmp (s1, s2) =
    let val out = Seqifier.init_expect_length (Seq.length s1 + Seq.length s2)
    in Seqifier.finalize (merge_loop cmp (s1, s2) out)
    end

end

structure FlattenMerge:
sig
  val merge: ('a * 'a -> order) -> 'a Seq.t * 'a Seq.t -> 'a Seq.t
end =
struct

  val serialGrain = CommandLineArgs.parseInt "MPLLib_Merge_serialGrain" 4000

  fun merge_loop cmp (s1, s2) =
    if Seq.length s1 = 0 then
      TFlatten.leaf s2
    else if Seq.length s1 + Seq.length s2 <= serialGrain then
      TFlatten.leaf (Merge.mergeSerial cmp (s1, s2))
    else
      let
        val n1 = Seq.length s1
        val n2 = Seq.length s2
        val mid1 = n1 div 2
        val pivot = Seq.nth s1 mid1
        val mid2 = BinarySearch.search cmp s2 pivot

        val l1 = Seq.take s1 mid1
        val r1 = Seq.drop s1 (mid1 + 1)
        val l2 = Seq.take s2 mid2
        val r2 = Seq.drop s2 mid2

        val (outl, outr) =
          ForkJoin.par (fn _ => merge_loop cmp (l1, l2), fn _ =>
            merge_loop cmp (r1, r2))
      in
        TFlatten.node
          (TFlatten.node (outl, TFlatten.leaf (Seq.singleton pivot)), outr)
      end

  fun merge cmp (s1, s2) =
    TFlatten.flatten (merge_loop cmp (s1, s2))

end

functor CollectHash (structure K: KEY structure V: VALUE):
sig
  val collect: (K.t * V.t) Seq.t -> (K.t * V.t) Seq.t
end =
struct

  structure T = HashTable (structure K = K structure V = V)


  fun collect kvs =
    let
      val t = T.make {capacity = Seq.length kvs} (* very rough upper bound *)

      val _ = ForkJoin.parfor 100 (0, Seq.length kvs) (fn i =>
        T.insert_combine t (Seq.nth kvs i))

      val contents = T.unsafe_view_contents t
      val results =
        ArraySlice.full
          (SeqBasis.filter 1000 (0, DelayedSeq.length contents)
             (fn i => valOf (DelayedSeq.nth contents i))
             (fn i => Option.isSome (DelayedSeq.nth contents i)))

      val sorted =
        Mergesort.sort (fn ((k1, v1), (k2, v2)) => K.cmp (k1, k2)) results
    in
      sorted
    end

end

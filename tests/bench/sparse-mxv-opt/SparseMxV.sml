structure SparseMxV =
struct

  fun sparseMxV (mat: (int * real) Seq.t Seq.t) (vec: real Seq.t) =
    let
      fun f (i,x) = (Seq.nth vec i) * x
      fun rowSum r =
        SeqBasis.reduce 5000 op+ 0.0 (0, Seq.length r) (fn i => f(Seq.nth r i))
    in
      ArraySlice.full (SeqBasis.tabulate 100 (0, Seq.length mat) (fn i =>
        rowSum (Seq.nth mat i)))
    end

end

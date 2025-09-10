functor MkMXV (Seq : SEQUENCE) =
struct

  structure ASeq = ArraySequence
  type 'a seq = 'a ASeq.t

  fun sparseMxV (mat : (int * real) seq seq) (vec : real seq) =
    let
      fun f (i,x) = (ASeq.nth vec i) * x
      fun rowSum r =
        Seq.reduce op+ 0.0 (Seq.map f (Seq.fromArraySeq r))
    in
      Seq.toArraySeq (Seq.map rowSum (Seq.fromArraySeq mat))
    end

end

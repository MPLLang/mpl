structure Integrate =
struct

  fun integrate f (s, e) n =
    let
      val delta = (e - s) / (Real.fromInt n)
      val s' = s + delta / 2.0
    in
      delta
      *
      SeqBasis.reduce 5000 op+ 0.0 (0, n) (fn i =>
        f (s' + (Real.fromInt i) * delta))
    end

end

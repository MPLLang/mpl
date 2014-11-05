functor Nonblocked (A: DENSEARG) : DENSE =
struct

  open A

  type v = real t
  type m = v t

  fun vvm _ (a, b) = 
    let in
      foldInt (length a) op+ (fn i => sub (a, i) * sub (b, i)) 0.0 (length a)
    end

  fun mvm maxSeq (m, a) = 
    let 
      val def = ~1.0
    in
      tabulate maxSeq (fn i => vvm maxSeq (sub (m, i), a))
               def (length m)
    end

  fun mmm maxSeq (m, n) =
    let 
      val def = tabulate maxSeq (fn _ => ~1.0) ~1.0 0
    in
      tabulate maxSeq (fn i => mvm maxSeq (n, sub (m, i))) 
               def (length m)
    end

end

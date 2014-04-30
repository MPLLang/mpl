functor Strided (structure A: DENSEARG
                 structure S: sig val stride : int end) : DENSE =
struct

  open A

  type v = real t
  type m = v t

  (* copied from Nonblocked *)
  fun vvm _ (a, b) = 
    let in
      foldInt (length a) op+ (fn i => sub (a, i) * sub (b, i)) 0.0 (length a)
    end

  (* returns an array of length ceil((len m)/stride) where each element is the
    dot product of the i^th slices of m and a *)
  fun mvm maxSeq (m, a) = 
    let 
      val def = tabulate maxSeq (fn _ => ~1.0) ~1.0 0
      val l = ceil (Real.fromInt (length m) / Real.fromInt S.stride)
    in
      tabulate maxSeq
               (* i^th stride *)
               (fn i => 
                   let 
                     val k = if i = l - 1 then NONE else SOME S.stride
                     val a = slice (a, i * S.stride, k)
                   in
                     tabulate maxSeq 
                              (fn j => 
                                  vvm maxSeq (slice (sub (m, j), i * S.stride, k), a))
                              ~1.0 (length m)
                   end)
               def l
    end

  fun mmm maxSeq (m, n) =
    let 
      val def = tabulate maxSeq (fn _ => ~1.0) ~1.0 0
      val def' = tabulate maxSeq (fn _ => def) def 0

      val p = tabulate maxSeq (fn i => mvm maxSeq (n, sub (m, i))) 
                       def' (length m)
    in
      tabulate maxSeq 
               (fn i => 
                   let
                     val a = sub (p, i)
                   in
                     tabulate maxSeq 
                              (* sum the j^th element of each subarray *)
                              (fn j => foldInt maxSeq op+ (fn k => sub (sub (a, k), j)) 0.0 (length a))
                              ~1.0 (length m)
                   end)
               def (length m)
    end

end

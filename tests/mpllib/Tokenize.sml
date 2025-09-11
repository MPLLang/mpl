structure Tokenize:
sig
  val tokenRanges: (char -> bool) -> char Seq.t -> int * (int -> (int * int))

  val tokensSeq: (char -> bool) -> char Seq.t -> (char Seq.t) Seq.t

  val tokens: (char -> bool) -> char Seq.t -> string Seq.t
end =
struct

  fun tokenRanges f s =
    let
      val n = Seq.length s
      fun check i =
        if (i = n) then not (f(Seq.nth s (n-1)))
        else if (i = 0) then not (f(Seq.nth s 0))
        else let val i1 = f (Seq.nth s i)
                 val i2 = f (Seq.nth s (i-1))
             in (i1 andalso not i2) orelse (i2 andalso not i1) end
      val ids = ArraySlice.full
        (SeqBasis.filter 10000 (0, n+1) (fn i => i) check)
      val count = (Seq.length ids) div 2
    in
      (count, fn i => (Seq.nth ids (2*i), Seq.nth ids (2*i+1)))
    end

  fun tokensSeq f s =
    let
      val (n, g) = tokenRanges f s
      fun token i =
        let
          val (lo, hi) = g i
        in
          Seq.subseq s (lo, hi-lo)
        end
    in
      Seq.tabulate token n
    end

  fun tokens f s =
    let
      val (n, g) = tokenRanges f s
      fun token i =
        let
          val (lo, hi) = g i
          val chars = Seq.subseq s (lo, hi-lo)
        in
          CharVector.tabulate (Seq.length chars, Seq.nth chars)
        end
    in
      ArraySlice.full (SeqBasis.tabulate 1024 (0, n) token)
    end
end

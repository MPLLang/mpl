structure Tokenize:
sig
  val tokens: (char -> bool) -> char Seq.t -> (char Seq.t) Seq.t
end =
struct

  fun tokens f s =
    let
      val n = Seq.length s
      val indices = Seq.tabulate (fn i => i) (n+1)
      fun check i =
        if (i = n) then not (f(Seq.nth s (n-1)))
        else if (i = 0) then not (f(Seq.nth s 0))
        else let val i1 = f (Seq.nth s i)
                 val i2 = f (Seq.nth s (i-1))
             in (i1 andalso not i2) orelse (i2 andalso not i1) end
      val ids = ArraySlice.full
        (SeqBasis.filter 10000 (0, n+1) (fn i => i) check)
      val res = Seq.tabulate (fn i =>
        let
          val (lo, hi) = (Seq.nth ids (2*i), Seq.nth ids (2*i+1))
        in
          Seq.subseq s (lo, hi-lo)
        end)
        ((Seq.length ids) div 2)
    in
      res
    end

end

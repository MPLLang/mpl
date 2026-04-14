functor MkLineFit (Seq : SEQUENCE) =
struct

  fun linefit (points : (real * real) ArraySequence.t) =
    let
      val points = Seq.fromArraySeq points

      val n = Real.fromInt (Seq.length points)
      fun sumPair((x1,y1),(x2,y2)) = (x1 + x2, y1 + y2)
      fun sum f = Seq.reduce sumPair (0.0, 0.0) (Seq.map f points)
      fun square x = x * x
      val (xsum, ysum) = sum (fn (x,y) => (x,y))
      val (xa, ya) = (xsum/n, ysum/n)
      val (Stt, bb) = sum (fn (x,y) => (square(x - xa), (x - xa) * y))
      val b = bb / Stt
      val a = ya - xa * b
    in
      (a, b)
    end

end

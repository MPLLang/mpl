structure LineFit =
struct

  fun linefit (points : (real * real) Seq.t) =
    let

      val n = Real.fromInt (Seq.length points)
      fun sumPair((x1,y1),(x2,y2)) = (x1 + x2, y1 + y2)
      fun sum f =
        SeqBasis.reduce 5000 sumPair (0.0, 0.0)
          (0, Seq.length points)
          (f o Seq.nth points)
      (* Seq.reduce sumPair (0.0, 0.0) (Seq.map f points) *)

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

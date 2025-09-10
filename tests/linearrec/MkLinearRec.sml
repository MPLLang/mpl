functor MkLinearRec (Seq: SEQUENCE) =
struct

  fun combine ((x1, y1), (x2, y2)) =
    (x1 * x2, y1 * x2 + y2)

  val id = (1.0, 0.0)

  fun linearRec s =
    Seq.toArraySeq (Seq.map #2 (Seq.scanIncl combine id (Seq.fromArraySeq s)))

end

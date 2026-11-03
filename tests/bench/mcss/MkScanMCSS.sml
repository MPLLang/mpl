functor MkScanMCSS (Seq: SEQUENCE) =
struct

  fun mcss (s: real ArraySequence.t) : real =
    let
      val s = Seq.fromArraySeq s
      val t = Util.startTiming ()

      val p = Seq.scanIncl op+ 0.0 s
      val t = Util.tick t "plus scan"

      val (m, _) = Seq.scan Real.min Real.posInf p
      val t = Util.tick t "min scan"

      val b = Seq.zipWith op- (p, m)
      val t = Util.tick t "zipWith"

      val result = Seq.reduce Real.max Real.negInf b
      val t = Util.tick t "reduce"
    in
      result
    end

end

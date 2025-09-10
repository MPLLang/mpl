functor MkMapReduceMCSS (Seq : SEQUENCE) =
struct

  val max = Real.max

  fun combine((l1,r1,b1,t1),(l2,r2,b2,t2)) =
    (max(l1, t1+l2),
     max(r2, r1+t2),
     max(r1+l2, max(b1,b2)),
     t1+t2)

  val id = (0.0, 0.0, 0.0, 0.0)

  fun singleton v =
    let
      val vp = max (v, 0.0)
    in
      (vp, vp, vp, v)
    end

  fun mcss (s : real ArraySequence.t) : real =
    let
      val (_,_,b,_) =
        Seq.reduce combine id (Seq.map singleton (Seq.fromArraySeq s))
    in
      b
    end

end

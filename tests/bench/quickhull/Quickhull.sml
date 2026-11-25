structure Quickhull :
sig
  val hull : (real * real) Seq.t -> int Seq.t
end =
struct

  structure AS = ArraySlice
  structure G = Geometry2D
  structure Tree = TreeSeq

  fun hull pts =
    let
      fun pt i = Seq.nth pts i
      fun dist p q i = G.Point.triArea (p, q, pt i)
      fun max ((i, di), (j, dj)) =
        if di > dj then (i, di) else (j, dj)
      fun x i = #1 (pt i)

      fun aboveLine p q i = (dist p q i > 0.0)

      fun parHull idxs l r =
        if Seq.length idxs < 2 then
          Tree.fromArraySeq idxs
        (* if DS.length idxs <= 2048 then
          seqHull idxs l r *)
        else
          let
            val lp = pt l
            val rp = pt r
            fun d i = dist lp rp i

            (* val idxs = DS.fromArraySeq idxs *)

            val (mid, _) = SeqBasis.reduce 10000 max (~1, Real.negInf)
              (0, Seq.length idxs) (fn i => (Seq.nth idxs i, d (Seq.nth idxs i)))
            (* val distances = DS.map (fn i => (i, d i)) idxs
            val (mid, _) = DS.reduce max (~1, Real.negInf) distances *)

            val midp = pt mid

            fun flag i =
              if aboveLine lp midp i then Split.Left
              else if aboveLine midp rp i then Split.Right
              else Split.Throwaway
            val (left, right) =
              Split.parSplit idxs (Seq.map flag idxs)
              (* (DS.force (DS.map flag idxs)) *)

            fun doLeft () = parHull left l mid
            fun doRight () = parHull right mid r
            val (leftHull, rightHull) =
              if Seq.length left + Seq.length right <= 2048
              then (doLeft (), doRight ())
              else ForkJoin.par (doLeft, doRight)
          in
            Tree.append (leftHull,
                         (Tree.append (Tree.$ mid, rightHull)))
          end

      (* val tm = Util.startTiming () *)

      (* val allIdx = DS.tabulate (fn i => i) (Seq.length pts) *)

      (* This is faster than doing two reduces *)
      (* val (l, r) = DS.reduce
        (fn ((l1, r1), (l2, r2)) =>
          (if x l1 < x l2 then l1 else l2,
           if x r1 > x r2 then r1 else r2))
        (0, 0)
        (DS.map (fn i => (i, i)) allIdx) *)

      val (l, r) = SeqBasis.reduce 10000
        (fn ((l1, r1), (l2, r2)) =>
          (if x l1 < x l2 then l1 else l2,
           if x r1 > x r2 then r1 else r2))
        (0, 0)
        (0, Seq.length pts)
        (fn i => (i, i))

      (* val tm = Util.tick tm "endpoints" *)

      val lp = pt l
      val rp = pt r

      fun flag i =
        let
          val d = dist lp rp i
        in
          if d > 0.0 then Split.Left
          else if d < 0.0 then Split.Right
          else Split.Throwaway
        end
      val (above, below) =
        (* Split.parSplit allIdx (DS.force (DS.map flag allIdx)) *)
        Split.parSplit
          (Seq.tabulate (fn i => i) (Seq.length pts))
          (Seq.tabulate flag (Seq.length pts))

      (* val tm = Util.tick tm "above/below filter" *)

      val (above, below) = ForkJoin.par
        (fn _ => parHull above l r,
         fn _ => parHull below r l)

      (* val tm = Util.tick tm "quickhull" *)

      val hullt =
        Tree.append
          (Tree.append (Tree.$ l, above),
           Tree.append (Tree.$ r, below))

      val result = Tree.toArraySeq hullt

      (* val tm = Util.tick tm "flatten" *)
    in
      result
    end

end

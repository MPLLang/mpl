functor MkQuickhull (Seq: SEQUENCE):
sig
  type 'a aseq = 'a ArraySequence.t
  val hull: (real * real) aseq -> int aseq
end =
struct

  structure AS = ArraySlice
  structure ASeq = ArraySequence
  type 'a aseq = 'a ASeq.t

  structure G = Geometry2D
  structure Tree = TreeSeq

  structure Split = MkSplit (Seq)

  fun hull pts =
    let
      fun pt i = ASeq.nth pts i
      fun dist p q i = G.Point.triArea (p, q, pt i)
      fun max ((i, di), (j, dj)) =
        if di > dj then (i, di) else (j, dj)
      fun x i = #1 (pt i)

      fun aboveLine p q i = (dist p q i > 0.0)

      fun parHull idxs l r =
        if ASeq.length idxs < 2 then
          Tree.fromArraySeq idxs
        else
          let
            val lp = pt l
            val rp = pt r
            fun d i = dist lp rp i

            val idxs = Seq.fromArraySeq idxs

            val distances = Seq.map (fn i => (i, d i)) idxs
            val (mid, _) = Seq.reduce max (~1, Real.negInf) distances

            val midp = pt mid

            fun flag i =
              if aboveLine lp midp i then Split.Left
              else if aboveLine midp rp i then Split.Right
              else Split.Throwaway
            val (left, right) =
              Split.parSplit idxs (Seq.force (Seq.map flag idxs))

            fun doLeft () = parHull left l mid
            fun doRight () = parHull right mid r
            val (leftHull, rightHull) =
              if ASeq.length left + ASeq.length right <= 2048
              then (doLeft (), doRight ())
              else ForkJoin.par (doLeft, doRight)
          in
            Tree.append (leftHull,
                         (Tree.append (Tree.$ mid, rightHull)))
          end

      (* val tm = Util.startTiming () *)

      val allIdx = Seq.tabulate (fn i => i) (ASeq.length pts)

      (* This is faster than doing two reduces *)
      val (l, r) = Seq.reduce
        (fn ((l1, r1), (l2, r2)) =>
          (if x l1 < x l2 then l1 else l2,
           if x r1 > x r2 then r1 else r2))
        (0, 0)
        (Seq.map (fn i => (i, i)) allIdx)

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
        Split.parSplit allIdx (Seq.force (Seq.map flag allIdx))

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

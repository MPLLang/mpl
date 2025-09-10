structure Skyline =
struct
  type 'a seq = 'a Seq.t
  type skyline = (int * int) Seq.t

  fun singleton (l, h, r) = Seq.fromList [(l, h), (r, 0)]

  fun combine (sky1, sky2) =
    let
      val lMarked = Seq.map (fn (x, y) => (x, SOME y, NONE)) sky1
      val rMarked = Seq.map (fn (x, y) => (x, NONE, SOME y)) sky2

      fun cmp ((x1, _, _), (x2, _, _)) = Int.compare (x1, x2)
      val merged = Merge.merge cmp (lMarked, rMarked)

      fun copy (a, b) = case b of SOME _ => b | NONE => a
      fun copyFused ((x1, yl1, yr1), (x2, yl2, yr2)) =
        (x2, copy (yl1, yl2), copy (yr1, yr2))

      val allHeights = Seq.scanIncl copyFused (0,NONE,NONE) merged

      fun squish (x, y1, y2) =
        (x, Int.max (Option.getOpt (y1, 0), Option.getOpt (y2, 0)))
      val sky = Seq.map squish allHeights

      (*fun isUnique (i, (x, h)) =
        i = 0 orelse let val (_, prevh) = Seq.nth sky (i-1) in h <> prevh end*)
      (*val sky = Seq.filterIdx isUnique sky*)
    in
      sky
    end

  fun skyline g bs =
    let
      fun skyline' bs =
        case Seq.length bs of
          0 => Seq.empty ()
        | 1 => singleton (Seq.nth bs 0)
        | n =>
            let
              val half = n div 2
              val sfL = fn _ => skyline' (Seq.take bs half)
              val sfR = fn _ => skyline' (Seq.drop bs half)
            in
              if Seq.length bs <= g then
                combine (sfL (), sfR ())
              else
                combine (ForkJoin.par (sfL, sfR))
            end

      val sky = skyline' bs

      fun isUnique (i, (x, h)) =
        i = 0 orelse let val (_, prevh) = Seq.nth sky (i-1) in h <> prevh end
      val sky = Seq.filterIdx isUnique sky
    in
      sky
    end

end

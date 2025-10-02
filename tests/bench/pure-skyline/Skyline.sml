structure Skyline =
struct
  type 'a seq = 'a PureSeq.t
  type skyline = (int * int) PureSeq.t

  fun singleton (l, h, r) = PureSeq.fromList [(l, h), (r, 0)]

  fun combine (sky1, sky2) =
    let
      val lMarked = PureSeq.map (fn (x, y) => (x, SOME y, NONE)) sky1
      val rMarked = PureSeq.map (fn (x, y) => (x, NONE, SOME y)) sky2

      fun cmp ((x1, _, _), (x2, _, _)) = Int.compare (x1, x2)
      val merged = PureSeq.merge cmp (lMarked, rMarked)

      fun copy (a, b) = case b of SOME _ => b | NONE => a
      fun copyFused ((x1, yl1, yr1), (x2, yl2, yr2)) =
        (x2, copy (yl1, yl2), copy (yr1, yr2))

      val allHeights = PureSeq.scanIncl copyFused (0,NONE,NONE) merged

      fun squish (x, y1, y2) =
        (x, Int.max (Option.getOpt (y1, 0), Option.getOpt (y2, 0)))
      val sky = PureSeq.map squish allHeights
    in
      sky
    end

  fun skyline g bs =
    let
      fun skyline' bs =
        case PureSeq.length bs of
          0 => PureSeq.empty ()
        | 1 => singleton (PureSeq.nth bs 0)
        | n =>
            let
              val half = n div 2
              val sfL = fn _ => skyline' (PureSeq.take bs half)
              val sfR = fn _ => skyline' (PureSeq.drop bs half)
            in
              if PureSeq.length bs <= g then
                combine (sfL (), sfR ())
              else
                combine (ForkJoin.par (sfL, sfR))
            end

      val sky = skyline' bs

      fun isUnique (i, (x, h)) =
        i = 0 orelse let val (_, prevh) = PureSeq.nth sky (i-1) in h <> prevh end
      val sky = PureSeq.filterIdx isUnique sky
    in
      sky
    end

end

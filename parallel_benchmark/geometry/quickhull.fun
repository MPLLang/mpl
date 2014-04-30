functor QuickHull (A : GEOMETRYARG) : HULL =
struct

  open A
  structure V = Vector

  (* Given two points on the hull (p1, p2), return the list of points on the
    hull between p1 (inclusive) and p2 (exclusive) in order. *)
  fun split maxSeq (ps, (p1, p2)) =
      let
        (* compute the "distance" from the line as well as the points above
          the line *)
        fun finddist (p, (ps, (p0, d0))) = 
            let 
              val d = V.cross (p, (p1, p2))
            in
              (if d > 0.0 then p::ps else ps,
               if Real.> (d, d0)
               then (p, d)
               else (p0, d0))
            end
        (* convert from list to seq *)
        fun seqToPar (ps, (p, d)) = (fromList maxSeq ps, (p, d))
        (* take the larger "distance" and merge the lines *)
        fun mergedist ((ps1, (p1, d1)), (ps2, (p2, d2))) = 
            (append maxSeq (ps1, ps2), 
             if Real.> (d1, d2)
             then (p1, d1) 
             else (p2, d2))
        (* points above the line, plus the point furthest above the line *)
        val (ps, (p0, _)) = fold maxSeq
                                 mergedist
                                 seqToPar
                                 finddist
                                 (nil, (p1, 0.0))
                                 ps
        (* include p1 *)
        val ps = append maxSeq (singleton p1, ps)
        val l = size ps
      in
        if l <= 2 then ps
        else if l <= maxSeq then
          append maxSeq (split maxSeq (ps, (p1, p0)),
                         split maxSeq (ps, (p0, p2)))
        else
          append maxSeq (fork (fn () => split maxSeq (ps, (p1, p0)),
                               fn () => split maxSeq (ps, (p0, p2))))
      end

  fun hull maxSeq ps =
      let 
        (* Find the extreme points in the x-dimension *)
        fun mergeminmax ((p1 as (min1, _), 
                          p1' as (max1, _)), 
                         (p2 as (min2, _),
                          p2' as (max2, _))) =
            (if min1 < min2 then p1 else p2,
             if max1 > max2 then p1' else p2')

        val (min, max) = fold maxSeq
                              mergeminmax 
                              (fn x => x) 
                              (fn (p, minmax) => mergeminmax ((p, p), minmax))
                              ((Real.posInf, 0.0), (Real.negInf, 0.0))
                              ps
      in
        append maxSeq (fork (fn () => split maxSeq (ps, (min, max)),
                             fn () => split maxSeq (ps, (max, min))))
      end
end

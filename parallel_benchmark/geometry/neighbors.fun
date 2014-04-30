(* XXX NOT FINISHED *)

functor Neighbors (A : GEOMETRYARG) =
struct

  open A
  structure V = Vector


  fun neighbors maxSeq ps =
      let 
        fun qsort ps = 
            let 
              val l = size ps
            if l < 2 then ps
            else
              let
                val (ls, gs) = split ps (sub (ps, 0))
                val (ls, gs) = if l > maxSeq then
                                 fork (fn () => qsort ls,
                                       fn () => qsort gs)
                               else
                                 (qsort ls, qsort gs)
              in
                append (ls, gs)
              end

        val ps = sort ps

        (* Find the extreme points in the x-dimension *)
        fun mergeminmax ((p1 as (min1, _), 
                          p1' as (max1, _)), 
                         (p2 as (min2, _),
                          p2' as (max2, _))) =
            (if min1 < min2 then p1 else p2,
             if max1 > max2 then p1' else p2')

        fun loop ps =
            if size ps < 2 then NONE
            else if size ps = 2 then SOME (sub (ps, 0), sub (ps, 1))
            else
              let

              in
                
              end


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

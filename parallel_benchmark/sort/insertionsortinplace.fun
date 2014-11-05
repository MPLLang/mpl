functor InsertionSortInplace (A: SORTARG) : SORTINPLACE =
struct

  open A

  (* PERF in place? *)
  fun sort _ (toLeft, a, a') =
      let
        val b = if toLeft then a else a'

        fun outer i =
          if i = length a then ()
          else 
            let 
              val x = sub (a, i)
              fun inner j = 
                  if j = 0 orelse sub (b, j - 1) < x then update (b, j, x)
                  else (update (b, j, sub (b, j - 1)); 
                        inner (j - 1))
            in
              inner i;
              outer (i + 1)
            end

      in
        outer 0
      end
end

functor MergeSeqInplace (A: SORTARG) : MERGEINPLACE =
struct

  open A

  (* merges two sorted inputs sequencially *)
  fun merge _ (b, c) a =
    let 
      fun loop (k, i, j) = 
        if i = length b then
          copy { src = slice (c, j, NONE), dst = a, di = k }
        else if j = length c then
          copy { src = slice (b, i, NONE), dst = a, di = k }
        else
          let 
            val x = sub (b, i)
            val y = sub (c, j)
          in
            if x < y then 
              (update (a, k, x);
               loop (k + 1, i + 1, j))
            else 
              (update (a, k, y);
               loop (k + 1, i, j + 1))
          end
          
    in 
      loop (0, 0, 0)
    end

end

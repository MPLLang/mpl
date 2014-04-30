functor MergeSeq (A: SORTARG) : MERGE =
struct

  open A

  (* takes a sorted slice and an element x; returns two lists of A.t such that
    every element in the first is <= x and those in the second >= x *)
  fun split _ x c =
    let
      fun loop c = 
          let
            val l = length c
          in
            if l = 0 then ([empty], [empty])
            else if l = 1 then
              if sub (c, 0) <= x then ([c], [empty]) else ([empty], [c])
            else
              let
                val == = Real.== infix 4 ==
                val (c1, c2) = halve c
                val y = sub (c2, 0)
              in
                if y == x then ([c1], [c2])
                else if y < x then 
                  let
                    val (c21, c22) = loop c2
                  in
                    (c1 :: c21, c22)
                  end
                else (* y > x *)
                  let
                    val (c11, c12) = loop c1
                  in
                    (c11, c12 @ [c2])
                  end
              end
          end
    in
      loop c
    end

  (* merges two sorted inputs sequentially *)
  fun merge _ _ (b, c) =
    let 
      val b_length = length b
      val c_length = length c

      fun loop (k, (i, j)) =
          if i = b_length then
            (sub (c, j), (i, j+1))
          else if j = c_length then
            (sub (b, i), (i+1, j))
          else
            let 
              val x = sub (b, i)
              val y = sub (c, j)
            in
              if x < y then 
                (x, (i+1, j))
              else
                (y, (i, j+1))
            end
    in 
      #1 (unfoldi NONE (b_length + c_length, (0, 0), loop))
    end

end

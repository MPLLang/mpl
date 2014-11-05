functor MergePar (A: SORTARG) : MERGE =
struct

  open A

  (* takes a sorted slice and an element x; returns two lists of A.t such that
    every element in the first is <= x and those in the second >= x *)
  fun split { cutoff, fallback } x c = fallback x c

  (* merges two sorted inputs in parallel *)
  fun merge split_cutofffallback { cutoff, fallback } (b, c) = 
    let
      fun loop (b, c) =
        let
          val l = length b
        in
          (* assumes cutoff > 0 *)
          if l < cutoff orelse length c < cutoff then [fallback (b, c)]
          else
            let
              val (b1, b2) = halve b
              val x = sub (b2, 0)
              val (c1s, c2s) = split split_cutofffallback x c
              val (c1, c2) = (concat c1s, concat c2s)
            in
              if l = 1 then
                 [c1, b, c2]
              else
                let
                  val (a1, a2) = fork (fn () => loop (c1, b1),
                                       fn () => loop (c2, b2))
                in
                  a1 @ a2
                end
            end
        end
    in
      concat (loop (b, c))
    end
  
end

functor MergeFutures (A: SORTARG) : MERGE =
struct

  open A

  (* takes a sorted slice and an element x; returns two lists of A.t such that
    every element in the first is <= x and those in the second >= x *)
  fun split' { cutoff, fallback } x c = 
    let
      fun loop c =
          if length c < cutoff then
            let
              val (c1, c2) = fallback x c
            in
              (c1, future (fn () => c2))
            end
          else
            let
              val == = Real.== infix 4 ==
              val (c1, c2) = halve c
              val y = sub (c2, 0)
            in
              if y == x then ([c1], future (fn () => [c2]))
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
                  (c11, future (fn () => touch c12 @ [c2]))
                end
            end
    in
      loop c
    end

  fun split cf x c =
      let
        val (c1s, c2s) = split' cf x c
      in
        (c1s, (touch c2s))
      end

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
              val (c1s, c2s) = split' split_cutofffallback x c
              val c1 = concat c1s
            in
              if l = 1 then
                 [c1, b, concat (touch c2s)]
              else
                let

                  val (a1, a2) = fork (fn () => loop (c1, b1),
                                       fn () => loop (concat (touch c2s), b2))
(*
                  val (a1, a2) = (loop (c1, b1),
                                  loop (concat (touch c2s), b2))
*)
                in
                  a1 @ a2
                end
            end
        end
    in
      concat (loop (b, c))
    end
  
end

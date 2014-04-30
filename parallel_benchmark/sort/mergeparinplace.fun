functor MergeParInplace (A: SORTARG)  =
struct
  open A

  fun break x c = 
    let
      val l = length c
    in
      if l = 0 then 0
      else if l = 1 then
        if sub (c, 0) <= x then 1 else 0
      else
        let
          val == = Real.== infix 4 ==
          val y = sub (c, l div 2)
        in
          if y == x then l div 2
          else if y < x then
            (break x (slice (c, l div 2, NONE))) + (l div 2)
          else (* y > x *)
            break x (slice (c, 0, SOME (l div 2)))
        end
    end

  (* merges two sorted inputs in parallel *)
  fun merge { cutoff, fallback } (b, c) a = 
    let
      fun loop (b, c) a =
        let
          val l = length b
        in
          (* assumes cutoff > 0 *)
          if l < cutoff orelse length c < cutoff then fallback (b, c) a
          else
            let
              val x = sub (b, l div 2)
              val k = break x c
              val (c1, c2) = (slice (c, 0, SOME k), slice (c, k, NONE))
            in
              if l = 1 then
                let in
                  copy { src = c1, dst = a, di = 0 };
                  update (a, k, x);
                  copy { src = c2, dst = a, di = k + 1 }
                end
              else
                let
                  val (b1, b2) = (slice (b, 0, SOME (l div 2)),
                                  slice (b, l div 2, NONE))
                  val (a1, a2) = (slice (a, 0, SOME ((l div 2) + k)),
                                  slice (a, (l div 2) + k, NONE))
                  val _ = fork (fn () => loop (c1, b1) a1,
                                fn () => loop (c2, b2) a2)
                in
                  ()
                end
            end
        end
    in
      loop (b, c) a
    end
  
end

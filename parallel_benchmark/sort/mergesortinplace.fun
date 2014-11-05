functor MergeSortInplace (structure A: SORTARG
                          structure M1: MERGEINPLACE
                            where type t = A.t
                          structure M2: MERGEINPLACE
                            where type t = A.t) : SORTINPLACE = 
struct

  open A

  fun sort { cutoff, fallback } args = 
    let 
      val merge = M1.merge { cutoff = 0, fallback = fn _ => raise Sort }
      val merge = M2.merge { cutoff = cutoff, fallback = merge}

      fun loop (toLeft, a, a') = 
        let 
          val l = length a 
        in
          if l <= 1 then 
            copy { src = a, dst = a', di = 0 }
          else if l < cutoff then fallback (toLeft, a, a')
          else
            let
              val (b, c) = (slice (a, 0, SOME (l div 2)),
                            slice (a, l div 2, NONE))
              val (b', c') = (slice (a', 0, SOME (l div 2)),
                              slice (a', l div 2, NONE))
              val _ = fork (fn () => loop (not toLeft, b, b'), 
                            fn () => loop (not toLeft, c, c'))
            in
              if toLeft then
                 merge (b', c') a
              else
                 merge (b, c) a'
            end
        end

    in
      loop args
    end
        
end



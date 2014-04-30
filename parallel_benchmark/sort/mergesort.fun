functor MergeSort (structure A: SORTARG
                   structure M1: MERGE
                     where type t = A.t
                   structure M2: MERGE
                     where type t = A.t) : SORT = 
struct

  open A

  fun sort { cutoff, fallback } a = 
    let 
      val merge = M1.merge { cutoff = 0, fallback = fn _ => raise Sort }
                           { cutoff = 0, fallback = fn _ => raise Sort }
      val split = M1.split { cutoff = 0, fallback = fn _ => raise Sort }
      val merge = M2.merge { cutoff = cutoff, fallback = split }
                           { cutoff = cutoff, fallback = merge }

      fun loop a = 
        let 
          val l = length a 
        in
          if l <= 1 then a
          else if l < cutoff then fallback a
          else
            let
              val (b, c) = halve a
              val (b, c) = fork (fn () => loop b, 
                                 fn () => loop c)
            in
              merge (b, c)
            end
        end
    in
      loop a
    end
        
end



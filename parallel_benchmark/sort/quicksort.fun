functor QuickSort (A: SORTARG) : SORT = 
struct

  open A

  fun sort { cutoff, fallback } a =
    let
      val c = ref 0
      fun next () = !c before c := !c + 1

      fun loop a =
        let 
          val l = length a 
(*
          val n = next ()
          val () = print ("<" ^ (Int.toString n) ^ "-" ^ ((Int.toString l) ^ " "))
*)
        in
          if l < cutoff then [fallback a]
          else
            let
              val == = Real.== infix 4 ==
              val x = (* pivot a *)
                  (* if l > 200 then
                       sub (a, 199)
                  else  *)
                  sub (a, 0)

              val (b, (c, d)) =

                fork (fn () => (loop (filter cutoff (fn y => y < x) a) 
                                      (* before print ("~" ^ (Int.toString n) ^ " ") *)),
                      fn () => (fork (fn () => [filter cutoff (fn y => y == x) a],
                                      fn () => (if l < cutoff + 10 then GC () else (); 
                                                loop (filter cutoff (fn y => y > x) a)))))
(*
                let 
                  val (ls, (es, gs)) =                    
                      fork (fn () => filter cutoff (fn y => y < x) a,
                            fn () => (fork (fn () => [filter cutoff (fn y => y = x) a],
                                            fn () => (GC (); filter cutoff (fn y => y > x) a))))
                  val (b, d) =
                      fork (fn () => loop ls,
                            fn () => loop gs)
                in
                  (b, (es, d))
                end
*)
            in
              b @ c @ d
(*
              before
              print (">" ^ (Int.toString n) ^ " ")
*)
            end
        end
    in
      concat (loop a)
    end
end

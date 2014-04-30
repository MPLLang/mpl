functor Print (structure A: SORTARG) : SORT =
struct

  type t = A.t

  fun sort { cutoff, fallback } a =
      let 
        val () = print "before:\n"
        val () = A.printArg a

        val a = fallback a

        val () = print "after:\n"
        val () = A.printArg a
      in
        a
      end


end

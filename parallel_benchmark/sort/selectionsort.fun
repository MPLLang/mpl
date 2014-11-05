functor SelectionSort (A: SORTARG) : SORT =
struct

  open A

  datatype mode = 
     Repeat of real * int
   (* PERF this could be nullary since we have the index of the last
     element added to the output *)
   | Larger of real

  fun sort _ a =
      let
        (* return the first occurrence of smallest element > z and it's idx *)
        fun select (_, Larger z) = 
            let 
              val (x, i) = foldli (fn (i, x, (y, j)) => 
                                      if x > z andalso x < y
                                      then (x, i)
                                      else (y, j))
                                  (Real.posInf, 0) a
            in
              (x, Repeat (x, i))
            end
          (* find the next occurance of x after index i *)
          | select (k, Repeat (x, i)) =
            let 
              val == = Real.== infix 4 ==
              (* return index >= j such that sub (a, j) = x *)
              fun loop j = 
                  if j = length a then NONE
                  else if sub (a, j) == x then SOME j
                  else loop (j + 1)
            in
              case loop (i + 1) of
                NONE => select (k, Larger x)
              | SOME j => (x, Repeat (x, j))
            end

        val b = #1 (unfoldi NONE (length a, Larger Real.negInf, select))
      in
        b
(*
        before
        (printArg b; print "\n")
*)
      end
end

functor InsertionSort (A: SORTARG) : SORT =
struct

  open A

  fun sort _ a =
      let
        (* take an element and a sorted list and return a new list with that
         element in the appropriate place *)
        fun insert (x, nil) = [x]
          | insert (x, y::ys) = if x <= y then x::y::ys
                                else y::(insert (x, ys))
      in
        fromList NONE (foldl insert nil (toList a))
      end
end

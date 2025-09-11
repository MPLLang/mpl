structure OffsetSearch :>
sig
  (* `search x xs (lo, hi)` searches the sorted array `xs` between indices `lo`
   * and `hi`, returning `(i, j)` where `i-lo` is the number of elements that
   * are strictly less than `x`, and `j-i` is the number of elements which are
   * equal to `x`. *)
  val search : int -> int array -> int * int -> int * int
end =
struct

  val sub = Array.sub
  val upd = Array.update

  fun lowSearch x xs (lo, hi) =
    case hi - lo of
      0 => lo
    | n => let val mid = lo + n div 2
           in case Int.compare (x, sub (xs, mid)) of
                 LESS => lowSearch x xs (lo, mid)
               | GREATER => lowSearch x xs (mid + 1, hi)
               | EQUAL => lowSearchEq x xs (lo, mid)
           end

  and lowSearchEq x xs (lo, mid) =
    if (mid = 0) orelse (x > sub (xs, mid-1))
    then mid
    else lowSearch x xs (lo, mid)

  and highSearch x xs (lo, hi) =
    case hi - lo of
      0 => lo
    | n => let val mid = lo + n div 2
           in case Int.compare (x, sub (xs, mid)) of
                 LESS => highSearch x xs (lo, mid)
               | GREATER => highSearch x xs (mid + 1, hi)
               | EQUAL => highSearchEq x xs (mid, hi)
           end

  and highSearchEq x xs (mid, hi) =
    if (mid = Array.length xs - 1) orelse (x < sub (xs, mid + 1))
    then mid + 1
    else highSearch x xs (mid + 1, hi)

  and search (x : int) (xs : int array) (lo, hi) : int * int =
    case hi - lo of
      0 => (lo, lo)
    | n => let val mid = lo + n div 2
           in case Int.compare (x, sub (xs, mid)) of
                LESS    => search x xs (lo, mid)
              | GREATER => search x xs (mid + 1, hi)
              | EQUAL   => (lowSearchEq x xs (lo, mid), highSearchEq x xs (mid, hi))
           end

end

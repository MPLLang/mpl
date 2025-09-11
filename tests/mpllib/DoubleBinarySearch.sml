(* The function `split_count` splits sequences s and t into (s1, s2) and
 * (t1, t2) such that the largest items of s1 and t1 are smaller than the
 * smallest items of s2 and t2. The desired output size |s1|+|t1| is given
 * as a parameter.
 *
 * Specifically, `split_count cmp (s, t) k` returns `(m, n)` where:
 *   (s1, s2) = (s[..m], s[m..])
 *   (t1, t2) = (t[..n], t[n..])
 *   m+n = k
 *   max(s1) <= min(t2)
 *   max(t1) <= min(s2)
 *
 * Note that there are many possible solutions, so we also mandate that `m`
 * should be minimized.
 *
 * Work: O(log(|s|+|t|))
 * Span: O(log(|s|+|t|))
 *)
structure DoubleBinarySearch:
sig
  type 'a seq = {lo: int, hi: int, get: int -> 'a}

  val split_count: ('a * 'a -> order) -> 'a seq * 'a seq -> int -> (int * int)

  val split_count_slice: ('a * 'a -> order)
                         -> 'a ArraySlice.slice * 'a ArraySlice.slice
                         -> int
                         -> (int * int)
end =
struct

  type 'a seq = {lo: int, hi: int, get: int -> 'a}

  fun leq cmp (x, y) =
    case cmp (x, y) of
      GREATER => false
    | _ => true

  fun geq cmp (x, y) =
    case cmp (x, y) of
      LESS => false
    | _ => true

  fun split_count cmp (s: 'a seq, t: 'a seq) k =
    let
      fun normalize_then_loop (slo, shi) (tlo, thi) count =
        let
          val slo_orig = slo
          val tlo_orig = tlo

          (* maybe count is small *)
          val shi = Int.min (shi, slo + count)
          val thi = Int.min (thi, tlo + count)

          (* maybe count is large *)
          val slack = (shi - slo) + (thi - tlo) - count
          val slack = Int.min (slack, shi - slo)
          val slack = Int.min (slack, thi - tlo)

          val slo = Int.max (slo, shi - slack)
          val tlo = Int.max (tlo, thi - slack)

          val count = count - (slo - slo_orig) - (tlo - tlo_orig)
        in
          loop (slo, shi) (tlo, thi) count
        end


      and loop (slo, shi) (tlo, thi) count =
        if shi - slo <= 0 then
          (slo, tlo + count)

        else if thi - tlo <= 0 then
          (slo + count, tlo)

        else if count = 1 then
          if geq cmp (#get s slo, #get t tlo) then (slo, tlo + 1)
          else (slo + 1, tlo)

        else
          let
            val m = count div 2
            val n = count - m

            (*  |------|x|-------|
             *  ^      ^         ^
             * slo   slo+m      shi
             *
             *  |------|y|-------|
             *  ^        ^       ^
             * tlo     tlo+n    thi
             *)

            val leq_y_x =
              n = 0 orelse slo + m >= shi
              orelse leq cmp (#get t (tlo + n - 1), #get s (slo + m))
          in
            if leq_y_x then
              normalize_then_loop (slo, shi) (tlo + n, thi) (count - n)
            else
              normalize_then_loop (slo, shi) (tlo, tlo + n) count
          end


      val {lo = slo, hi = shi, ...} = s
      val {lo = tlo, hi = thi, ...} = t

      val (m, n) = normalize_then_loop (slo, shi) (tlo, thi) k
    in
      (m - slo, n - tlo)
    end


  fun fromslice s =
    let val (sarr, slo, slen) = ArraySlice.base s
    in {lo = slo, hi = slo + slen, get = fn i => Array.sub (sarr, i)}
    end


  fun split_count_slice cmp (s, t) k =
    split_count cmp (fromslice s, fromslice t) k

end

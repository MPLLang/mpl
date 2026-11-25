(* Author: The 210 Team
 *
 * Uses dual-pivot quicksort from:
 *
 * Dual-Pivot Quicksort Algorithm
 * Vladimir Yaroslavskiy
 * http://codeblab.com/wp-content/uploads/2009/09/DualPivotQuicksort.pdf
 * 2009
 *
 * Insertion sort is taken from the SML library ArraySort
 *)

structure Quicksort:
sig
  type 'a seq = 'a ArraySlice.slice
  val sortInPlaceG : int -> ('a * 'a -> order) -> 'a seq -> unit
  val sortInPlace : ('a * 'a -> order) -> 'a seq -> unit
  val sortG : int -> ('a * 'a -> order) -> 'a seq -> 'a seq
  val sort : ('a * 'a -> order) -> 'a seq -> 'a seq
end =
struct

  type 'a seq = 'a ArraySlice.slice

  structure A = Array
  structure AS = ArraySlice

  fun sortRange grainsize (array, start, n, compare) =
    let
      val sub = A.sub
      val update = A.update

      fun item i = sub(array,i)
      fun set(i,v) = update(array,i,v)
      fun cmp(i,j) = compare(item i, item j)

      fun swap (i,j) =
        let val tmp = item i
        in set(i, item j); set(j, tmp) end

      (* same as swap(j,k); swap(i,j) *)
      fun rotate(i,j,k) =
        let val tmp = item k
        in set(k, item j); set(j, item i); set(i, tmp) end

      fun insertSort (start, n) =
        let val limit = start+n
            fun outer i =
              if i >= limit then ()
              else let fun inner j =
                         if j = start then outer(i+1)
                         else let val j' = j - 1
                              in if cmp(j', j) = GREATER
                                 then (swap(j,j'); inner j')
                                 else outer(i+1)
                              end
                   in inner i end
        in outer (start+1) end

      (* puts lesser pivot at start and larger at end *)
      fun twoPivots(a, n) =
        let fun sortToFront(size) =
              let val m = n div (size + 1)
                  fun toFront(i) =
                    if (i < size) then (swap(a + i, a + m*(i+1)); toFront(i+1))
                    else ()
              in (toFront(0); insertSort(a,size)) end
        in if (n < 80) then
               (if cmp(a, a+n-1) = GREATER then swap(a,a+n-1) else ())
           else (sortToFront(5); swap(a+1,a); swap(a+3,a+n-1))
        end

      (* splits based on two pivots (p1 and p2) into 3 parts:
          less than p1, greater than p2, and the rest in the middle.
          The pivots themselves end up at the two ends.
          If the pivots are the same, returns a false flag to indicate middle
          need not be sorted. *)
      fun split3 (a, n) =
        let
            val (p1,p2) = (twoPivots(a,n); (a, a+n-1))
            fun right(r) = if cmp(r, p2) = GREATER then right(r-1) else r
            fun loop(l,m,r) =
              if (m > r) then (l,m)
              else if cmp(m, p1) = LESS then (swap(m,l); loop(l+1, m+1, r))
              else (if cmp(m, p2) = GREATER then
			(if cmp(r, p1) = LESS
			 then (rotate(l,m,r); loop(l+1, m+1, right(r-1)))
			 else (swap(m,r); loop(l, m+1, right(r-1))))
                    else loop(l, m+1, r))
            val (l,m) = loop(a + 1, a + 1, right(a + n - 2))
        in (l, m, cmp(p1, p2) = LESS) end

      (* makes recursive calls in parallel if big enough *)
      fun qsort (a, n) =
        if (n < 16) then insertSort(a, n)
        else let
            val (l, m, doMid) = split3(a,n)
        in if (n <= grainsize) then
               (qsort (a, l-a);
                (if doMid then qsort(l, m-l) else ());
                qsort (m, a+n-m))
           else let val par = ForkJoin.par
                    val left = (fn () => qsort (a, l-a))
                    val mid = (fn () => qsort (l, m-l))
                    val right = (fn () => qsort (m, a+n-m))
                    val maybeMid = if doMid then (fn () => (par(mid,right);()))
                                   else right
                in par(left,maybeMid);() end
        end

  in qsort (start,n) end

  (* sorts an array slice in place *)
  fun sortInPlaceG grainsize compare aslice =
    let val (a, i, n) = AS.base aslice
    in sortRange grainsize (a, i, n, compare)
    end

  fun sortG grainsize compare aslice =
    let
      val result = AS.full (ForkJoin.alloc (AS.length aslice))
    in
      Util.foreach aslice (fn (i, x) => AS.update (result, i, x));
      sortInPlaceG grainsize compare result;
      result
    end

  val grainsize = 8192

  fun sortInPlace c s = sortInPlaceG grainsize c s
  fun sort c s = sortG grainsize c s

end

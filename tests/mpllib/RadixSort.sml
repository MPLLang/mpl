(* Author: Lawrence Wang (lawrenc2@andrew.cmu.edu, github.com/larry98)
 *
 * The lsdSort and msdSort functions take the following arguments:
 *   - s : 'a ArraySequence.t
 *       the array (of strings) to sort
 *   - bucket : 'a ArraySequence.t -> int -> int -> int
 *       bucket s k i specifies which bucket the k'th digit of the i'th element
 *       of s should map to
 *   - numPasses : int
 *       the number of counting sort passes to make i.e. the number of digits
 *       in the strings of the array
 *   - numBuckets : int
 *       the number of buckets used in counting sort
 *
 * The quicksort function implements 3-way radix quicksort and takes the
 * following arguments:
 *   - s : 'a ArraySequence.t
 *      the array (of strings) to sort
 *   - cmp : int -> 'a * 'a -> order
 *      cmp k (x, y) specifies the comparison between the kth digit of x with
 *      the kth digit of y
 *   - numPasses : int
 *      the maximum number of quicksort passes to make (commonly the maximum
 *      length of the strings being sorted)
 *)
structure RadixSort :>
sig
  val lsdSort : 'a Seq.t -> ('a Seq.t -> int -> int -> int)
                -> int -> int -> 'a Seq.t
  val msdSort : 'a Seq.t -> ('a Seq.t -> int -> int -> int)
                -> int -> int -> 'a Seq.t
  val quicksort : 'a Seq.t -> (int -> 'a * 'a -> order) -> int
                -> 'a Seq.t
end =
struct

  structure AS =
  struct
    open ArraySlice
    open Seq

    val GRAIN = 4096
    val ASupdate = ArraySlice.update
    val alloc = ForkJoin.alloc
  end

  fun lsdSort s bucket numPasses numBuckets =
    let
      fun loop s i =
        if i < 0 then s
        else loop (#1 (CountingSort.sort s (bucket s i) numBuckets)) (i - 1)
    in
      loop s (numPasses - 1)
    end

  fun msdSort s bucket numPasses numBuckets =
    let
      val n = AS.length s
      val result = ArraySlice.full (AS.alloc n)
      fun msdSort' s pass lo hi =
        if pass = numPasses then
          ForkJoin.parfor AS.GRAIN (0, hi - lo) (fn i =>
            AS.ASupdate (result, lo + i, AS.nth s i)
          )
        else
          let
            val (s', offsets) = CountingSort.sort s (bucket s pass) numBuckets
          in
            ForkJoin.parfor AS.GRAIN (0, numBuckets) (fn i =>
              let
                val start = AS.nth offsets i
                val len = if i = numBuckets - 1 then AS.length s' - start
                          else AS.nth offsets (i + 1) - start
                val s'' = AS.subseq s' (start, len)
              in
                if len = 0 then ()
                else if len = 1 then
                  AS.ASupdate (result, lo + start, AS.nth s'' 0)
                else
                  msdSort' s'' (pass + 1) (lo + start) (lo + start + len)
              end
            )
          end
      val () = msdSort' s 0 0 n
    in
      result
    end

  fun par3 (a, b, c) =
    let
      val ((ar, br), cr) = ForkJoin.par (fn _ => ForkJoin.par (a, b), c)
    in
      (ar, br, cr)
    end

  fun quicksort s cmp numPasses =
    let
      val n = AS.length s
      val result = ArraySlice.full (AS.alloc n)
      (* TODO: Change to insertion sort if size of array is small *)
      fun quicksort' s digit lo hi seed =
        if hi = lo then ()
        else if hi - lo = 1 then AS.ASupdate (result, lo, AS.nth s 0)
        else if digit = numPasses then
          ForkJoin.parfor AS.GRAIN (0, hi - lo) (fn i =>
            AS.ASupdate (result, lo + i, AS.nth s i)
          )
        else
          let
            val n' = hi - lo
            val pivot = AS.nth s (seed mod n')
            fun bucket i =
              case cmp digit (AS.nth s i, pivot) of
                   LESS => 0
                 | EQUAL => 1
                 | GREATER => 2
            val (s', offsets) = CountingSort.sort s bucket 3
            val mid1 = AS.nth offsets 1
            val mid2 = AS.nth offsets 2
            val seed1 = Util.hash (seed + 1)
            val seed2 = Util.hash (seed + 2)
            val seed3 = Util.hash (seed + 3)
            val s1 = AS.subseq s' (0, mid1)
            val s2 = AS.subseq s' (mid1, mid2 - mid1)
            val s3 = AS.subseq s' (mid2, n' - mid2)
            val () = if hi - lo < 1024 then (
              quicksort' s1 digit lo (lo + mid1) seed1;
              quicksort' s2 (digit + 1) (lo + mid1) (lo + mid2) seed2;
              quicksort' s3 digit (lo + mid2) hi seed3
            ) else (
              let val ((), (), ()) = par3 (
                fn () => quicksort' s1 digit lo (lo + mid1) seed1,
                fn () => quicksort' s2 (digit + 1) (lo + mid1) (lo + mid2) seed2,
                fn () => quicksort' s3 digit (lo + mid2) hi seed3
              ) in () end
            )
          in
            ()
          end
      val () = quicksort' s 0 0 n (Util.hash 0)
    in
      result
    end

end

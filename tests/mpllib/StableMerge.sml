structure StableMerge:
sig
  type 'a seq = 'a ArraySlice.slice

  val writeMergeSerial: ('a * 'a -> order) (* compare *)
                        -> 'a seq * 'a seq (* (sorted) sequences to merge *)
                        -> 'a seq (* output *)
                        -> unit

  val writeMerge: ('a * 'a -> order) (* compare *)
                  -> 'a seq * 'a seq (* (sorted) sequences to merge *)
                  -> 'a seq (* output *)
                  -> unit

  val mergeSerial: ('a * 'a -> order) -> 'a seq * 'a seq -> 'a seq
  val merge: ('a * 'a -> order) -> 'a seq * 'a seq -> 'a seq
end =
struct

  structure AS = ArraySlice
  type 'a seq = 'a AS.slice

  val for = Util.for
  val parfor = ForkJoin.parfor
  val par = ForkJoin.par
  val allocate = ForkJoin.alloc

  val serialGrain =
    CommandLineArgs.parseInt "MPLLib_StableMerge_serialGrain" 4000

  fun sliceIdxs s i j =
    AS.subslice (s, i, SOME (j - i))

  fun writeMergeSerial cmp (s1, s2) t =
    let
      fun write i x = AS.update (t, i, x)

      val n1 = AS.length s1
      val n2 = AS.length s2

      (* i1 index into s1
       * i2 index into s2
       * j index into output *)
      fun loop i1 i2 j =
        if i1 = n1 then
          Util.foreach (sliceIdxs s2 i2 n2) (fn (i, x) => write (i + j) x)
        else if i2 = n2 then
          Util.foreach (sliceIdxs s1 i1 n1) (fn (i, x) => write (i + j) x)
        else
          let
            val x1 = AS.sub (s1, i1)
            val x2 = AS.sub (s2, i2)
          in
            (* NOTE: this is stable *)
            case cmp (x1, x2) of
              GREATER => (write j x2; loop i1 (i2 + 1) (j + 1))
            | _ => (write j x1; loop (i1 + 1) i2 (j + 1))
          end
    in
      loop 0 0 0
    end

  fun mergeSerial cmp (s1, s2) =
    let val out = AS.full (allocate (AS.length s1 + AS.length s2))
    in writeMergeSerial cmp (s1, s2) out; out
    end

  fun writeMerge cmp (s1, s2) t =
    if AS.length t <= serialGrain then
      writeMergeSerial cmp (s1, s2) t
    else if AS.length s1 = 0 then
      Util.foreach s2 (fn (i, x) => AS.update (t, i, x))
    else
      let
        val n1 = AS.length s1
        val n2 = AS.length s2
        val mid1 = n1 div 2
        val pivot = AS.sub (s1, mid1)
        val mid2 = BinarySearch.countLess cmp s2 pivot

        val l1 = sliceIdxs s1 0 mid1
        val r1 = sliceIdxs s1 (mid1 + 1) n1
        val l2 = sliceIdxs s2 0 mid2
        val r2 = sliceIdxs s2 mid2 n2

        val _ = AS.update (t, mid1 + mid2, pivot)
        val tl = sliceIdxs t 0 (mid1 + mid2)
        val tr = sliceIdxs t (mid1 + mid2 + 1) (AS.length t)
      in
        par (fn _ => writeMerge cmp (l1, l2) tl, fn _ =>
          writeMerge cmp (r1, r2) tr);
        ()
      end

  fun merge cmp (s1, s2) =
    let val out = AS.full (allocate (AS.length s1 + AS.length s2))
    in writeMerge cmp (s1, s2) out; out
    end

end

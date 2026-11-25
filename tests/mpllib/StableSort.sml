structure StableSort:
sig
  type 'a seq = 'a ArraySlice.slice
  val sortInPlace: ('a * 'a -> order) -> 'a seq -> unit
  val sort: ('a * 'a -> order) -> 'a seq -> 'a seq
end =
struct

  type 'a seq = 'a ArraySlice.slice

  structure AS = ArraySlice

  fun take s n = AS.subslice (s, 0, SOME n)
  fun drop s n = AS.subslice (s, n, NONE)

  val par = ForkJoin.par
  val allocate = ForkJoin.alloc

  (* in-place sort s, using t as a temporary array if needed *)
  fun sortInPlace' cmp s t =
    if AS.length s <= 1 then
      ()
    else let
      val half = AS.length s div 2
      val (sl, sr) = (take s half, drop s half)
      val (tl, tr) = (take t half, drop t half)
    in
      (* recursively sort, writing result into t *)
      if AS.length s <= 1024 then
        (writeSort cmp sl tl; writeSort cmp sr tr)
      else
        ( par (fn _ => writeSort cmp sl tl, fn _ => writeSort cmp sr tr)
        ; ()
        );

      (* merge back from t into s *)
      StableMerge.writeMerge cmp (tl, tr) s;

      ()
    end

  (* destructively sort s, writing the result in t *)
  and writeSort cmp s t =
    if AS.length s <= 1 then
      Util.foreach s (fn (i, x) => AS.update (t, i, x))
    else let
      val half = AS.length s div 2
      val (sl, sr) = (take s half, drop s half)
      val (tl, tr) = (take t half, drop t half)
    in
      (* recursively in-place sort sl and sr *)
      if AS.length s <= 1024 then
        (sortInPlace' cmp sl tl; sortInPlace' cmp sr tr)
      else
        ( par (fn _ => sortInPlace' cmp sl tl, fn _ => sortInPlace' cmp sr tr)
        ; ()
        );

      (* merge into t *)
      StableMerge.writeMerge cmp (sl, sr) t;

      ()
    end

  fun sortInPlace cmp s =
    let
      val t = AS.full (allocate (AS.length s))
    in
      sortInPlace' cmp s t
    end

  fun sort cmp s =
    let
      val result = AS.full (allocate (AS.length s))
    in
      Util.foreach s (fn (i, x) => AS.update (result, i, x));
      sortInPlace cmp result;
      result
    end

end

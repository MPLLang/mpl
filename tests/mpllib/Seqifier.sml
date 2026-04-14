(* Implements a parallel "sequence builder" data structure:
 *   type 'a seqifier
 *   type 'a t = 'a seqifier
 *
 * These can be used to write purely functional algorithms that directly write
 * to an underlying (mutable) array, but the mutable array is hidden behind
 * the interface and not exposed to the programmer.
 *
 * Example usage is:
 *   val sb = init_expect_length n       (* O(1) *)
 *   val (sb1, sb2) = split_at (sb, i)   (* O(1) *)
 *   val (sb1', sb2') =
 *     ForkJoin.par
 *       (fn () => put (sb1, X),         (* O(|X|) work, O(log|X|) span *)
 *        fn () => put (sb2, Y))         (* O(|Y|) work, O(log|Y|) span *)
 *   ...
 *   val sb' = append (sb1', sb2')       (* O(1) *)
 *   val result = finalize sb'           (* O(1) *)
 *
 * The value semantics of these functions can be described in terms of a
 * purely functional sequence with elements of type 'a option. Initially,
 * every element is NONE. Calling `put (...)` returns a seqifier that is
 * full of SOME(x) elements. Calling `finalize` checks that there are no
 * NONEs, and returns a sequence of just the elements themselves.
 *
 * To achieve good cost bounds, seqifiers can only be "used" at most once.
 * "Using" a seqifier means passing it as argument to one of the following
 * functions:
 *   split_at
 *   put
 *   append
 *   finalize
 *
 * (Note that the function `length: 'a seqifier -> int` is read-only and does
 * not constitute a "use"; this function is safe to call in any context.)
 *
 * Every time a seqifier is used, it is immediately invalidated. Any call to
 * one of the functions above that receives an invalid seqifier as input
 * will raise the exception UsedTwice.
 *
 * When appending two seqifiers, it is essential that they "came from" the
 * same original seqifier and are physically adjacent to each other. Calling
 * append will raise NonAdjacent otherwise.
 *
 * For example, this is okay:
 *   val (l, r) = split_at (x, i)
 *   val (l1, l2) = split_at (l, j)
 *   ...
 *   val foo = append (l1, append (l2, r))
 *
 * But this would raise NonAdjacent:
 *   val (l, r) = split_at (x, i)
 *   val (l1, l2) = split_at (l, j)
 *   ...
 *   val foo = append (l1, r)
 *
 * When finalizing a seqifier, you may get the exception MaybeMissingPut. This
 * occurs if one of the components of the seqifier was never covered by the
 * result of a `put`.
 *
 * For example, this would raise MaybeMissingPut, because we never called
 * `put` on the segment `r`.
 *   val x = init_expect_length n
 *   val (l, r) = split_at (x, i)
 *   val l' = put (x, ...)
 *   val result = finalize (append (l', r))
 *
 * The MaybeMissingPut issue is checked conservatively by keeping track, for
 * every seqifier, whether or not that seqifier has been fully put. This is set
 * to `true` on the output of a `put`, and at each `append` we check if both
 * sides are marked as fully put. Calling `split_at` just copies the boolean
 * to both of the results. This approach is conservative because we don't
 * individually track every index. (Specifically, at `append`, if at least
 * one index has not yet been put, we mark the whole result as not put.)
 *)
structure Seqifier:
sig
  exception UsedTwice
  exception NonAdjacent
  exception MaybeMissingPut

  type 'a seqifier
  type 'a t = 'a seqifier

  val length: 'a t -> int

  val init_expect_length: int -> 'a t
  val split_at: 'a t * int -> 'a t * 'a t
  val append: 'a t * 'a t -> 'a t
  val put: 'a t * 'a Seq.t -> 'a t
  val finalize: 'a t -> 'a Seq.t

  (* ========================================================================
   * UNSAFE FUNCTIONS
   * These give direct access to the internals of the seqifier.
   * Don't use unless you know what you are doing!
   *)

  val unsafe_mark_put: 'a t -> 'a t
  val unsafe_view_contents: 'a t -> 'a ArraySlice.slice

end =
struct


  datatype 'a t =
    T of
      { output: 'a array
      , offset: int
      , len: int
      , fully_put: bool
      , valid: Word8.word ref
      }

  type 'a seqifier = 'a t


  exception UsedTwice
  exception NonAdjacent
  exception MaybeMissingPut


  fun unpack_and_mark_used (T {output, offset, len, fully_put, valid}) =
    if !valid = 0w0 orelse Concurrency.cas valid (0w1, 0w0) = 0w0 then
      raise UsedTwice
    else
      (output, offset, len, fully_put)


  fun pack (output, offset, len, fully_put) =
    T { output = output
      , offset = offset
      , len = len
      , fully_put = fully_put
      , valid = ref 0w1
      }


  fun init_expect_length n =
    pack (ForkJoin.alloc n, 0, n, false)


  fun length (T {len, ...}) = len


  fun split_at (t, i) =
    let
      val (output, offset, len, fp) = unpack_and_mark_used t
    in
      if i < 0 orelse i > len then
        raise Subscript
      else
        (pack (output, offset, i, fp), pack (output, offset + i, len - i, fp))
    end


  fun append (l, r) =
    let
      val (output1, offset1, len1, fp1) = unpack_and_mark_used l
      val (output2, offset2, len2, fp2) = unpack_and_mark_used r
    in
      if not (MLton.eq (output1, output2) andalso offset1 + len1 = offset2) then
        raise NonAdjacent
      else
        pack (output1, offset1, len1 + len2, fp1 andalso fp2)
    end


  fun put (t, s) =
    let
      val (output, offset, len, _) = unpack_and_mark_used t
    in
      if Seq.length s <> len then
        raise Size
      else
        ( Seq.foreach s (fn (i, x) => Array.update (output, offset + i, x))
        ; pack (output, offset, len, true)
        )
    end


  fun finalize t =
    let
      val (output, offset, len, fp) = unpack_and_mark_used t
    in
      if not fp then raise MaybeMissingPut
      else ArraySlice.slice (output, offset, SOME len)
    end


  (* =======================================================================
   * UNSAFE FUNCTIONS BELOW
   *)

  fun unsafe_mark_put (T {output, offset, len, valid, ...}) =
    T { output = output
      , offset = offset
      , len = len
      , valid = valid
      , fully_put = true
      }

  fun unsafe_view_contents (T {output, offset, len, ...}) =
    ArraySlice.slice (output, offset, SOME len)

end

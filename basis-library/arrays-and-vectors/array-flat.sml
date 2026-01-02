structure ArrayFlat: ARRAY_FLAT_EXTRA =
struct
  structure A = Sequence (Primitive.ArrayFlat)
  open A

  val op < = Int.<
  val op <= = Int.<=

  fun wrap2 f = fn (i, x) => f (SeqIndex.toIntUnsafe i, x)

  type 'a array = 'a ArrayFlat.t
  type 'a vector = 'a VectorFlat.vector

  structure ArraySlice =
  struct
    open Slice
    val vector = Primitive.ArrayFlat.Slice.vector
    val copyVec = VectorFlat.VectorSlice.copy
    val unsafeCopyVec = VectorFlat.VectorSlice.unsafeCopy
    fun modifyi f sl = Primitive.ArrayFlat.Slice.modifyi (wrap2 f) sl
    val modify = Primitive.ArrayFlat.Slice.modify
  end

  val array = new
  val unsafeArray = unsafeNew
  val vector = Primitive.ArrayFlat.vector
  val copyVec = VectorFlat.copy
  val unsafeCopyVec = VectorFlat.unsafeCopy
  fun modifyi f sl = Primitive.ArrayFlat.modifyi (wrap2 f) sl
  val modify = Primitive.ArrayFlat.modify
  structure Raw = Primitive.ArrayFlat.Raw
  structure Raw =
  struct
    type 'a rawarr = 'a Raw.rawarr

    fun length a =
        if Primitive.Controls.safe
          then (SeqIndex.toInt (Raw.length a))
                handle Overflow => raise Fail "Raw.length"
          else SeqIndex.toIntUnsafe (Raw.length a)

    fun alloc n = Raw.alloc (SeqIndex.fromIntForLength n)
    fun unsafeAlloc n = Raw.unsafeAlloc (SeqIndex.fromIntUnsafe n)

    val uninitIsNop = Raw.uninitIsNop
    fun unsafeUninit (a, i) =
        Raw.unsafeUninit (a, SeqIndex.fromIntUnsafe i)
    fun uninit (a, i) =
        if Primitive.Controls.safe
          then let
                  val i =
                      (SeqIndex.fromInt i)
                      handle Overflow => raise Subscript
                in
                  Raw.uninit (a, i)
                end
          else unsafeUninit (a, i)

    val unsafeToArray = Primitive.ArrayFlat.Raw.unsafeToArray
  end
end

structure ArrayFlatSlice: ARRAY_FLAT_SLICE_EXTRA = ArrayFlat.ArraySlice
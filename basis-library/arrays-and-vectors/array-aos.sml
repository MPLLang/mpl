structure ArrayAos: ARRAY_AOS_EXTRA =
struct
  structure A = Sequence (Primitive.ArrayAos)
  open A

  val op < = Int.<
  val op <= = Int.<=

  fun wrap2 f = fn (i, x) => f (SeqIndex.toIntUnsafe i, x)

  type 'a array = 'a ArrayAos.t
  type 'a vector = 'a VectorAos.vector

  structure ArraySlice =
  struct
    open Slice
    val vector = Primitive.ArrayAos.Slice.vector
    val copyVec = VectorAos.VectorSlice.copy
    val unsafeCopyVec = VectorAos.VectorSlice.unsafeCopy
    fun modifyi f sl = Primitive.ArrayAos.Slice.modifyi (wrap2 f) sl
    val modify = Primitive.ArrayAos.Slice.modify
  end

  val array = new
  val unsafeArray = unsafeNew
  val vector = Primitive.ArrayAos.vector
  val copyVec = VectorAos.copy
  val unsafeCopyVec = VectorAos.unsafeCopy
  fun modifyi f sl = Primitive.ArrayAos.modifyi (wrap2 f) sl
  val modify = Primitive.ArrayAos.modify
  structure Raw = Primitive.ArrayAos.Raw
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

    val unsafeToArray = Primitive.ArrayAos.Raw.unsafeToArray
  end
end

structure ArrayAosSlice: ARRAY_AOS_SLICE_EXTRA = ArrayAos.ArraySlice
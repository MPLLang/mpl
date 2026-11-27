structure ArraySoa: ARRAY_SOA_EXTRA =
struct
  structure A = Sequence (Primitive.ArraySoa)
  open A

  val op < = Int.<
  val op <= = Int.<=

  fun wrap2 f = fn (i, x) => f (SeqIndex.toIntUnsafe i, x)

  type 'a array = 'a ArraySoa.t
  type 'a vector = 'a VectorSoa.vector

  structure ArraySlice =
  struct
    open Slice
    val vector = Primitive.ArraySoa.Slice.vector
    val copyVec = VectorSoa.VectorSlice.copy
    val unsafeCopyVec = VectorSoa.VectorSlice.unsafeCopy
    fun modifyi f sl = Primitive.ArraySoa.Slice.modifyi (wrap2 f) sl
    val modify = Primitive.ArraySoa.Slice.modify
  end

  val array = new
  val unsafeArray = unsafeNew
  val vector = Primitive.ArraySoa.vector
  val copyVec = VectorSoa.copy
  val unsafeCopyVec = VectorSoa.unsafeCopy
  fun modifyi f sl = Primitive.ArraySoa.modifyi (wrap2 f) sl
  val modify = Primitive.ArraySoa.modify
  structure Raw = Primitive.ArraySoa.Raw
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

    val unsafeToArray = Primitive.ArraySoa.Raw.unsafeToArray
  end
end

structure ArraySoaSlice: ARRAY_SOA_SLICE_EXTRA = ArraySoa.ArraySlice
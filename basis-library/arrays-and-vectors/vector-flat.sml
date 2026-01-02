structure VectorFlat: VECTOR_FLAT_EXTRA =
struct
  structure V = Sequence(Primitive.VectorFlat)
  open V

  type 'a vector = 'a VectorFlat.t

  structure VectorSlice =
  struct
    open Slice
    type 'a vector = 'a vector
    val vector = sequence

    val isSubvector = isSubsequence
    val span = fn (sl, sl') =>
      Primitive.VectorFlat.Slice.span (op= : ''a vector * ''a vector -> bool)
        (sl, sl')
  end

  fun update (v, i, x) =
    (Primitive.VectorFlat.updateVector (v, SeqIndex.fromInt i, x))
    handle Overflow => raise Subscript

  val isSubvector = isSubsequence

  val unsafeFromArray = Primitive.VectorFlat.unsafeFromArray

  val vector = new
end

structure VectorFlatSlice: VECTOR_FLAT_SLICE_EXTRA = VectorFlat.VectorSlice

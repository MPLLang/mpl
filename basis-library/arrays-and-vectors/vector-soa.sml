structure VectorSoa: VECTOR_SOA_EXTRA =
struct
  structure V = Sequence(Primitive.VectorSoa)
  open V

  type 'a vector = 'a VectorSoa.t

  structure VectorSlice =
  struct
    open Slice
    type 'a vector = 'a vector
    val vector = sequence

    val isSubvector = isSubsequence
    val span = fn (sl, sl') =>
      Primitive.VectorSoa.Slice.span (op= : ''a vector * ''a vector -> bool)
        (sl, sl')
  end

  fun update (v, i, x) =
    (Primitive.VectorSoa.updateVector (v, SeqIndex.fromInt i, x))
    handle Overflow => raise Subscript

  val isSubvector = isSubsequence

  val unsafeFromArray = Primitive.VectorSoa.unsafeFromArray

  val vector = new
end

structure VectorSoaSlice: VECTOR_SOA_SLICE_EXTRA = VectorSoa.VectorSlice

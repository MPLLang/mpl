structure VectorAos: VECTOR_AOS_EXTRA =
struct
  structure V = Sequence(Primitive.VectorAos)
  open V

  type 'a vector = 'a VectorAos.t

  structure VectorSlice =
  struct
    open Slice
    type 'a vector = 'a vector
    val vector = sequence

    val isSubvector = isSubsequence
    val span = fn (sl, sl') =>
      Primitive.VectorAos.Slice.span (op= : ''a vector * ''a vector -> bool)
        (sl, sl')
  end

  fun update (v, i, x) =
    (Primitive.VectorAos.updateVector (v, SeqIndex.fromInt i, x))
    handle Overflow => raise Subscript

  val isSubvector = isSubsequence

  val unsafeFromArray = Primitive.VectorAos.unsafeFromArray

  val vector = new
end

structure VectorAosSlice: VECTOR_AOS_SLICE_EXTRA = VectorAos.VectorSlice

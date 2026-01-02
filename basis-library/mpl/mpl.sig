(* Copyright (C) 2020 Sam Westrick.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MPL =
sig
  structure File: MPL_FILE
  structure GC: MPL_GC

  structure ArrayFlat: ARRAY_FLAT_EXTRA
  structure ArrayFlatSlice: ARRAY_FLAT_SLICE_EXTRA

  structure VectorFlat: VECTOR_FLAT_EXTRA
  structure VectorFlatSlice: VECTOR_FLAT_SLICE_EXTRA
end

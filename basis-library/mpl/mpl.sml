(* Copyright (C) 2020 Sam Westrick.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MPL: MPL =
struct
  structure File = MPLFile
  structure GC = MPLGC

  structure ArrayFlat = ArrayFlat
  structure ArrayFlatSlice = ArrayFlatSlice

  structure VectorFlat = VectorFlat
  structure VectorFlatSlice = VectorFlatSlice
end

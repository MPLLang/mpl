(* Copyright (C) 2020 Sam Westrick.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Primitive = struct
open Primitive

structure MPL =
struct

  structure ForkJoin =
  struct
    type 'a thunk = unit -> 'a
    val parWrapper = _prim "parWrap":
      ('a thunk * 'b thunk -> 'a * 'b) * 'a thunk * 'b thunk -> 'a * 'b;
  end

  structure File =
  struct
    val copyCharsToBuffer = _import "GC_memcpyToBuffer" runtime private:
      Pointer.t * Char8.t array * C_Size.word * C_Size.word -> unit;
    val copyWord8sToBuffer = _import "GC_memcpyToBuffer" runtime private:
      Pointer.t * Word8.word array * C_Size.word * C_Size.word -> unit;
    val mmapFileReadable = _import "GC_mmapFileReadable" runtime private:
      C_Int.int * C_Size.word -> Pointer.t;
    val release = _import "GC_release" runtime private:
      Pointer.t * C_Size.word -> unit;
  end

end

end

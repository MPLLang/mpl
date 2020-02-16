(* Copyright (C) 2020 Sam Westrick.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MPL_FILE =
sig
  type t

  exception Closed

  val openFile: string -> t
  val closeFile: t -> unit
  val size: t -> int

  val readChar: t -> int -> char
  val readWord8: t -> int -> Word8.word
  val unsafeReadChar: t -> int -> char
  val unsafeReadWord8: t -> int -> Word8.word
end

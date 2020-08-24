(* Copyright (C) 2020 Sam Westrick.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MPL =
sig
  structure File: MPL_FILE
  structure GC: MPL_GC
end

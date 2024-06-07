(* Copyright (C) 2024 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature EXN_DEC_ELAB_STRUCTS =
   sig
   end

signature EXN_DEC_ELAB =
   sig
      include EXN_DEC_ELAB_STRUCTS

      datatype t = App | Gen

      val layout: t -> Layout.t
      val parse: t Parse.t
   end

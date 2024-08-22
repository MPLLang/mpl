(* Copyright (C) 2024 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature INLINE_ATTR_STRUCTS =
   sig
   end

signature INLINE_ATTR =
   sig
      include INLINE_ATTR_STRUCTS

      datatype t =
         Always
       | Auto
       | Never

      val equals: t * t -> bool
      val hash: t -> word
      val layout: t -> Layout.t
      val mayInline: t -> bool
      val mustInline: t -> bool
      val parse: t Parse.t
   end

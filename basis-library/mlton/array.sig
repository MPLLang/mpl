(* Copyright (C) 2022 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_ARRAY =
   sig
      val unfoldi: int * 'b * (int * 'b -> 'a * 'b) -> 'a array * 'b
   end

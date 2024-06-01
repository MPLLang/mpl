(* Copyright (C) 2024 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ExnDecElab (S: EXN_DEC_ELAB_STRUCTS): EXN_DEC_ELAB =
struct

open S

open Control.Elaborate.ExnDecElab

val all = [App, Gen]

val layout = Layout.str o toString

val parse =
   let
      open Parse
      infix 3 *>
   in
      any (List.map (all, fn ede => kw (toString ede) *> pure ede))
   end

end

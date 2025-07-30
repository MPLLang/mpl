(* Copyright (C) 2024 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor InlineAttr(S: INLINE_ATTR_STRUCTS): INLINE_ATTR =
struct
open S

datatype t =
   Always
 | Auto
 | Never

fun mayInline i =
   case i of
      Always => true
    | Auto => true
    | Never => false

fun mustInline i =
   case i of
      Always => true
    | Auto => false
    | Never => false

val all = [Always, Auto, Never]

fun equals (i, i'): bool =
   case (i, i') of
      (Always, Always) => true
    | (Auto, Auto) => true
    | (Never, Never) => true
    | _ => false

fun join (i, i'): t =
    case (i, i') of
        (Never, _) => Never
      | (_, Never) => Never
      | (Always, _) => Always
      | (_, Always) => Always
      | (Auto, Auto) => Auto

fun toString i =
   case i of
      Always => "Always"
    | Auto => "Auto"
    | Never => "Never"

val layout = Layout.str o toString

val parse =
   let
      open Parse
      infix  3 *>
      fun kw s =
         spaces *> str s *>
         failing (nextSat (fn c => Char.isAlphaNum c orelse c = #"_"))
   in
      any (List.map (all, fn t => kw (toString t) *> pure t))
   end

local
   val newHash = Random.word
   val always = newHash ()
   val auto = newHash ()
   val never = newHash ()
in
   fun hash i =
      case i of
         Always => always
       | Auto => auto
       | Never => never
end

end

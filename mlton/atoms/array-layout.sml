(* Memory layouts for arrays. *)
structure ArrayLayout :>
sig
  datatype t = Default | Aos

  val equals: t * t -> bool

  (* confusing naming: layout here means to produce a Layout.t
   * representation (for pretty printing) of the ArrayLayout.t value
   *)
  val layout: t -> Layout.t

  val toString: t -> string
end =
struct
  datatype t = Default | Aos

  fun equals (Default, Default) = true
    | equals (Aos, Aos) = true
    | equals _ = false

  fun toString lay =
    case lay of
      Default => "Default"
    | Aos => "Aos"

  fun layout lay = Layout.str (toString lay)

end
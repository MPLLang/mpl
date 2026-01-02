(* Memory layouts for arrays. *)
structure ArrayLayout :>
sig
  datatype t = Default | Flattened

  val equals: t * t -> bool

  (* confusing naming: layout here means to produce a Layout.t
   * representation (for pretty printing) of the ArrayLayout.t value
   *)
  val layout: t -> Layout.t

  val toString: t -> string
end =
struct
  datatype t = Default | Flattened

  fun equals (Default, Default) = true
    | equals (Flattened, Flattened) = true
    | equals _ = false

  fun toString lay =
    case lay of
      Default => "Default"
    | Flattened => "Flattened"

  fun layout lay = Layout.str (toString lay)

end
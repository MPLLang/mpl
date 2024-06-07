signature SPID_STRUCTS =
   sig
   end

signature SPID =
sig
  type t
  val new: unit -> t
  val equals: t * t -> bool
  val parse: t Parse.t
  val layout: t -> Layout.t
  val toString: t -> string
  val hash: t -> Word.t
end

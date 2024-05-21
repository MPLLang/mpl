signature SPID =
sig
  type t
  val new: unit -> t
  val equals: t * t -> bool
  val parse: t Parse.t
  val layout: t -> Layout.t
  val hash: t -> Word.t
end

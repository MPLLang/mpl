signature QUEUE =
sig
  type 'a t

  val new : unit -> 'a t
  val empty : 'a t -> bool
  val pushBot : 'a * 'a t -> unit
  val popBotDiscard : 'a t -> bool
  val popTop : 'a t -> 'a option

end

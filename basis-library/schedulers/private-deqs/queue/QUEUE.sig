signature QUEUE =
sig
  type 'a t

  val new : unit -> 'a t
  val empty : 'a t -> bool

  val pushBot : 'a * 'a t -> unit

  val popBotDiscard : 'a t -> bool
  val popBot : 'a t -> 'a option
  val popTop : 'a t -> 'a option

  val peekBot : 'a t -> 'a option
  val peekTop : 'a t -> 'a option

end

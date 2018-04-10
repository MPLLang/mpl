signature QUEUE =
sig
  type 'a t

  (* new p -- registers this queue on processor p *)
  val new : int -> 'a t
  val empty : 'a t -> bool

  val pushFront : 'a * 'a t -> unit

  val popFront : 'a t -> 'a option
  val popBack : 'a t -> 'a option

  val toList : 'a t -> 'a list

  (* val peekFront : 'a t -> 'a option
  val peekBack : 'a t -> 'a option *)

end

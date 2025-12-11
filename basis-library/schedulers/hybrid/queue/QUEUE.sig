signature QUEUE =
sig
  type 'a t

  val new : int -> 'a t
  val empty : 'a t -> bool

  val setDepth : 'a t -> int -> unit

  val pushFront : 'a * 'a t -> unit

  val popFront : 'a t -> 'a option
  val popBack : 'a t -> ('a * int) option

  val toList : 'a t -> 'a list

  (* val peekFront : 'a t -> 'a option
  val peekBack : 'a t -> 'a option *)

end

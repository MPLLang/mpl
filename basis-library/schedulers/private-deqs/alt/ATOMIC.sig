signature ATOMIC =
sig

  (* `print f` atomically prints f() *)
  val print : (unit -> string) -> unit

  type 'a t
  val new : 'a -> 'a t
  val write : 'a t * 'a -> unit
  val read : 'a t -> 'a
end

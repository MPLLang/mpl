signature FUTURE =
sig
  type 'a t
  val future : (unit -> 'a) -> 'a t
  val force : 'a t -> 'a
  val poll : 'a t -> 'a option
end

signature FORKJOIN =
sig
  val fork : (unit -> 'a) * (unit -> 'b) -> 'a * 'b
end

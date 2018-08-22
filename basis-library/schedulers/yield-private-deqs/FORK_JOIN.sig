signature FORK_JOIN =
sig
  val fork : (unit -> 'a) * (unit -> 'b) -> 'a * 'b
end

signature ASYNC_FINISH =
sig
  (*type async = (unit -> unit) -> unit
  val finish : (async -> 'a) -> 'a*)

  val parallelFor : int -> int * int -> (int -> unit) -> unit
end

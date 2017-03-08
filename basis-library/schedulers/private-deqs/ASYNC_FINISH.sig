signature ASYNC_FINISH =
sig
  type async = (unit -> unit) -> unit
  val finish : (async -> 'a) -> 'a
end

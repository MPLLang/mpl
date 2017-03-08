structure AsyncFinish :> ASYNC_FINISH =
struct
  type async = (unit -> unit) -> unit
  val finish = Scheduler.finish
end

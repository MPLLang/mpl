structure AsyncFinish :> ASYNC_FINISH =
struct
  exception AsyncFinish
  type async = (unit -> unit) -> unit
  fun finish _ = (*Scheduler.finish*) raise AsyncFinish
end

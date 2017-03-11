structure AsyncFinish :> ASYNC_FINISH =
struct
  exception AsyncFinish

  type work = unit -> unit
  type async = work -> unit

  fun finish body =
    let
      val join = Scheduler.new ()
      fun async work = Scheduler.push (join, work)
      val result = body async
    in ( Scheduler.sync join
       ; result
       )
    end
end

structure AsyncFinish :> ASYNC_FINISH =
struct

  exception AsyncFinish

  type task = unit -> unit
  type async = task -> unit

  fun finish body =
    let
      val join = Scheduler.new ()
      val result = body (Scheduler.push join)
    in ( Scheduler.sync join
       ; result
       )
    end

end

structure AsyncFinish :> ASYNC_FINISH =
struct

  exception AsyncFinish

  (*type task = unit -> unit
  type async = task -> unit*)

  (*fun finish body =
    let
      val join = Scheduler.new ()
      val result = body (Scheduler.push join)
    in ( Scheduler.sync join
       ; result
       )
    end*)

  fun serialFor (lo, hi) f =
    if lo = hi then () else (f lo; serialFor (lo+1, hi) f)

  fun parallelFor gran (lo, hi) f =
    let
      val _ = print "Warning: running AsyncFinish.parallelFor, which is buggy\n"
      val join = Scheduler.new ()

      fun doRange master (lo, hi) () =
        if hi - lo <= gran then serialFor (lo, hi) f
        else let val mid = lo + (hi - lo) div 2
             in ( Scheduler.push join (doRange false (mid, hi))
                ; doRange master (lo, mid) ()
                ; if Scheduler.popDiscard () then doRange master (mid, hi) ()
                  else if master then Scheduler.sync join
                  else ()
                )
             end
    in
      doRange true (lo, hi) ()
    end

end

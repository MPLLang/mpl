structure ForkJoin :> FORKJOIN =
struct

  fun fork (f : unit -> 'a, g : unit -> 'b) =
    let
      val gt = Scheduler.push g
      val a = f ()
    in
      if Scheduler.pop () then (a, g ())
      else (a, Scheduler.sync gt)
    end

end

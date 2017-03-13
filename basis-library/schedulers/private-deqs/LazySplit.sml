structure LazySplit :>
sig
  val parallelFor : int -> int * int -> (int -> unit) -> unit
end =
struct

  exception LazySplit

  fun serialFor (lo, hi) f =
    if lo = hi then () else (f lo; serialFor (lo+1, hi) f)

  (* A failed experiment. It seems we need scheduler support for lazy
   * splitting. *)
  fun parallelFor gran (lo, hi) f =
    raise LazySplit
    (*let
      val join = Scheduler.new ()

      fun execute (lo, hi) () =
        if hi - lo <= gran then serialFor (lo, hi) f
        else let val mid = lo + (hi - lo) div 2
             in ( Scheduler.push join (execute (mid, hi))
                ; serialFor (lo, lo+gran) f
                ; if Scheduler.popDiscard ()
                  then execute (lo+gran, hi) ()
                  else execute (lo+gran, mid) ()
                )
             end
    in
      ( execute (lo, hi) ()
      ; Scheduler.sync join
      )
    end*)

end

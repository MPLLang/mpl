signature SCHEDULER =
sig
  (* general errors related to parallelism *)
  exception Parallel of string

  (* a handle on an exposed thunk *)
  type 'a t

  (* push a thunk onto the work stack *)
  val push : (unit -> 'a) -> 'a t

  (* attempt to pop a thunk off the work stack; if it fails, then
   * the desired piece of work must have been stolen. *)
  val pop : unit -> bool

  (* wait for a stolen piece of work to complete *)
  val sync : 'a t -> 'a


  (* async-finish *)
  type async = (unit -> unit) -> unit
  val finish : (async -> 'a) -> 'a

end

(*fun parallelFor granularity (lo, hi) f =
  let
    fun serial (lo, hi) = if lo = hi then () else (f lo; serial (lo+1, hi))
    fun parallel (lo, hi) async =
      if hi - lo <= granularity then serial (lo, hi)
      else let val mid = lo + (hi - lo) div 2
           in ( async (fn () => parallel (lo, mid) async)
              ; async (fn () => parallel (mid, hi) async)
              )
           end
  in
    Scheduler.finish (parallel (lo, hi))
  end*)

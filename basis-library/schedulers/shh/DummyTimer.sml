functor DummyTimer(val timerName: string):
sig
  val start: unit -> unit
  val tick: unit -> unit   (* Essentially the same as (stop();start()) *)
  val stop: unit -> unit

  val isStarted: unit -> bool

  val cumulative: unit -> Time.time
end =
struct

  fun isStarted () = false
  fun start () = ()
  fun tick () = ()
  fun stop () = ()
  fun cumulative () = Time.zeroTime

end
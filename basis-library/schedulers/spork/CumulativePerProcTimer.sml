functor CumulativePerProcTimer(val timerName: string):
sig
  val start: unit -> unit
  val tick: unit -> unit   (* Essentially the same as (stop();start()) *)
  val stop: unit -> unit

  val isStarted: unit -> bool

  val cumulative: unit -> Time.time
end =
struct

  val numP = MLton.Parallel.numberOfProcessors
  fun myId () = MLton.Parallel.processorNumber ()

  fun die strfn =
    ( print (Int.toString (myId ()) ^ ": " ^ strfn ())
    ; OS.Process.exit OS.Process.failure
    )

  val totals = Array.array (numP, Time.zeroTime)
  val starts = Array.array (numP, Time.zeroTime)
  val isRunning = Array.array (numP, false)

  fun isStarted () =
    Array.sub (isRunning, myId())

  fun start () =
    let
      val p = myId()
    in
      if Array.sub (isRunning, p) then
        die (fn _ => "timer \"" ^ timerName ^ "\": start after start")
      else
        ( Array.update (isRunning, p, true)
        ; Array.update (starts, p, Time.now ())
        )
    end

  fun tick () =
    let
      val p = myId()
      val tnow = Time.now ()
      val delta = Time.- (tnow, Array.sub (starts, p))
    in
      if not (Array.sub (isRunning, p)) then
        die (fn _ => "timer \"" ^ timerName ^ "\": tick while stopped")
      else
        ( Array.update (totals, p, Time.+ (Array.sub (totals, p), delta))
        ; Array.update (starts, p, tnow)
        )
    end

  fun stop () =
    let
      val p = myId()
      val tnow = Time.now ()
      val delta = Time.- (tnow, Array.sub (starts, p))
    in
      if not (Array.sub (isRunning, p)) then
        die (fn _ => "timer \"" ^ timerName ^ "\": stop while stopped")
      else
        ( Array.update (isRunning, p, false)
        ; Array.update (totals, p, Time.+ (Array.sub (totals, p), delta))
        )
    end

  fun cumulative () =
    ( if isStarted () then tick () else ()
    ; Array.foldl Time.+ Time.zeroTime totals
    )

end
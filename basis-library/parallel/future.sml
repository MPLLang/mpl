functor MLtonParallelFuture (structure V : MLTON_PARALLEL_SYNCVAR
                             structure B : sig 
                               type void
                               val add : (unit -> void) -> unit
                               val start : unit -> unit
                               val stop : unit -> unit
                               val return : unit -> void
                             end)
        :> MLTON_PARALLEL_FUTURE =
struct

  val suspends = ref 0
  fun incr r = r := !r + 1

  datatype 'a result =
      Finished of 'a
    | Raised of exn

  type 'a t = 'a result V.t

  fun future f =
    let 
      val v = V.empty ()
      val _ = B.add (fn () => (B.start ();
                               V.write (v, Finished (f ())
                                           handle e => Raised e);
                               B.stop ();
                               B.return ()))
    in
      v
    end

  fun touch v =
      let
        val (susp, a) = V.read v
        val () = if susp then incr suspends else ()
      in
        case a of 
          Finished v => v
        | Raised e => raise e
      end

  fun reportSuspends () = !suspends
  fun resetSuspends () = suspends := 0

end

local
structure NoDelay =
struct
  open MLtonParallelBasic
  val add = ignore o addLeft
  fun start () = ()
  fun stop () = ()
end
structure Delay =
struct
  open MLtonParallelBasic
  val add = delayedAdd
  fun start () = ()
  fun stop () = ()
end
structure MaybeDelay =
struct
  open MLtonParallelBasic
  open MLtonParallelInternal
  val inFuture = Array.array (numberOfProcessors, false)
  fun add f = if Array.sub (inFuture, processorNumber()) then
                (delayedAdd f)
              else
                (ignore (addLeft f))
  fun start () = Array.update (inFuture, processorNumber (), true)
  fun stop () = Array.update (inFuture, processorNumber (), false)
end
in
structure MLtonParallelFutureSuspend = 
  MLtonParallelFuture (structure V = MLtonParallelSyncVarSuspend
                       structure B = NoDelay)
structure MLtonParallelFutureSuspendDelay = 
  MLtonParallelFuture (structure V = MLtonParallelSyncVarSuspend
                       structure B = Delay)
structure MLtonParallelFutureSuspendMaybeDelay = 
  MLtonParallelFuture (structure V = MLtonParallelSyncVarSuspend
                       structure B = MaybeDelay)
structure MLtonParallelFutureCapture = 
  MLtonParallelFuture (structure V = MLtonParallelSyncVarCapture
                       structure B = NoDelay)
structure MLtonParallelFutureCaptureDelay = 
  MLtonParallelFuture (structure V = MLtonParallelSyncVarCapture
                       structure B = Delay)
structure MLtonParallelFutureCaptureMaybeDelay = 
  MLtonParallelFuture (structure V = MLtonParallelSyncVarCapture
                       structure B = MaybeDelay)
end

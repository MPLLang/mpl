functor MLtonParallelFuture (structure V : MLTON_PARALLEL_SYNCVAR
                             structure B : sig
                               type void
                               val addPrio : Priority.t * (unit -> void) -> unit
                               val start : unit -> unit
                               val stop : unit -> unit
                               val return : unit -> void
                                       end
                             )
        :> MLTON_PARALLEL_FUTURE =
struct

  val suspends = ref 0
  fun incr r = r := !r + 1

  datatype 'a result =
      Finished of 'a
    | Raised of exn

  type 'a t = 'a result V.t

  fun futurePrio (prio, f) =
    let
      val v = V.empty ()
      val _ = B.addPrio (prio, (fn () => (B.start ();
                               V.write (v, Finished (f ())
                                           handle e => Raised e);
                               B.stop ();
                               B.return ())))
    in
      v
    end

  fun future f = futurePrio (Priority.bot, f)

  fun touch v =
      let
        val (susp, a) = V.read v
        val () = if susp then incr suspends else ()
      in
        case a of
          Finished v => v
        | Raised e => raise e
      end

  fun poll v =
      case V.poll v of
          NONE => NONE
        | SOME (Finished a) => SOME a
        | SOME (Raised e) => raise e

  fun reportSuspends () = !suspends
  fun resetSuspends () = suspends := 0

end

local
structure NoDelay =
struct
  open MLtonParallelBasic
  val addPrio = ignore o addRightPrio
  fun start () = ()
  fun stop () = ()
end
in
structure MLtonParallelFutureSuspend =
  MLtonParallelFuture (structure V = MLtonParallelSyncVarSuspend
                       structure B = NoDelay)
end

functor MLtonParallelSyncVar (structure B : MLTON_PARALLEL_BASIC)
        :> MLTON_PARALLEL_SYNCVAR =
struct

  datatype 'a state =
      Waiting of (bool * 'a) B.t list
    | Done of 'a

  type 'a t = int ref * 'a state ref

  val lock = _import "Parallel_lockTake" runtime private: int ref -> unit;
  val unlock = _import "Parallel_lockRelease" runtime private: int ref -> unit;

  fun empty () = (ref ~1, ref (Waiting nil))

  fun write ((r, v), a) =
      case !v
       of Done _ => raise B.Parallel "two writes to sync var!"
        | Waiting _ =>
          let
            (* First take the lock *)
              (* val () = print "locking\n" *)
            val () = lock r
            (* val () = print "got lock\n" *)
            (* Now read the wait list *)
            val readers = case !v
                           of Waiting w => w
                            | Done _ => raise B.Parallel "async write to sync var!"
            val () = v := Done a
            val () = unlock r
            (* val () = print ("unlocked, waking " ^ (Int.toString (List.length readers)) ^ " readers\n") *)
          in
            (* Add readers to the queue *)
            app (fn k => B.resume (k, (true, a))) readers
            (* print "done\n" *)
          end

  fun read (r, v) =
      case !v
       (* Do the easy case first *)
       of Done a => (false, a)
        (* Synchronization required... *)
        | Waiting _ =>
          let
            (* Take the lock *)
            val () = lock r
          in
            (* Read again *)
            case !v
             of Done a =>
                (unlock r; (false, a))
              | Waiting readers =>
                B.suspend (fn k => (v := Waiting (k::readers); unlock r))
          end

  fun poll (r, v) =
      case !v of
          Done a => SOME a
        | Waiting _ => NONE

(*
  fun read (r, v) =
      case !v
       (* Do the easy case first *)
       of Done a => a
        (* Synchronization required... *)
        | s as Waiting ks =>
          B.suspend (fn k =>
            if compareAndSwap (v, s, Waiting k::ks) then ()
            else valOf (!b)
*)
end

structure MLtonParallelSyncVarSuspend = MLtonParallelSyncVar (structure B = MLtonParallelBasic)
structure MLtonParallelSyncVarCapture = MLtonParallelSyncVar (structure B = struct
                                                                open MLtonParallelBasic
                                                                val suspend = capture
                                                              end)

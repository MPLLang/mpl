functor MLtonParallelSyncVar (structure B : MLTON_PARALLEL_BASIC)
        :> MLTON_PARALLEL_SYNCVAR =
struct

  datatype 'a state =
      Waiting of (bool * 'a) B.t list
    | Done of 'a

  type 'a t = (*int ref*) Word32.word ref * 'a state ref (* shwestrick *)

  (* shwestrick *)
  (*val lock = _import "Parallel_lockTake" runtime private: int ref -> unit;
  val unlock = _import "Parallel_lockRelease" runtime private: int ref -> unit;*)
  val lockInit = MLton.Parallel.Deprecated.lockInit
  val lock = MLton.Parallel.Deprecated.takeLock
  val unlock = MLton.Parallel.Deprecated.releaseLock

  (* shwestrick *)
  fun newLock () =
    let val x = ref 0w0
    in (lockInit x; x)
    end

  fun empty () = ((*ref ~1*) newLock (), ref (Waiting nil)) (* shwestrick *)

  (* shwestrick: copied guyb's fix *)
  fun write ((r, v), a) = (lock r;
      case !v
       of Done _ => (unlock r; raise B.Parallel "two writes to sync var!")
        | Waiting readers =>
          let
            val () = v := Done a
            val () = unlock r
          in
            (* Add readers to the queue *)
            app (fn k => B.resume (k, (true, a))) readers
          end)

  fun read (r, v) = (lock r;
      case !v
       (* Do the easy case first *)
       of Done a => (unlock r; (false, a))
        (* Synchronization required... *)
        | Waiting readers => B.suspend (fn k => (v := Waiting (k::readers); unlock r)))

(*fun write ((r, v), a) =
      case !v
       of Done _ => raise B.Parallel "two writes to sync var!"
        | Waiting _ =>
          let
            (* First take the lock *)
            val () = lock r
            (* Now read the wait list *)
            val readers = case !v
                           of Waiting w => w
                            | Done _ => raise B.Parallel "async write to sync var!"
            val () = v := Done a
            val () = unlock r
          in
            (* Add readers to the queue *)
            app (fn k => B.resume (k, (true, a))) readers
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
          end*)

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

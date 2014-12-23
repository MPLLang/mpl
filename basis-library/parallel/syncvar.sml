functor MLtonParallelSyncVar (structure B : MLTON_PARALLEL_BASIC)
        :> MLTON_PARALLEL_SYNCVAR =
struct

  datatype 'a state =
      Waiting of (bool * 'a) B.t list
    | Done of 'a

  type 'a t = int ref * 'a state ref

  val enterGlobalHeap = MLtonParallelInternal.enterGlobalHeap
  val exitGlobalHeap = MLtonParallelInternal.exitGlobalHeap
  val lock = _import "Parallel_lockTake" runtime private: int ref -> unit;
  val unlock = _import "Parallel_lockRelease" runtime private: int ref -> unit;

  fun empty () = (ref ~1, ref (Waiting nil))

  local
      fun doWrite ((r, v), a) =
          case !v of
              Done _ => raise B.Parallel "two writes to sync var!"
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
  in
      fun write argument =
          let
              val () = enterGlobalHeap ()
              val result = doWrite argument
                           handle e => (exitGlobalHeap ();
                                        raise e)
              val () = exitGlobalHeap ()
          in
              result
          end
  end

  local
      fun doRead (r, v) =
          case !v of
              Done a => (false, a) (* Do the easy case first *)
            | Waiting _ =>
              (* Synchronization required... *)
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
  in
      fun read argument =
          let
              val () = enterGlobalHeap ()
              val result = doRead argument
              val () = exitGlobalHeap ()
          in
              result
          end
  end

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

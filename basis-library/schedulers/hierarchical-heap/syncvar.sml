structure MLtonParallelSyncVar :> MLTON_PARALLEL_SYNCVAR =
struct
  structure B = MLtonParallelBasic
  structure HM = MLton.HM

  datatype 'a state =
      Waiting of B.t list
    | Done of 'a

  type 'a t = Word32.word ref * 'a state ref

  val lockInit = MLton.Parallel.Deprecated.lockInit
  val lock = MLton.Parallel.Deprecated.takeLock
  val unlock = MLton.Parallel.Deprecated.releaseLock

  fun empty () =
      let
        val r = ref (Word32.fromInt 0)
        val () = lockInit r
      in
        (r, ref (Waiting nil))
      end

  fun die message =
      (TextIO.output (TextIO.stdErr, message);
       TextIO.flushOut TextIO.stdErr;
       OS.Process.exit OS.Process.failure)

  fun checkZeroReader list =
      case list
       of [] => ()
        | _ => die "syncvar.sml: reader already waiting!\n"

  fun checkMaxOneReader list =
      case list
       of [] => ()
        | [_] => ()
        | _ => die "syncvar.sml: more than one reader waiting?!\n"

  fun write ((r, v), a) =
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
            val () = checkMaxOneReader readers
          in
            app (fn k => B.resume k) readers
          end

  fun read (r, v) =
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
                (unlock r;
                 (false, a))
              | Waiting readers =>
                (checkZeroReader readers;
                 B.capture (fn k => (v := Waiting (k::readers);
                                     unlock r));
                 (case !v
                   of Done a => (true, a)
                    | Waiting _ => die "Syncvar unwritten after resume."))
          end
end

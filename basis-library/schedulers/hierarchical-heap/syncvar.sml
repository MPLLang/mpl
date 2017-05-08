structure MLtonParallelSyncVar :> MLTON_PARALLEL_SYNCVAR =
struct
  structure B = MLtonParallelBasic
  structure HM = MLton.HM

  val dbgmsg =
      if false
      then fn m => MLtonParallelInternal.dbgmsg ("syncvar: " ^ m)
      else fn _ => ()

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
      (lock r;
       (case !v of
            Done _ => (unlock r; raise B.Parallel "two writes to sync var!")
          | Waiting readers =>
            let
              val () = v := Done a
              val () = dbgmsg "Wrote to syncvar"
              val () = unlock r
              val () = checkMaxOneReader readers
            in
              app (fn k => B.resume k) readers
            end))

  fun read (r, v) =
      (lock r;
       (case !v of
            Done a => (unlock r; (false, a)) (* Do the easy case first *)
          | Waiting readers =>
            (checkZeroReader readers;
             dbgmsg "suspending waiting for syncvar";
             B.capture (fn k => (v := Waiting (k::readers);
                                 unlock r));
             dbgmsg "resuming before reading syncvar";
             lock r;
             (case !v
               of Done a => (unlock r; (true, a))
                | Waiting _ => (unlock r; die "Syncvar unwritten after resume.")))))
end

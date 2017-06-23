structure MLtonParallelSyncVar :> MLTON_PARALLEL_SYNCVAR =
struct
  structure B = MLtonParallelBasic
  structure HM = MLton.HM

  val dbgmsg: (unit -> string) -> unit =
      if false
      then fn m => MLtonParallelInternal.dbgmsg ("syncvar: " ^ (m ()))
      else fn _ => ()

  datatype state =
           Waiting of B.t list
         | Done

  type 'a t = Word32.word ref * state ref * 'a option ref

  val lockInit = MLton.Parallel.Deprecated.lockInit
  val lock = MLton.Parallel.Deprecated.takeLock
  val unlock = MLton.Parallel.Deprecated.releaseLock

  fun empty () =
      let
        val r = ref (Word32.fromInt 0)
        val () = lockInit r
      in
        (r, ref (Waiting nil), ref NONE)
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

  fun write ((r, s, v), a) =
      (lock r;
       (case !s of
            Done => (unlock r; raise B.Parallel "two writes to sync var!")
          | Waiting readers =>
            let
              val () = v := SOME a
              val () = s := Done
              val () = dbgmsg (fn () => "Wrote to syncvar")
              val () = unlock r
              val () = checkMaxOneReader readers
            in
              app (fn k => B.resume k) readers
            end))

  fun read (r, s, v) =
      (lock r;
       (case !s of
            Done => (unlock r; (false, valOf (!v))) (* Do the easy case first *)
          | Waiting readers =>
            (checkZeroReader readers;
             dbgmsg (fn () => "suspending waiting for syncvar");
             B.capture (fn k => (s := Waiting (k::readers);
                                 unlock r));
             dbgmsg (fn () => "resuming before reading syncvar");
             lock r;
             (case !s
               of Done  => (unlock r;
                             dbgmsg
                                 (fn () => "read syncvar");
                             (true, valOf (!v)))
                | Waiting _ => (unlock r; die "Syncvar unwritten after resume.")))))
end

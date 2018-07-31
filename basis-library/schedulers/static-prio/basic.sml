structure MLtonParallelBasic :> MLTON_PARALLEL_BASIC =
struct

  type void = unit
  type work = unit -> void

  structure T = MLton.Thread

  datatype job = Work of (unit -> void)
               | Thread of (unit T.t)

  val numberOfProcessors = MLtonParallelInternal.numberOfProcessors

  structure Q = WorkQueue (struct
                             type work = job
                             val numberOfProcessors = fn () => numberOfProcessors
                           end)
    :> PARALLEL_WORKQUEUE where type work = job

  datatype 'a t = Suspend of Priority.t * 'a T.t * Q.susp
                | Capture of Priority.t * 'a T.t

  type token = Q.token

  val stringOfToken = Q.stringOfToken

  val processorNumber = MLtonParallelInternal.processorNumber
  val profileDisable = _import "GC_profileDisable" runtime private: unit -> unit;
  val profileEnable = _import "GC_profileEnable" runtime private: unit -> unit;

  exception Parallel of string

  val suspends = Array.array (numberOfProcessors, 0)
  fun incSuspends p = Array.update (suspends, p, Array.sub (suspends, p) + 1)

  val delayed = Array.array (numberOfProcessors, nil)

  val enabled = ref true

  fun currentPrio () = Q.workOnPriority numberOfProcessors (processorNumber ())

  val ioqueues = Array.array (numberOfProcessors, [])

  fun prerr s = (TextIO.output (TextIO.stdErr, s);
                 TextIO.flushOut TextIO.stdErr)

  fun addtoio ((t, f) : unit t * (unit -> bool)) =
      let val p = processorNumber ()
          (* val _ = print ("addtoio at " ^ (Int.toString p) ^ "\n") *)
          val q = Array.sub (ioqueues, p)
      in
          Array.update (ioqueues, p, (t, f)::q)
      end


  fun resume (Suspend (prio, k, q), v) =
      let
        val p = processorNumber ()
      in
        Q.resumeWork (prio, p, q, (Q.newWork p, Thread (T.prepend (k, fn () => v))))
        handle e => (print "here 59\n"; raise e)
      end
    | resume (Capture (prio, k), v) =
      let
        val p = processorNumber ()
        (* val _ = print "resuming\n" *)
      in
        Q.addWork (prio, p, [(Q.newWork p, Thread (T.prepend (k, fn () => v)))])
        handle e => (print "here 67\n"; raise e)
      end

  fun latency (Suspend (prio, _, _)) = prio
    | latency (Capture (prio, _)) = prio

                                        (*
  fun mkLat (Suspend (_, k, q)) = Suspend (true, k, q)
    | mkLat (Capture (_, k)) = Capture (true, k)
*)

  fun procio p =
      let (* val _ = print ("procio on " ^ (Int.toString p) ^ "\n") *)
          val q = Array.sub (ioqueues, p)
          val q' =
              List.foldl
                  (fn ((t, f), r) =>
                      (* if latency t then *)
                          if f () then (resume (t, ()); r)
                          else (t, f)::r
                      (* else raise (Parallel "Invariant violated!\n") *)
                  )
                  []
                  q
      in
          Array.update (ioqueues, p, q')
      end

  fun schedule countSuspends () =
    let
      fun loop (countSuspends, p) =
          let
              val _ = procio p
          in
            case T.atomically (fn () => Q.getWork p)
             of NONE =>
                let in
                  (* if !enabled then (enabled := false; profileDisable ()) else (); *)
                  loop (countSuspends, p)
                end
              | SOME (nonlocal, _,  j) =>
                let
                  val () = if countSuspends andalso nonlocal then incSuspends p else ()
                  (* val () = if not (!enabled) then (enabled := true; profileEnable ()) else (); *)
                  val () = Q.startWork p
                  val () = (case j
                             of Work w => w ()
                              | Thread k => T.switch (fn _ => T.prepare (k, ())))
                      (* PERF? this handle only makes sense for the Work case *)
                      (* PERF? move this handler out to the native entry point? *)
                      handle Parallel s =>
                             (TextIO.output (TextIO.stdErr,
                                             ("WARNING: Caught parallel exception \""
                                                   ^ s
                                                   ^ "\" in parallel scheduler!\n"));
                                   TextIO.flushOut TextIO.stdErr;
                                   OS.Process.exit OS.Process.failure)
                             | e => (TextIO.output (TextIO.stdErr,
                                                  ("WARNING: Caught exception \""
                                                   ^ (General.exnMessage e)
                                                   ^ "\" in parallel scheduler!\n"));
                                   TextIO.flushOut TextIO.stdErr;
                                   OS.Process.exit OS.Process.failure)
                  (* A job should never return -- we will only get here in exceptional
                    conditions. *)
                  (* NB we call processorNumber again here in case that this
                    job has been split across two processors *)
                  val p = processorNumber ()
                  val () = incSuspends p
                  val () = Q.finishWork p
                in
                  loop (false, p)
                end
          end
    in
      loop (countSuspends, processorNumber ())
    end

  fun capture' (p, tail) =
      let (* val _ = print "capture'\n" *) in
      T.switch
          (fn k =>
              (* Note that we cannot call Q.addWork on the current thread!
                Also, we can't call directly schedule here because we need to
                preserve the current thread/stack. Instead we switch to a
                different thread that will continue by calling f and then
                schedule.  This avoids a whole host of bugs related to f
                leaking out k to another thread and that thread resuming k
                before f has finished. *)
              let
                (* Check to see what the next job is.  If it's a thread, then
                  hijack that thread to run the tail of the current job.
                  Otherwise, create a new thread. *)
                val t =
                    case Q.getWork p
                     of SOME (_, _, Work w) => T.new (fn () => (Q.startWork p; w ()))
                      | SOME (_, _, Thread k') =>
                        ((* print "before-prepend\n"; *)
                         T.prepend (k', fn () => (Q.startWork p))
                         (* before print "after-prepend\n" *))
                      | NONE => T.new (schedule false)
                (* to disable hijacking, use this instead
                val t = T.new schedule
                 *)
                fun add (prio, w) = Q.addWork (prio, p, [(Q.newWork p, Work w)])
              in
                (* XX maybe this should move out (before suspend/finishWork) *)
                (* add any delayed work *)
                app add (rev (Array.sub (delayed, p)));
                Array.update (delayed, p, nil);
                (* return the new thread to switch to *)
                (T.prepare (T.prepend (t, tail), (p, k))
                handle e => (print "here 176\n"; raise e))
              end)
      end

  fun suspend f =
      let
        val p = processorNumber ()
        (* val _ = print "suspend\n" *)
        (* val _ = print ("suspend at " ^ (Int.toString p) ^ "\n") *)
        fun tail (p, k) =
            let
              val () = incSuspends p
              val q = Q.suspendWork p
            in
              f (Suspend (currentPrio (), k, q))
            end
      in
        capture' (p, tail)
      end

  fun capture f =
      let
        val p = processorNumber ()
        (* val _ = print "capture\n" *)
        fun tail (p, k) =
            let
              val () = incSuspends p
              val () = Q.finishWork p
            in
              f (Capture (currentPrio (), k))
            end
      in
        capture' (p, tail)
      end

  fun yield () =
      let
        val p = processorNumber ()
      in
        if Q.shouldYield p then
          capture' (p, fn (p, k) =>
                          let in
                              Q.addWork (currentPrio (),
                                         p, [(Q.newWork p, Thread k)]); (* XXX *)
                            incSuspends p;
                            Q.finishWork p
                          end)
        else
          ()
      end

  fun event f =
      f ()
      (* raise (Parallel "not supported") *)

  fun addRightPrio (prio, w) =
      let
        val p = processorNumber ()
        val t = Q.newWork p
      in
        if Q.shouldYield p then
          (* Switch to a new thread *)
          capture' (p, fn (p, k) =>
                         let in
                           (* Add the continuation first -- it is higher priority *)
                           Q.addWork (prio, p, [(Q.newWork p, Thread (T.prepend (k, fn () => t))),
                                          (t, Work w)]);
                           incSuspends p;
                           Q.finishWork p
                         end)
        else
          let
            fun add (prio, w) = Q.addWork (prio, p, [(Q.newWork p, Work w)])
          in
            (* add any delayed work *)
            (* XXX maybe should run delayed work and queue the currrent thread too? *)
            app add (rev (Array.sub (delayed, p)));
            Array.update (delayed, p, nil);
            Q.addWork (prio, p, [(t, Work w)]);
            t
          end
      end

  fun addRight w = addRightPrio (Priority.bot, w)

  (* smuller: XXX This adds the current thread to the lqueue and so is totally
     wrong. Fix it. *)
  fun addLeftPrio (prio, w) =
      let
        val p = processorNumber ()
        val t = Q.newWork p
        val _ = print "addLeftPrio\n"
      in
        if Q.shouldYield p then
          capture' (p, fn (p, k) =>
                         let in
                           Q.addWork (prio, p, [(Q.newWork p, Work w),
                                          (t, Thread (T.prepend (k, fn () => t)))]);
                           incSuspends p;
                           Q.finishWork p
                         end)
        else
          T.switch (fn k =>
                       T.prepare
                       (T.new (fn () =>
                                  let
                                    fun add (prio, w) = Q.addWork (prio, p, [(Q.newWork p, Work w)])
                                  in
                                    (* add any delayed work *)
                                    (* XXX maybe should run delayed work and queue the currrent thread too? *)
                                    app add (rev (Array.sub (delayed, p)));
                                    Array.update (delayed, p, nil);
                                    Q.addWork (prio, p, [(t, Thread (T.prepend (k, fn () => t)))]);
                                    w ()
                                  end), ()))
      end

  fun addLeft w = addLeftPrio (Priority.bot, w)

  fun removePrio prio t = Q.removeWorkPrio (prio, processorNumber (), t)
  fun remove t = Q.removeWork (processorNumber (), t)

(* XXX left? what about the right? *)
  fun delayedAddPrio (prio, w) =
      let
        (* PERF use a array-based buffer to avoid allocation *)
        val p = processorNumber ()
        val ws = Array.sub (delayed, p)
      in
        Array.update (delayed, p, (prio, w)::ws)
      end

  fun delayedAdd w = delayedAddPrio (Priority.bot, w)

  fun return () =
      let
        val p = processorNumber ()
      in
        (* Look for delayed work *)
        case Array.sub (delayed, p)
         of nil => ((* this is counted in schedule: incSuspends p;  *)
                    Q.finishWork p;
                    schedule true ())
          | ws =>
            let
              val ((prio, w), ws) =
                  case rev ws of (prio, w)::ws => ((prio, w), ws) | nil => raise Match
              val () = Array.update (delayed, p, nil)
              fun add nil = ()
                | add ((prio, w)::ws) =
                  Q.addWork (prio, p, [(Q.newWork p, Work (fn () => (add ws; w ())))])
              (* add any lower priority work *)
              val () = add ws
            in
              (* now what do to with w? *)
              if Q.shouldYield p then
                (Q.addWork (currentPrio (), p, [(Q.newWork p, Work w)]); (* XXX *)
                 (* this is counted in schedule: incSuspends p; *)
                 Q.finishWork p;
                 schedule true ())
              else
                w ()
            end
      end


  fun prun () = schedule false ()

  fun init () =
      (Q.initQueues ();
       MLton.Parallel.registerProcessorFunction (prun);
       MLton.Parallel.initializeProcessors ())

  fun initPriorities () = Priority.init ()
  fun finalizePriorities () = Priority.check ()

  val policyName = Q.policyName
  val maxBytesLive = _import "Parallel_maxBytesLive" runtime private: unit -> Word64.word;
  val gcTime = _import "Parallel_getTimeInGC" runtime private: unit -> Word64.word;
  val successfulSteals = Q.reportSuccessfulSteals
  val failedSteals = Q.reportFailedSteals
  fun resetStatistics () =
      let
        val resetBytesLive = _import "Parallel_resetBytesLive" runtime private: unit -> unit;
      in
        Q.resetSteals ();
        Array.modify (fn _ => 0) suspends;
        resetBytesLive ()
      end
  val suspends = fn () => Array.foldl op+ 0 suspends

end

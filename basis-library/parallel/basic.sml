structure MLtonParallelBasic :> MLTON_PARALLEL_BASIC =
struct

  type void = unit
  type work = unit -> void

  structure HH = MLtonHM.HierarchicalHeap
  structure I = MLtonParallelInternal
  structure T = MLtonThread

  datatype job = Work of (unit -> void) * HH.t * int
               | Thread of unit T.t * HH.t

  val numberOfProcessors = I.numberOfProcessors

  structure Q = WorkQueue (struct
                             type work = job
                             val numberOfProcessors = fn () => numberOfProcessors
                           end)
    :> PARALLEL_WORKQUEUE where type work = job

  datatype 'a t = Suspend of 'a T.t * HH.t * Q.susp
                | Capture of 'a T.t * HH.t

  type token = Q.token

  val processorNumber = I.processorNumber
  val profileDisable = _import "GC_profileDisable" runtime private: unit -> unit;
  val profileEnable = _import "GC_profileEnable" runtime private: unit -> unit;

  exception Parallel of string

  val suspends = Array.array (numberOfProcessors, 0)
  fun incSuspends p = Array.update (suspends, p, Array.sub (suspends, p) + 1)

  val delayed = Array.array (numberOfProcessors, nil)

  val enabled = ref true

  fun evaluateInGlobalHeap (f: 'a -> 'b): 'a -> 'b =
      fn argument =>
         let
             val _ = I.enterGlobalHeap ()
             val result = f argument
             val _ = I.exitGlobalHeap ()
         in
             result
         end

  fun die message =
      (TextIO.output (TextIO.stdErr, message);
       TextIO.flushOut TextIO.stdErr;
       MLtonProcess.exit MLtonProcess.Status.failure)

  (* RAM_NOTE: Remove once futures are reintegrated *)
  fun checkDelayedEmpty p =
      if length (Array.sub (delayed, p)) > 0 then
          die "basic.sml: capture': ERROR: Delayed work exists?\n"
      else
          ()

  fun makeHHSetter hhOption = fn () => case hhOption
                                        of SOME(hh) => (I.enterGlobalHeap ();
                                                        HH.set hh;
                                                        HH.useHierarchicalHeap ();
                                                        I.exitGlobalHeap ())
                                         | NONE => (I.enterGlobalHeap ();
                                                    HH.useHierarchicalHeap ();
                                                    I.exitGlobalHeap ())

  fun schedule countSuspends () =
      let
          fun loop (countSuspends, p) =
              let in
                  case Q.getWork p
                   of NONE =>
                      let in
                          (* if !enabled then (enabled := false; profileDisable ()) else (); *)
                          loop (countSuspends, p)
                      end
                    | SOME (nonlocal, j) =>
                      let
                          val () = if countSuspends andalso nonlocal then incSuspends p else ()
                          (* val () = if not (!enabled) then (enabled := true; profileEnable ()) else (); *)
                          val () = Q.startWork p
                          val () = (case j
                                     of Work (w, parentHH, level) =>
                                        let
                                            (* I *must* find a stolen work in forkjoin-only! *)
                                            val _ = if nonlocal
                                                    then ()
                                                    else die ("MLton.Parallel.Basic.schedule: " ^
                                                              "ERROR: work exists on queue?!")

                                            val childHH = HH.new ()
                                            val () = HH.setLevel (childHH, level)
                                            val () = HH.appendChild (parentHH,
                                                                     childHH)
                                        in
                                            makeHHSetter (SOME childHH) ();
                                            w ()
                                        end

                                      | Thread (k, hh) =>
                                        (*
                                         * This doesn't have to be stolen as it
                                         * could be a resume parent
                                         *)
                                        T.switch
                                            (fn _ =>
                                                T.prepare
                                                    (T.prepend
                                                         (k, makeHHSetter
                                                                 (SOME hh)),
                                                     ()))

                                        (* PERF? this handle only makes sense for the Work case *)
                                        (* PERF? move this handler out to the native entry point? *)
                                        handle e => (I.enterGlobalHeap ();
                                                     die ("MLton.Parallel.Basic.schedule: WARNING: Caught exception \""
                                                          ^ (General.exnMessage e)
                                                          ^ "\" in parallel scheduler!\n")))
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

  (* RAM_NOTE: This is the older version that includes support for futures *)
  (* fun schedule countSuspends () = *)
  (*   let *)
  (*     fun loop (countSuspends, p) = *)
  (*         let in *)
  (*           case Q.getWork p *)
  (*            of NONE => *)
  (*               let in *)
  (*                 (* if !enabled then (enabled := false; profileDisable ()) else (); *) *)
  (*                 loop (countSuspends, p) *)
  (*               end *)
  (*             | SOME (nonlocal, j) => *)
  (*               let *)
  (*                 val () = if countSuspends andalso nonlocal then incSuspends p else () *)
  (*                 (* val () = if not (!enabled) then (enabled := true; profileEnable ()) else (); *) *)
  (*                 val () = Q.startWork p *)
  (*                 val () = (case j *)
  (*                            of Work (w, parentHH, level) => *)
  (*                               if nonlocal *)
  (*                               then *)
  (*                                   let *)
  (*                                       val childHH = HH.new () *)
  (*                                       val () = HH.setLevel (childHH, level) *)
  (*                                       val () = *)
  (*                                           HH.appendChild (parentHH, childHH) *)
  (*                                   in *)
  (*                                       makeHHSetter (SOME childHH) (); *)
  (*                                       w () *)
  (*                                   end *)
  (*                               else *)
  (*                                   (makeHHSetter NONE (); *)
  (*                                    w ()) *)
  (*                             | Thread (k, hh) => *)
  (*                               T.switch (fn _ => *)
  (*                                            T.prepare *)
  (*                                                (T.prepend *)
  (*                                                     (k, makeHHSetter (SOME hh)), *)
  (*                                                 ())) *)

  (*                               (* PERF? this handle only makes sense for the Work case *) *)
  (*                               (* PERF? move this handler out to the native entry point? *) *)
  (*                               handle e => (I.enterGlobalHeap (); *)
  (*                                            die ("MLton.Parallel.Basic.schedule: WARNING: Caught exception \"" *)
  (*                                                 ^ (General.exnMessage e) *)
  (*                                                 ^ "\" in parallel scheduler!\n"))) *)
  (*                 (* A job should never return -- we will only get here in exceptional *)
  (*                   conditions. *) *)
  (*                 (* NB we call processorNumber again here in case that this *)
  (*                   job has been split across two processors *) *)
  (*                 val p = processorNumber () *)
  (*                 val () = incSuspends p *)
  (*                 val () = Q.finishWork p *)
  (*               in *)
  (*                 loop (false, p) *)
  (*               end *)
  (*         end *)
  (*   in *)
  (*       loop (countSuspends, processorNumber ()) *)
  (*   end *)

  fun capture' (p, tail) =
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
                      case Q.getWork p of
                          SOME (nonlocal, Work (w, parentHH, level)) =>
                          let
                              val _ = if nonlocal
                                      then ()
                                      else die ("MLton.Parallel.Basic.schedule: " ^
                                                "ERROR: work exists on queue?!")

                              val childHH = HH.new ()
                              val () = HH.setLevel (childHH, level)
                              val () = HH.appendChild (parentHH, childHH)
                          in
                              T.new (fn () =>
                                        (Q.startWork p;
                                         makeHHSetter (SOME childHH) ();
                                         w ()
                                        (* RAM_NOTE: Do I need to switch heaps on exception here? *)
                                        ))
                          end

                        | SOME (_, Thread (k, hh)) =>
                          T.prepend (k, fn _ => (makeHHSetter (SOME hh) ();
                                                 Q.startWork p))

                        | NONE => T.new (fn () => schedule false ())
                  (* to disable hijacking, use this instead
                val t = T.new schedule
                   *)
                  val currentHH = HH.get ()
                  (* fun add w = Q.addWork (p, *)
                  (*                        [(Q.newWork p, Work (w, currentHH))]) *)
              in
                  checkDelayedEmpty p;
                  (* RAM_NOTE: Disabled until reintegrated *)
                  (* (* XX maybe this should move out (before suspend/finishWork) *) *)
                  (* (* add any delayed work *) *)
                  (* app add (rev (Array.sub (delayed, p))); *)
                  (* Array.update (delayed, p, nil); *)
                  (* return the new thread to switch to *)
                  T.prepare (T.prepend (t, tail), (p, k))
              end)

  (* Original capture' *)
  (* fun capture' (p, tail) = *)
  (*     T.switch *)
  (*         (fn k => *)
  (*             (* Note that we cannot call Q.addWork on the current thread! *)
  (*               Also, we can't call directly schedule here because we need to *)
  (*               preserve the current thread/stack. Instead we switch to a *)
  (*               different thread that will continue by calling f and then *)
  (*               schedule.  This avoids a whole host of bugs related to f *)
  (*               leaking out k to another thread and that thread resuming k *)
  (*               before f has finished. *) *)
  (*             let *)
  (*               (* Check to see what the next job is.  If it's a thread, then *)
  (*                 hijack that thread to run the tail of the current job. *)
  (*                 Otherwise, create a new thread. *) *)
  (*               val t = *)
  (*                   case Q.getWork p of *)
  (*                       SOME (nonlocal, Work (w, parentHH, level)) => *)
  (*                       if nonlocal *)
  (*                       then *)
  (*                           let *)
  (*                               val childHH = HH.new () *)
  (*                               val () = HH.setLevel (childHH, level) *)
  (*                               val () = HH.appendChild (parentHH, childHH) *)
  (*                           in *)
  (*                               T.new (fn () => *)
  (*                                         (Q.startWork p; *)
  (*                                          makeHHSetter (SOME childHH) (); *)
  (*                                          w () *)
  (*                                         (* RAM_NOTE: Do I need to switch heaps on exception here? *) *)
  (*                                         )) *)
  (*                           end *)
  (*                       else *)
  (*                           T.new (fn () => *)
  (*                                     (Q.startWork p; *)
  (*                                      makeHHSetter NONE (); *)
  (*                                      w () *)
  (*                                     (* RAM_NOTE: Do I need to switch heaps on exception here? *) *)
  (*                                     )) *)
  (*                     | SOME (_, Thread (k, hh)) => *)
  (*                       T.prepend (k, fn _ => (makeHHSetter (SOME hh) (); Q.startWork p)) *)
  (*                     | NONE => T.new (fn () => schedule false ()) *)
  (*               (* to disable hijacking, use this instead *)
  (*               val t = T.new schedule *)
  (*                *) *)
  (*               val currentHH = HH.get () *)
  (*               fun add w = Q.addWork (p, [(Q.newWork p, Work (w, currentHH))]) *)
  (*             in *)
  (*                 checkDelayedEmpty p; *)
  (*                 (* RAM_NOTE: Disabled until reintegrated *) *)
  (*                 (* (* XX maybe this should move out (before suspend/finishWork) *) *) *)
  (*                 (* (* add any delayed work *) *) *)
  (*                 (* app add (rev (Array.sub (delayed, p))); *) *)
  (*                 (* Array.update (delayed, p, nil); *) *)
  (*                 (* return the new thread to switch to *) *)
  (*                 T.prepare (T.prepend (t, tail), (p, k)) *)
  (*             end) *)

  fun suspend (f: 'a t -> unit) =
      let
          val _ = I.enterGlobalHeap ()

          val p = processorNumber ()
          val hh = HH.get ()
          fun tail (p, k) =
              let
                  val () = incSuspends p
                  val q = Q.suspendWork p
              in
                f (Suspend (k, hh, q))
              end

          val result = capture' (p, tail)

          val _ = I.exitGlobalHeap ()
      in
          result
      end

  fun capture (f: 'a t -> unit) =
      let
          val _ = I.enterGlobalHeap ()

          val p = processorNumber ()
          val hh = HH.get ()
          fun tail (p, k) =
              let
                  val () = incSuspends p
                  val () = Q.finishWork p
              in
                f (Capture (k, hh))
              end
          val result = capture' (p, tail)

          val _  = I.exitGlobalHeap ()
      in
          result
      end

  fun resume suspension =
      let
          val _ = I.enterGlobalHeap ()

          val result =
              case suspension of
                  (Suspend (k, hh, q), v) =>
                  let
                      val p = processorNumber ()
                  in
                      Q.resumeWork (p,
                                    q,
                                    (Q.newWork p,
                                     Thread (T.prepend (k, fn () => v), hh)))
                  end
                | (Capture (k, hh), v) =>
                  let
                      val p = processorNumber ()
                  in
                      Q.addWork (p, [(Q.newWork p,
                                      Thread (T.prepend (k, fn () => v), hh))])
                  end

          val _ = I.exitGlobalHeap ()
      in
          result
      end

  val yield =
      evaluateInGlobalHeap
          (fn () =>
              let
                  val p = processorNumber ()
              in
                  if Q.shouldYield p then
                      let
                          val hh = HH.get ()
                          val () =
                              capture' (p, fn (p, k) =>
                                              (Q.addWork (p,
                                                          [(Q.newWork p,
                                                            Thread (k, hh))]);
                                               incSuspends p;
                                               Q.finishWork p))
                      in
                          ()
                      end
                  else
                      ()
              end)

  local
      fun doAddRight (w, level) =
          let
              val p = processorNumber ()
              val t = Q.newWork p
              val currentHH = HH.get ()
          in
              if Q.shouldYield p
              then
                  (* Switch to a new thread *)
                  let
                      val t =
                          capture' (p, fn (p, k) =>
                                          let in
                                              (* Add the continuation first -- it is higher priority *)
                                              Q.addWork (p,
                                                         [(Q.newWork p,
                                                           Thread (T.prepend (k, fn () => t),
                                                                   currentHH)),
                                                          (t,
                                                           Work (w, currentHH, level))]);
                                              incSuspends p;
                                              Q.finishWork p
                                          end)
                  in
                      t
                  end
              else
                  let
                      (* fun add w = Q.addWork (p, [(Q.newWork p, *)
                      (*                             Work (w, currentHH))]) *)
                  in
                      checkDelayedEmpty p;
                      (* RAM_NOTE: Disabled until reintegrated *)
                      (* (* add any delayed work *) *)
                      (* (* XXX maybe should run delayed work and queue the currrent thread too? *) *)
                      (* app add (rev (Array.sub (delayed, p))); *)
                      (* Array.update (delayed, p, nil); *)
                      Q.addWork (p, [(t, Work (w, currentHH, level))]);
                      t
                  end
          end
  in
      val addRight = evaluateInGlobalHeap doAddRight
  end

  (* RAM_NOTE: Disabled until reintegrated *)
  (* local *)
  (*     fun doAddLeft w = *)
  (*         let *)
  (*             val p = processorNumber () *)
  (*             val t = Q.newWork p *)
  (*         in *)
  (*             if Q.shouldYield p then *)
  (*                 capture' (p, fn (p, k) => *)
  (*                                 let in *)
  (*                                     Q.addWork (p, [(Q.newWork p, Work w), *)
  (*                                                    (t, Thread (T.prepend (k, fn () => t)))]); *)
  (*                                     incSuspends p; *)
  (*                                     Q.finishWork p *)
  (*                                 end) *)
  (*             else *)
  (*                 T.switch (fn k => *)
  (*                              T.prepare *)
  (*                                  (T.new (fn () => *)
  (*                                             let *)
  (*                                                 fun add w = Q.addWork (p, [(Q.newWork p, Work w)]) *)
  (*                                             in *)
  (*                                                 (* add any delayed work *) *)
  (*                                                 (* XXX maybe should run delayed work and queue the currrent thread too? *) *)
  (*                                                 app add (rev (Array.sub (delayed, p))); *)
  (*                                                 Array.update (delayed, p, nil); *)
  (*                                                 Q.addWork (p, [(t, Thread (T.prepend (k, fn () => t)))]); *)
  (*                                                 w () *)
  (*                                             end), ())) *)
  (*         end *)
  (* in *)
  (*     val addLeft = evaluateInGlobalHeap doAddLeft *)
  (* end *)

  val remove =
      evaluateInGlobalHeap (fn t => Q.removeWork (processorNumber (), t))

  (* XXX left? what about the right? *)
  val delayedAdd =
      evaluateInGlobalHeap
          (fn w =>
              let
                  val _ = die "basic.sml: delayedAdd: FATAL: function called!\n"
                  (* PERF use a array-based buffer to avoid allocation *)
                  val p = processorNumber ()
                  val ws = Array.sub (delayed, p)
              in
                  Array.update (delayed, p, w::ws)
              end)

  val return =
      evaluateInGlobalHeap
          (fn () =>
              let
                  val p = processorNumber ()
              in
                  (* Look for delayed work *)
                  case Array.sub (delayed, p) of
                      nil =>
                      (* this is counted in schedule: incSuspends p;  *)
                      (Q.finishWork p;
                       T.switch (fn _ => T.prepare (T.new (schedule true), ())))
                    | ws =>
                      (checkDelayedEmpty p;
                       die "basic.sml: return: FATAL: Delayed work exists!\n")
                       (* RAM_NOTE: Disabled until reintegrated *)
                       (* let *)
                       (*     val (w, ws) = case rev ws of w::ws => (w, ws) | nil => raise Match *)
                       (*     val () = Array.update (delayed, p, nil) *)
                       (*     fun add nil = () *)
                       (*       | add (w::ws) = *)
                       (*         Q.addWork (p, [(Q.newWork p, Work (fn () => (add ws; w ())))]) *)
                       (*     (* add any lower priority work *) *)
                       (*     val () = add ws *)
                       (* in *)
                       (*     (* now what do to with w? *) *)
                       (*     if Q.shouldYield p then *)
                       (*         (Q.addWork (p, [(Q.newWork p, Work w)]); *)
                       (*          (* this is counted in schedule: incSuspends p; *) *)
                       (*          Q.finishWork p; *)
                       (*          schedule true ()) *)
                       (*     else *)
                       (*         (I.exitGlobalHeap (); *)
                       (*          w ()) *)
                       (* end *)
              end)


  val () = (_export "Parallel_run": (unit -> void) -> unit;) (fn () => schedule false ())
  (* init MUST come after schedulerLoop has been exported *)

  val () = print ("**************************\n" ^
                  "Parallel_init happens after WSQ array init. Should happen first!\n" ^
                  "**************************\n")

  val () = (_import "Parallel_init" runtime private: unit -> unit;) ()

  val policyName = Q.policyName
  val maxBytesLive =
      evaluateInGlobalHeap (_import "Parallel_maxBytesLive" runtime private: unit -> Word64.word;)
  val gcTime =
      evaluateInGlobalHeap (_import "Parallel_getTimeInGC" runtime private: unit -> Word64.word;)
  val successfulSteals = evaluateInGlobalHeap Q.reportSuccessfulSteals
  val failedSteals = evaluateInGlobalHeap Q.reportFailedSteals
  val resetStatistics =
      evaluateInGlobalHeap
          (fn () =>
              let
                  val resetBytesLive = _import "Parallel_resetBytesLive" runtime private: unit -> unit;
              in
                  Q.resetSteals ();
                  Array.modify (fn _ => 0) suspends;
                  resetBytesLive ()
              end)
  val suspends = evaluateInGlobalHeap (fn () => Array.foldl op+ 0 suspends)

end

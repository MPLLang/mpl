structure MLtonParallelBasic :> MLTON_PARALLEL_BASIC =
struct

  type void = unit
  type work = unit -> void

  structure HM = MLtonHM
  structure HH = HM.HierarchicalHeap
  structure I = MLtonParallelInternal
  structure T = MLtonThread

  datatype job = Work of (unit -> void) * unit HH.t * int
               | Thread of unit T.t * unit HH.t

  val numberOfProcessors = Word32.toInt I.numberOfProcessors

  structure Q = WorkQueue (struct
                             type work = job
                             val numberOfProcessors = fn () => numberOfProcessors
                           end)
    :> PARALLEL_WORKQUEUE where type work = job

  datatype 'a t = Suspend of 'a T.t * Q.susp
                | Capture of 'a T.t * unit HH.t

  type token = Q.token

  val processorNumber = Word32.toInt o I.processorNumber
  val profileDisable = _import "GC_profileDisable" runtime private: unit -> unit;
  val profileEnable = _import "GC_profileEnable" runtime private: unit -> unit;

  exception Parallel of string

  val suspends = Array.array (numberOfProcessors, 0)
  fun incSuspends p = Array.update (suspends, p, Array.sub (suspends, p) + 1)

  val schedThreads = Array.array (numberOfProcessors, NONE)

  (* RAM_NOTE: Restore futures are reintegrated *)
  (* val delayed = Array.array (numberOfProcessors, nil) *)

  val enabled = ref true

  fun evaluateInGlobalHeap (f: 'a -> 'b): 'a -> 'b =
      fn argument =>
         let
             val _ = HM.enterGlobalHeap ()
             val result = f argument
             val _ = HM.exitGlobalHeap ()
         in
             result
         end

  fun die message =
      (TextIO.output (TextIO.stdErr, message);
       TextIO.flushOut TextIO.stdErr;
       MLtonProcess.exit MLtonProcess.Status.failure)

  fun dbgmsg m =
      let
          val p = Word32.toInt (I.processorNumber ())
      in
          print (String.concat ["[",
                                Int.toString p,
                                "] ",
                                m,
                                "\n"])
      end

  (* Comment out to enable debug messages *)
  fun dbgmsg m = ()

  fun useHH (hh : unit HH.t) : unit = (HM.enterGlobalHeap ();
                                       HH.set hh;
                                       HH.setUseHierarchicalHeap true;
                                       HM.exitGlobalHeap ())

  fun stopUseHH (() : unit) : unit = (HM.enterGlobalHeap ();
                                      HH.setUseHierarchicalHeap false;
                                      HM.exitGlobalHeap ())

  (*
   * RAM_NOTE: MUST be called by a thread that has inGlobalHeap = 0 and useHH =
   * FALSE
   *)
  fun schedule' p countSuspends =
      case Q.getWork p
       of NONE =>
          (* if !enabled then (enabled := false; profileDisable ()) else (); *)
          schedule' p countSuspends

        | SOME (nonlocal, unlocker, j) =>
          (if countSuspends andalso nonlocal
           then incSuspends p
           else ();
n
           (* val () = if not (!enabled) then (enabled := true; profileEnable ()) else (); *)

           dbgmsg "stole work";

           Q.startWork p;

           (case j
             of Work (w, parentHH, sharedLevel) =>
                let
                    (*
                     * RAM_NOTE: I *must* find a stolen work in forkjoin-only!
                     *)
                    val () = if nonlocal
                             then ()
                             else die ("MLton.Parallel.Basic.schedule: " ^
                                       "ERROR: work exists on queue?!")

                    val childHH = HH.new ()
                    val () = HH.appendChild (parentHH, childHH, sharedLevel)
                in
                    (* switch to childHH to allocate the thread there *)
                    T.switch (fn st : unit T.t =>
                                 (Array.update (schedThreads,
                                                p,
                                                SOME (T.prepare (st, ())));
                                  useHH childHH;
                                  T.prepare (T.new (fn () =>
                                                       (*
                                                        * make sure to use
                                                        * the HH in the new
                                                        * thread too.
                                                        *)
                                                       (useHH childHH;
                                                        unlocker ();
                                                        w ())),
                                             ())));
                    stopUseHH ()
                end

              | Thread (k, hh) =>
                (*
                 * This doesn't have to be stolen as it
                 * could be a resume parent
                 *)
                (T.switch (fn st =>
                              (Array.update (schedThreads,
                                             p,
                                             SOME (T.prepare (st, ())));
                               useHH hh;
                               T.prepare (T.prepend (k, fn () => unlocker ()), ())));
                 stopUseHH ()))
           (* PERF? this handle only makes sense for the Work case *)
           (* PERF? move this handler out to the native entry point? *)
           handle e => (HM.enterGlobalHeap ();
                        die ("MLton.Parallel.Basic.schedule: WARNING: Caught exception \""
                             ^ (General.exnMessage e)
                             ^ "\" in parallel scheduler!\n"));

           schedule' p countSuspends)

  fun schedule () =
      let
          val p = processorNumber ()
      in
          schedule' p false
      end

  val () =
      let
          fun loop i n =
              if i >= n
              then ()
              else Array.update (schedThreads,
                                 i,
                                 SOME (T.initPrimitive (T.new schedule)))
      in
          loop 0 numberOfProcessors
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
  (*                               handle e => (HM.enterGlobalHeap (); *)
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
      let
          val r =
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
                          (* val t = *)
                          (*     case Q.getWork p *)
                          (*      of SOME (nonlocal, Work (w, parentHH, sharedLevel)) => *)
                          (*         let *)
                          (*             val _ = if nonlocal *)
                          (*                     then () *)
                          (*                     else die ("MLton.Parallel.Basic.schedule: " ^ *)
                          (*                               "ERROR: work exists on queue?!") *)

                          (*             val childHH = HH.new () *)
                          (*         in *)
                          (*             HH.appendChild (parentHH, childHH, sharedLevel); *)
                          (*             T.new (fn () => *)
                          (*                       (Q.startWork p; *)
                          (*                        setAndUseHH childHH; *)
                          (*                        w () *)
                          (*                       (* RAM_NOTE: Do I need to switch heaps on exception here? *) *)
                          (*                       )) *)
                          (*         end *)

                          (*       | SOME (_, Thread (k, hh)) => k *)

                          (*       | NONE => T.new (schedule false) *)
                          (* to disable hijacking, use this instead
                val t = T.new schedule
                           *)
                          (* val currentHH = HH.get () *)
                          (* fun add w = Q.addWork (p, *)
                          (*                        [(Q.newWork p, Work (w, currentHH))]) *)

                          val () = tail (p, k)

                          val () = HM.enterGlobalHeap ()
                          val t = valOf (Array.sub (schedThreads, p))
                      in
                          (* RAM_NOTE: Disabled until reintegrated *)
                          (* (* XX maybe this should move out (before suspend/finishWork) *) *)
                          (* (* add any delayed work *) *)
                          (* app add (rev (Array.sub (delayed, p))); *)
                          (* Array.update (delayed, p, nil); *)
                          (* return the new thread to switch to *)
                          t
                      end)
          val () = HM.exitGlobalHeap ()
      in
          r
      end

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
      die "Basic.suspend shouldn't be called!"
      (* let *)
      (*     val _ = HM.enterGlobalHeap () *)

      (*     val p = processorNumber () *)
      (*     val hh = HH.get () *)
      (*     fun tail (p, k) = *)
      (*         let *)
      (*             val () = incSuspends p *)
      (*             val q = Q.suspendWork p *)
      (*         in *)
      (*           f (Suspend (k, hh, q)) *)
      (*         end *)

      (*     val result = capture' (p, tail) *)

      (*     val _ = HM.exitGlobalHeap () *)
      (* in *)
      (*     result *)
      (* end *)

  fun capture (f: 'a t -> unit) =
      let
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
      in
          result
      end

  fun resume suspension =
      case suspension of
          (Suspend (k, q), v) =>
          die "MLton.Parallel.Basic.resume: Suspend objects shouldn't exist!"
        (* let *)
        (*     val p = processorNumber () *)
        (* in *)
        (*     Q.resumeWork (p, *)
        (*                   q, *)
        (*                   (Q.newWork p, *)
        (*                    Thread (T.prepend (k, fn () => v), hh), *)
        (*                    NONE)) *)
        (* end *)
        | (Capture (k, hh), v) =>
          let
              val p = processorNumber ()
              val myHH = HH.get ()
              val t =
                  let
                      val () = useHH hh
                      val t = Thread (T.prepend (k, fn () => v), hh)
                      val () = useHH myHH
                  in
                      t
                  end
          in
              Q.addWork (p, [(Q.newWork p, t)])
          end

  val yield =
      die "MLton.Parallel.Basic.yield: yield is not supported!"
      (* let *)
      (*     val p = processorNumber () *)
      (* in *)
      (*     if Q.shouldYield p *)
      (*     then *)
      (*         let *)
      (*             val hh = HH.get () *)
      (*         in *)
      (*             capture' (p, *)
      (*                       fn (p, k) => *)
      (*                          (Q.addWork (p, [(Q.newWork p, *)
      (*                                           Thread k)]); *)
      (*                           incSuspends p; *)
      (*                           Q.finishWork p)) *)
      (*         end *)
      (*     else *)
      (*         () *)
      (* end *)

  local
      fun doAddRight (w, level) =
          let
              val p = processorNumber ()
              val t = Q.newWork p
              val currentHH = HH.get ()
          in
              if Q.shouldYield p
              then
                  die ("MLton.Parallel.Basic.doAddRight: yielding addRight " ^
                       "unimplemented!")
                  (* (* Switch to a new thread *) *)
                  (* let *)
                  (*     val t = *)
                  (*         capture' (p, fn (p, k) => *)
                  (*                         let in *)
                  (*                             (* Add the continuation first -- it is higher priority *) *)
                  (*                             Q.addWork (p, *)
                  (*                                        [(Q.newWork p, *)
                  (*                                          Thread (T.prepend (k, fn () => t)), *)
                  (*                                          (currentHH, NONE)), *)
                  (*                                         (t, *)
                  (*                                          Work w, *)
                  (*                                          (currentHH, *)
                  (*                                           SOME level))]); *)
                  (*                             incSuspends p; *)
                  (*                             Q.finishWork p *)
                  (*                         end) *)
                  (* in *)
                  (*     t *)
                  (* end *)
              else
                  let
                      (* fun add w = Q.addWork (p, [(Q.newWork p, *)
                      (*                             Work (w, currentHH))]) *)
                  in
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
      val addRight = doAddRight
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

  fun remove t = Q.removeWork (processorNumber (), t)

  (* XXX left? what about the right? *)
  val delayedAdd =
      die "MLton.Parallel.Basic.delayedAdd: unimplemented!"
      (* evaluateInGlobalHeap *)
      (*     (fn w => *)
      (*         let *)
      (*             (* PERF use a array-based buffer to avoid allocation *) *)
      (*             val p = processorNumber () *)
      (*             val ws = Array.sub (delayed, p) *)
      (*         in *)
      (*             Array.update (delayed, p, w::ws) *)
      (*         end) *)

  fun return (() : unit) : void =
      let
          val p = processorNumber ()
      in
          Q.finishWork p;
          T.switch (fn _ =>
                       let
                           val () = HM.enterGlobalHeap ()
                           val t = valOf (Array.sub (schedThreads, p))
                       in
                           t
                       end)
          (* (* Look for delayed work *) *)
          (* case Array.sub (delayed, p) of *)
          (*     nil => *)
          (*     (* this is counted in schedule: incSuspends p;  *) *)
          (*     () *)
          (*   | ws => *)
          (*     let *)
          (*         val (w, ws) = case rev ws of w::ws => (w, ws) | nil => raise Match *)
          (*         val () = Array.update (delayed, p, nil) *)
          (*         fun add nil = () *)
          (*           | add (w::ws) = *)
          (*             Q.addWork (p, [(Q.newWork p, Work (fn () => (add ws; w ())))]) *)
          (*         (* add any lower priority work *) *)
          (*         val () = add ws *)
          (*     in *)
          (*         (* now what do to with w? *) *)
          (*         if Q.shouldYield p then *)
          (*             (Q.addWork (p, [(Q.newWork p, Work w)]); *)
          (*              (* this is counted in schedule: incSuspends p; *) *)
          (*              Q.finishWork p; *)
          (*              schedule true ()) *)
          (*         else *)
          (*             (HM.exitGlobalHeap (); *)
          (*              w ()) *)
          (*     end *)
      end

  val () = (_export "Parallel_run": (unit -> void) -> unit;) schedule
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

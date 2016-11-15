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

  val dbgmsg = (* I.dbgmsg *) fn _ => ()

  val numberOfProcessors = Word32.toInt I.numberOfProcessors

  structure Q = WorkQueue (struct
                             type work = job
                             val numberOfProcessors = fn () => numberOfProcessors
                           end)
    :> PARALLEL_WORKQUEUE where type work = job

  type token = Q.token

  datatype t = Capture of token * job

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

  fun useHH (hh : unit HH.t) : unit = (HM.enterGlobalHeap ();
                                       HH.set hh;
                                       HH.setUseHierarchicalHeap true;
                                       HM.exitGlobalHeap ();
                                       dbgmsg "useHH")

  fun stopUseHH (() : unit) : unit = (HM.enterGlobalHeap ();
                                      HH.setUseHierarchicalHeap false;
                                      HM.exitGlobalHeap ();
                                      dbgmsg "stopUseHH")

  (*
   * RAM_NOTE: MUST be called by a thread that has inGlobalHeap = 0 and useHH =
   * FALSE
   *)
  fun schedule' p countSuspends =
      case Q.getWork p
       of NONE =>
          schedule' p countSuspends

        | SOME (nonlocal, unlocker, j) =>
          (dbgmsg "stole work";

           if countSuspends andalso nonlocal
           then incSuspends p
           else ();

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
                (dbgmsg "resumed thread";
                 T.switch (fn st =>
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

  fun capture' (p, tail) =
      let
        val r =
            T.switch
                (fn k =>
                    let
                      (* Save the full work object here *)
                      val () = tail (p, (Q.newWork (), Thread (k, HH.get ())))

                      val () = HM.enterGlobalHeap ()
                      val t = valOf (Array.sub (schedThreads, p))
                    in
                      t
                    end)
          val () = HM.exitGlobalHeap ()
      in
          r
      end

  fun capture (f: t -> unit): unit =
      let
          val p = processorNumber ()
          fun tail (p, w) =
              let
                  val () = incSuspends p
              in
                f (Capture w)
              end
          val result = capture' (p, tail)
      in
          result
      end

  fun resume (Capture w: t): unit =
      let
        val p = processorNumber ()
      in
        Q.addWork (p, [w])
      end

  fun addRight (w, level) =
          let
              val p = processorNumber ()
              val t = Q.newWork ()
              val currentHH = HH.get ()
          in
            Q.addWork (p, [(t, Work (w, currentHH, level))]);
            t
          end

  fun remove t = Q.removeWork (processorNumber (), t)

  fun return (() : unit) : void =
      let
          val p = processorNumber ()
      in
          T.switch (fn _ =>
                       let
                           val () = HM.enterGlobalHeap ()
                           val t = valOf (Array.sub (schedThreads, p))
                       in
                           t
                       end)
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

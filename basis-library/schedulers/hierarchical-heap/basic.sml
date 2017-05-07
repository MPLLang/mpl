structure MLtonParallelBasic :> MLTON_PARALLEL_BASIC =
struct

  type void = unit
  type work = unit -> void

  structure HM = MLton.HM
  structure HH = HM.HierarchicalHeap
  structure I = MLtonParallelInternal
  structure T = MLton.Thread

  datatype job = Work of (unit -> void) * unit HH.t * int
               | Thread of unit T.t * unit HH.t

  val dbgmsg =
      if false
      then fn msg => I.dbgmsg ("basic: " ^ msg)
      else fn _ => ()

  val numberOfProcessors = I.numberOfProcessors

  structure Q = WorkStealing (structure W =
                              struct
                                type work = job
                                val numberOfProcessors =
                                 fn () => numberOfProcessors
                              end)
    :> PARALLEL_WORKQUEUE where type work = job

  type token = Q.token

  datatype t = Capture of token * job

  val processorNumber = I.processorNumber

  exception Parallel of string

  val suspends = Array.array (numberOfProcessors, 0)
  fun incSuspends p = Array.update (suspends, p, Array.sub (suspends, p) + 1)

  val schedThreads = Array.array (numberOfProcessors, NONE)
  val hhToSetDead = Array.array (numberOfProcessors, NONE)

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
       OS.Process.exit OS.Process.failure)

  fun useHH (hh : unit HH.t) : unit = (HM.enterGlobalHeap ();
                                       HH.set hh;
                                       HH.setUseHierarchicalHeap true;
                                       HM.exitGlobalHeap ();
                                       dbgmsg "useHH")

  fun stopUseHH (() : unit) : unit = (HM.enterGlobalHeap ();
                                      HH.setUseHierarchicalHeap false;
                                      HM.exitGlobalHeap ();
                                      dbgmsg "stopUseHH")

  fun maybeSetHHDead (p: int): unit =
      case Array.sub (hhToSetDead, p)
       of NONE => ()
        | SOME hh => (HH.setDead hh;
                      Array.update(hhToSetDead, p, NONE))

  val numSteals = Array.array (I.numberOfProcessors, 0)
  fun incrSteals n =
      let
        val p = I.processorNumber ()
      in
        Array.update (numSteals, p, Array.sub (numSteals, p) + n)
      end

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

                    val () = incrSteals 1
                    val childHH = HH.new ()
                    val () = HH.appendChild (parentHH, childHH, sharedLevel)
                in
                    dbgmsg "switching to new thread";
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
                    dbgmsg "returned to scheduler from new thread";
                    stopUseHH ();
                    maybeSetHHDead p
                end

              | Thread (k, hh) =>
                (*
                 * This doesn't have to be stolen as it
                 * could be a resume parent
                 *)
                (dbgmsg "switching to suspended thread";
                 T.switch (fn st =>
                              (Array.update (schedThreads,
                                             p,
                                             SOME (T.prepare (st, ())));
                               useHH hh;
                               T.prepare (T.prepend (k, fn () => (useHH hh;
                                                                  unlocker ())),
                                          ())));
                 dbgmsg "returned to scheduler from suspended thread";
                 stopUseHH ();
                 maybeSetHHDead p))
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
                                 SOME (MLton.Parallel.Unsafe.initPrimitiveThread (T.new schedule)))
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
  fun getNumSteals () = Array.foldl (op+) 0 numSteals

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
          val hh = HH.get ()
      in
          Array.update(hhToSetDead, p, SOME hh);
          T.switch (fn _ =>
                       let
                           val () = HM.enterGlobalHeap ()
                           val t = valOf (Array.sub (schedThreads, p))
                       in
                           t
                       end)
      end

  val () = MLton.Parallel.registerProcessorFunction schedule
  (* init MUST come after schedulerLoop has been exported *)

  val successfulSteals = evaluateInGlobalHeap Q.reportSuccessfulSteals
  val failedSteals = evaluateInGlobalHeap Q.reportFailedSteals
  val suspends = evaluateInGlobalHeap (fn () => Array.foldl op+ 0 suspends)
end

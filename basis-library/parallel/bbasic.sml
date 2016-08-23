structure MLtonParallelBasic :> MLTON_PARALLEL_BASIC =
struct

  type void = unit
  type work = unit -> void

  structure T = MLtonThread

  datatype job = Work of (unit -> void)
               | Thread of (unit T.t)

  val numberOfProcessors = MLtonParallelInternal.numberOfProcessors

  structure Args : ARGS =
  struct
  val QUEUE_ARRAY_SIZE = 8192
  val activeQueue = Array.tabulate (numberOfProcessors, fn i => i)
  val readyQueues = Array.array (numberOfProcessors, [])
  val emptyQueues = Array.array (numberOfProcessors, [])
  val blocked = Array.array (QUEUE_ARRAY_SIZE, 0)
  end

  (* val numberOfProcessors = numberOfProcessors *)

  structure Q = BWorkStealing (structure W = struct
                               type work = job
                               val numberOfProcessors = fn () => numberOfProcessors
                               end
                              structure Args = Args)
    :> PARALLEL_WORKQUEUE where type work = job

  open Args

  datatype 'a t = Suspend of bool * 'a T.t * Q.susp
                | Capture of bool * 'a T.t

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

  val ioqueues = Array.array (numberOfProcessors, [])

  fun prerr s = (TextIO.output (TextIO.stdErr, s);
                 TextIO.flushOut TextIO.stdErr)

  fun addtoio ((t, f) : unit t * (unit -> bool)) =
      let val p = processorNumber ()
          val qi = Array.sub (activeQueue, p)
                   handle Subscript => (print "62\n"; raise Subscript)
          (* val _ = print ("addtoio at " ^ (Int.toString p) ^ "\n") *)
          val q = Array.sub (ioqueues, p)
                  handle Subscript => (print "65\n"; raise Subscript)
      in
          Array.update (blocked, qi, Array.sub (blocked, qi) + 1);
          Array.update (ioqueues, p, (t, f, qi)::q)
          handle Subscript => (print "69\n"; raise Subscript)
      end


  fun resume (Suspend (lat, k, q), v) =
      let
        val p = processorNumber ()
      in
        Q.resumeWork (lat, p, q, (Q.newWork p, Thread (T.prepend (k, fn () => v))))
        handle e => (print "here 59\n"; raise e)
      end
    | resume (Capture (lat, k), v) =
      let
        val p = processorNumber ()
        (* val _ = print "resuming\n" *)
      in
        Q.addWork (lat, p, [(Q.newWork p, Thread (T.prepend (k, fn () => v)))])
        handle e => (print "here 67\n"; raise e)
      end

  fun latency (Suspend (lat, _, _)) = lat
    | latency (Capture (lat, _)) = lat

  fun mkLat (Suspend (_, k, q)) = Suspend (true, k, q)
    | mkLat (Capture (_, k)) = Capture (true, k)

  fun procio p =
      let val q = Array.sub (ioqueues, p)
                  handle Subscript => (print "97\n"; raise Subscript)
          val readyForDeque = Array.array (QUEUE_ARRAY_SIZE, [])
          val newlyReadyDeques = ref []
          fun addToResume (t, qi) =
              (Array.update (readyForDeque, qi,
                             t::Array.sub (readyForDeque, qi));
               Array.update (blocked, qi, Array.sub (blocked, qi) - 1);
               if not (List.exists (fn x => x = qi) (!newlyReadyDeques)) then
                   newlyReadyDeques := qi::(!newlyReadyDeques)
               else ())
              handle Subscript => (print "107\n"; raise Subscript)
          val q' =
              List.foldl
                  (fn ((t, f, qi), r) =>
                      (* if latency t then *)
                          if f () then (addToResume (t, qi); r)
                          else (t, f, qi)::r
                      (* else raise (Parallel "Invariant violated!\n") *)
                  )
                  []
                  q
          fun forkn n f =
              let fun fork (l, r) =
                      (Q.addWork (false, p, [(Q.newWork p, Work r)]);
                       l ())
                  fun fork_int n i =
                      if n = 1 then f i else
                      let val left = Int.div (n, 2)
                          val right = n - left
                      in
                          fork
                              ((fn () => fork_int left i),
                               (fn () => fork_int right (i + left)))
                      end
              in
                  fork_int n 0
              end
          fun handleDeque qi =
              let val ts = Array.sub (readyForDeque, qi)
                           handle Subscript => (print "136\n"; raise Subscript)
                  val n = List.length ts
                  fun runi i =
                      let val t =
                          case List.nth (ts, i)
                               handle Subscript => (print "141\n"; raise Subscript) of
                              Suspend (_, t, _) => t
                            | Capture (_, t) => t
                      in
                          MLtonThread.switch (fn _ => T.prepare (t, ()))
                      end
                  fun f () = forkn n runi
              in
                  Q.addWork (false, p, [(Q.newWork p, Work f)])
              end
      in
          Array.update (ioqueues, p, q');
          List.map handleDeque (!newlyReadyDeques)
      end

  fun schedule countSuspends () =
    let
      fun loop (countSuspends, p) =
          let
              val _ = procio p
                      handle Subscript => (print "in procio\n"; raise Subscript)
          in
            case MLtonThread.atomically (fn () => Q.getWork p)
                 handle Subscript => (print "in getWork\n"; raise Subscript)
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
                           handle Subscript => (print "in startWork\n"; raise Subscript)
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
                                   MLtonProcess.exit MLtonProcess.Status.failure)
                             | e => (TextIO.output (TextIO.stdErr,
                                                  ("WARNING: Caught exception \""
                                                   ^ (General.exnMessage e)
                                                   ^ "\" in parallel scheduler!\n"));
                                   TextIO.flushOut TextIO.stdErr;
                                   MLtonProcess.exit MLtonProcess.Status.failure)
                  (* A job should never return -- we will only get here in exceptional
                    conditions. *)
                  (* NB we call processorNumber again here in case that this
                    job has been split across two processors *)
                  val p = processorNumber ()
                  val () = incSuspends p
                  val () = Q.finishWork p
                           handle Subscript => (print "in finishWork\n"; raise Subscript)
                in
                  loop (false, p)
                end
          end
    in
      loop (countSuspends, processorNumber ()) 0
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
                    case (Q.getWork p)
                         handle Subscript => (print "in getWork\n"; raise Subscript)
                     of SOME (_, Work w) => T.new (fn () => (Q.startWork p; w ()))
                      | SOME (_, Thread k') =>
                        ((* print "before-prepend\n"; *)
                         T.prepend (k', fn () => (Q.startWork p))
                         (*before print "after-prepend\n" *))
                      | NONE => T.new (schedule false)
                (* to disable hijacking, use this instead
                val t = T.new schedule
                 *)
                fun add (lat, w) = (Q.addWork (lat, p, [(Q.newWork p, Work w)]))
                                   handle Subscript => (print "in addWork\n"; raise Subscript)
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
              val q = (Q.suspendWork p)
                      handle Subscript => (print "in suspendWork\n"; raise Subscript)
            in
              f (Suspend (false, k, q)) (* XXX *)
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
              val () = (Q.finishWork p)
                       handle Subscript => (print "in finishWork\n"; raise Subscript)
            in
              f (Capture (false, k)) (* XXX *)
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
                            Q.addWork (false, p, [(Q.newWork p, Thread k)]); (* XXX *)
                            incSuspends p;
                            Q.finishWork p
                          end)
        else
          ()
      end

  fun addRightLat (lat, w) =
      let
        val p = processorNumber ()
        val t = Q.newWork p
      in
        if Q.shouldYield p then
          (* Switch to a new thread *)
          capture' (p, fn (p, k) =>
                         let in
                           (* Add the continuation first -- it is higher priority *)
                           Q.addWork (lat, p, [(Q.newWork p, Thread (T.prepend (k, fn () => t))),
                                          (t, Work w)]);
                           incSuspends p;
                           Q.finishWork p
                         end)
        else
          let
            fun add (lat, w) = Q.addWork (lat, p, [(Q.newWork p, Work w)])
          in
            (* add any delayed work *)
            (* XXX maybe should run delayed work and queue the currrent thread too? *)
            app add (rev (Array.sub (delayed, p)));
            Array.update (delayed, p, nil);
            Q.addWork (lat, p, [(t, Work w)]);
            t
          end
      end

  fun addRight w = addRightLat (false, w)

  (* smuller: XXX This adds the current thread to the lqueue and so is totally
     wrong. Fix it. *)
  fun addLeftLat (lat, w) =
      let
        val p = processorNumber ()
        val t = Q.newWork p
        val _ = print "addLeftLat\n"
      in
        if Q.shouldYield p then
          capture' (p, fn (p, k) =>
                         let in
                           Q.addWork (lat, p, [(Q.newWork p, Work w),
                                          (t, Thread (T.prepend (k, fn () => t)))]);
                           incSuspends p;
                           Q.finishWork p
                         end)
        else
          T.switch (fn k =>
                       T.prepare
                       (T.new (fn () =>
                                  let
                                    fun add (lat, w) = Q.addWork (lat, p, [(Q.newWork p, Work w)])
                                  in
                                    (* add any delayed work *)
                                    (* XXX maybe should run delayed work and queue the currrent thread too? *)
                                    app add (rev (Array.sub (delayed, p)));
                                    Array.update (delayed, p, nil);
                                    Q.addWork (lat, p, [(t, Thread (T.prepend (k, fn () => t)))]);
                                    w ()
                                  end), ()))
      end

  fun addLeft w = addLeftLat (false, w)

  fun removeLat lat t = Q.removeWorkLat (lat, processorNumber (), t)
  fun remove t = Q.removeWork (processorNumber (), t)

(* XXX left? what about the right? *)
  fun delayedAddLat (lat, w) =
      let
        (* PERF use a array-based buffer to avoid allocation *)
        val p = processorNumber ()
        val ws = Array.sub (delayed, p)
      in
        Array.update (delayed, p, (lat, w)::ws)
      end

  fun delayedAdd w = delayedAddLat (false, w)

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
              val ((lat, w), ws) =
                  case rev ws of (lat, w)::ws => ((lat, w), ws) | nil => raise Match
              val () = Array.update (delayed, p, nil)
              fun add nil = ()
                | add ((lat, w)::ws) =
                  Q.addWork (lat, p, [(Q.newWork p, Work (fn () => (add ws; w ())))])
              (* add any lower priority work *)
              val () = add ws
            in
              (* now what do to with w? *)
              if Q.shouldYield p then
                (Q.addWork (false, p, [(Q.newWork p, Work w)]); (* XXX *)
                 (* this is counted in schedule: incSuspends p; *)
                 Q.finishWork p;
                 schedule true ())
              else
                w ()
            end
      end

  val () = (_export "Parallel_run": (unit -> void) -> unit;) (schedule false)
  (* init MUST come after schedulerLoop has been exported *)
  val () = (_import "Parallel_init" runtime private: unit -> unit;) ()

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

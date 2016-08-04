structure MLtonParallelBasic :> MLTON_PARALLEL_BASIC =
struct

val takeLock = _import "Parallel_lockTake" runtime private: int ref -> unit;
val releaseLock = _import "Parallel_lockRelease" runtime private: int ref -> unit;

val printLock = ref ~1

fun print s =
    (takeLock printLock;
     TextIO.print s;
     releaseLock printLock)


type void = unit
type work = unit -> void
type proc = int

exception WorkQueue
exception QueueSize

structure T = MLtonThread
structure A = Array
structure V = Vector
structure Tm = Time

  datatype job = JWork of (unit -> void)
               | Thread of (unit T.t)
               | RThread of T.Runnable.t * int

(* type work = unit -> void *)

  val numberOfProcessors = MLtonParallelInternal.numberOfProcessors

  val processorNumber = MLtonParallelInternal.processorNumber
  val P = numberOfProcessors

val yield = _import "Parallel_yield" runtime private: unit -> unit;
val compareAndSwap = _import "Parallel_compareAndSwap" runtime private: int ref * int * int -> bool;

structure S = MLtonSignal

val prim_sig = MLtonItimer.signal MLtonItimer.Real
val sec_sig = Posix.Signal.usr1

val delta = 100 (* microseconds *)

fun block_sigs p =
    if p = 0 then
        S.Mask.block (S.Mask.some [prim_sig])
        (* S.Mask.block S.Mask.all *)
    else
        S.Mask.block (S.Mask.some [sec_sig])
        (* S.Mask.block S.Mask.all *)

fun unblock_sigs p =
    if p = 0 then
        S.Mask.unblock (S.Mask.some [prim_sig])
    else
        S.Mask.unblock (S.Mask.some [sec_sig])

fun atomically f =
    let val p = processorNumber () in
        block_sigs p;
        T.atomically f
        before unblock_sigs p
    end

  datatype 'a t = Suspend of bool * 'a T.t
                | Capture of bool * 'a T.t

  val ioqueues = Array.array (numberOfProcessors, [])
  val workingOnLatency = Array.array (numberOfProcessors, false)
  val lookingForWork = Array.array (numberOfProcessors, true)

val WORK_ARRAY_SIZE = 1024
(* private state *)
datatype token = Token of int * queue option ref
and entry = Empty | Work of (token * job (* * int *))
and queue = Queue of
            {
              top : int ref,
              bottom : int ref,
              work : entry A.array ref,
              lat : bool
            }

fun newQueue lat =
    Queue {
      top = ref 0,
      bottom = ref 0,
      work = ref (A.array (WORK_ARRAY_SIZE, Empty)),
      lat = lat
    }

val fqueues = A.array (P, newQueue true)
val bqueues = A.array (P, newQueue false)

val successfulSteals = ref 0
fun resetSteals () = successfulSteals := 0
fun incSteals () = successfulSteals := !successfulSteals + 1
fun reportSuccessfulSteals () = !successfulSteals
fun reportFailedSteals () = 0

val waiting = ~1
val notwaiting = ~2
type cellval = (bool * job) option
val commcells = V.tabulate (P, fn _ => ref notwaiting)
val taskcells = A.array (P, NONE)
val nextdeal = A.array (P, Tm.zeroTime)

val nextToken = ref 0

fun newWork p = Token (!nextToken, ref NONE)
                before (nextToken := !nextToken + 1)

fun stringOfToken (Token (i, _)) = Int.toString i

fun resize (Queue {top, bottom, work, ... }) =
    let
        val i = !top
        val j = !bottom
        val w = !work
        val l = A.length w
    in
        if i - j < l div 2 then
            let
                val () = print "copying!"
                fun erase k = if k = i then ()
                              else (A.update (w, k, Empty);
                                    erase (k + 1))
                fun copy k = if k = i then erase j
                             else (A.update (w, k - j, A.sub (w, k));
                                   copy (k + 1))
            in
                copy j;
                top := i - j;
                bottom := 0
            end
        else
            let
                val () = print "resizing!"
                val w = A.tabulate (l * 2,
                                    fn k => if k < (i - j) then A.sub (w, k + j)
                                            else Empty)
            in
                work := w;
                top := i - j;
                bottom := 0
            end
    end

fun add (q as Queue {top, bottom, work, ... }) (tw as (t, w)) =
    let
        val i = !top
        val () = if i = A.length (!work) then resize q else ()
        val i = !top (* in case of resize *)
        val Token (_, qr) = t
    in
        (A.update (!work, i, Work tw)
         handle e => (print "here 159\n"; raise e));
        qr := SOME q;
        top := i + 1
    end

fun popTop (q as Queue {top, bottom, work, ... }) =
    let val i = !top
        val j = !bottom
        val w = !work
    in
        if i = j then raise WorkQueue (* empty *)
        else
            (bottom := j + 1;
             case A.sub (w, j) of
                 Empty => raise WorkQueue
               | Work (t, w) => w)
            handle e => (print "here 174\n"; raise e)
    end

fun topIsRThread (q as Queue {top, bottom, work, ... }) =
    let val i = !top
        val j = !bottom
        val w = !work
    in
        if i = j then false else
        case A.sub (w, j) of
            Work (_, RThread _) => true
          | _ => false
    end

fun dealAttempt p =
    let val (fq as Queue {top, bottom, work, ...}) = A.sub (fqueues, p)
        val fi = !top
        val fj = !bottom
        val fw = !work
        val (bq as Queue {top, bottom, work, ...}) = A.sub (bqueues, p)
        val bi = !top
        val bj = !bottom
        val bw = !work
    in
        if (fi = fj orelse topIsRThread fq) andalso
           (bi = bj orelse topIsRThread bq) then (* both empty *)
            false
        else
            let val p' = Word.toIntX (MLtonRandom.rand ()) mod P
                val c = V.sub (commcells, p')
                (* val _ = A.update (taskcells, p, NONE)
                   Shouldn't do this; the receiver should reset it. *)
            in
                if p' = p then false
                else
                    if !c = waiting then
                        case A.sub (taskcells, p) of
                            NONE =>
                            if compareAndSwap (c, waiting, p) then
                                let val (isLat, q) =
                                        if (fi = fj orelse topIsRThread fq)
                                        then (false, bq)
                                        else (true, fq)
                                in
                                    A.update (taskcells, p,
                                              SOME (isLat, popTop q));
                                    true
                                end
                            else
                                false
                          | SOME _ => false
                    else false
            end
    end

fun communicate p =
    if Time.> (Time.now (), A.sub (nextdeal, p)) then
        ((dealAttempt p
         handle e => (print "here 219\n"; raise e));
         let val now = Time.now ()
             val mi = case Int.maxInt of SOME i => i | NONE => 1000000
             val rnd = Real.abs
                           (Real./
                            (Real.fromInt (Word.toIntX (MLtonRandom.rand ())),
                             Real.fromInt mi))
                       handle Overflow => (print "here 171\n"; raise Overflow)
             val mult = Math.ln rnd
             val us = LargeInt.fromInt
                          (Real.floor (Real.* (Real.fromInt delta, mult)))
         in
             A.update (nextdeal, p, Tm.- (Tm.now (),
                                          Tm.fromMicroseconds us))
         end)
    else
        ()


fun addWork (lat, p, tws) =
    let val q = if lat then A.sub (fqueues, p) else A.sub (bqueues, p)
    in
        ignore (List.map (add q) tws)
    end

fun maybeMove {top, bottom, work} =
    let val i = !top
        val j = !bottom
    in
        if i = j then
            (top := 0;
             bottom := 0)
        else
            ()
    end

fun resumeWork (lat, p, (t, w)) = addWork (lat, p, [(t, w)])

fun removeWork (p, Token (ti, qr)) =
    case !qr of
        SOME (Queue {top, bottom, work, ...}) =>
        let val tp = !top
            val bot = !bottom
            val w = !work
            fun unloop i =
                if i >= tp - 1 then (top := tp - 1; ())
                else
                    (A.update (w, i, A.sub (w, i + 1));
                     unloop (i + 1))
            fun loop i =
                if i >= tp then false
                else
                    case A.sub (w, i) of
                        Empty => raise WorkQueue
                      | Work (Token (t, _), w) =>
                        if t = ti then
                            (unloop i; true)
                        else
                            loop (i + 1)
        in
            (loop bot) handle e => (print "here 279\n"; raise e)
        end
      | NONE => raise WorkQueue

fun removeWorkLat (_, p, t) = removeWork (p, t)

fun shouldYield _ = false

val policyName = "Prompt private deques"

  val profileDisable = _import "GC_profileDisable" runtime private: unit -> unit;
  val profileEnable = _import "GC_profileEnable" runtime private: unit -> unit;

  val signalThread = _import "signal_thread" runtime private: int * SysWord.word -> unit;

  exception Parallel of string

  val suspends = Array.array (numberOfProcessors, 0)
  fun incSuspends p = Array.update (suspends, p, Array.sub (suspends, p) + 1)

  val delayed = Array.array (numberOfProcessors, nil)

  val enabled = ref true

  fun prerr s = (TextIO.output (TextIO.stdErr, s);
                 TextIO.flushOut TextIO.stdErr)


  fun resume (Suspend (lat, k), v) =
      let
        val p = processorNumber ()
        val _ = if p = 0 then print "enter resume\n" else ()
        val t = T.prepend (k, fn () => v)
                handle e => (print "here 117\n"; raise e)
      in
        (resumeWork (lat, p, (newWork p, Thread t))
        handle e => (print "here 59\n"; raise e))
        before (if p = 0 then print "leave resume\n" else ())
      end
    | resume (Capture (lat, k), v) =
      let
        val p = processorNumber ()
        (* val _ = print "resuming\n" *)
      in
        addWork (lat, p, [(newWork p, Thread (T.prepend (k, fn () => v)))])
        handle e => (print "here 67\n"; raise e)
      end

  fun latency (Suspend (lat, _)) = lat
    | latency (Capture (lat, _)) = lat

  fun mkLat (Suspend (_, k)) = Suspend (true, k)
    | mkLat (Capture (_, k)) = Capture (true, k)

  fun addtoio ((t, f) : unit t * (unit -> bool)) =
      let val p = processorNumber ()
          val _ = print ("addtoio at " ^ (Int.toString p) ^ "\n")
          val q = Array.sub (ioqueues, p)
      in
          Array.update (ioqueues, p, (t, f)::q)
      end

  val inpio = ref false

  fun procio p =
      let (* val _ = if p = 1 then print "enter procio\n" else () *)
          val _ = if p = 0 then ((if !inpio then
                                     print "Entered procio twice!\n"
                                 else ());
                                 inpio := true)
                  else ()
          val q = Array.sub (ioqueues, p)
          val (resumed, q') =
              List.foldl
                  (fn ((t, f), (rsm, r)) =>
                      (* if latency t then *)
                          if f () then (print "resumed\n"; resume (mkLat t, ()); (true, r))
                                       handle e => (print "here 142\n"; raise e)
                          else (rsm, (t, f)::r)
                      (* else raise (Parallel "Invariant violated!\n") *)
                  )
                  (false, [])
                  q
      in
          Array.update (ioqueues, p, q');
          resumed
          before (if p = 0 then ((*print "leave procio\n";*) inpio := false) else ())
      end

val waitingForTask = ref false

fun getWorkLat lat p =
    let (* val _ = print "getWorkLat\n" *)
        val _ = if p <> processorNumber () then
                    print "wrong p at 389\n"
                else ()
        val q = (if lat then A.sub (fqueues, p) else A.sub (bqueues, p))
                handle e => (print "here 372\n"; raise e)
        val Queue {top, bottom, work, ...} = q
        val i = !top
        val j = !bottom
        val w = !work
        val c = (V.sub (commcells, p))
                handle e => (print "here 376\n"; raise e)
        val endwait = Tm.+ (Tm.now(),
                            Tm.fromMicroseconds (LargeInt.fromInt (delta * P)))
        fun waitForTask' p' =
            (case A.sub (taskcells, p') of
                NONE => waitForTask' p'
              | SOME (b, RThread t) => (print "stole RThread\n";
                                        SOME (b, RThread t))
                                       before A.update (taskcells, p', NONE)
              | SOME bw => SOME bw
                           before A.update (taskcells, p', NONE))
            handle e => (print "here 384\n"; raise e)
        fun waitForTask () =
            (let val _ = if p = 0 then waitingForTask := true else ()
                val p' = !c
                val _ = if p' = notwaiting then print "??????\n" else ()
            in
                if p' = waiting then
                    (procio p;
                     let val i = !top (* array may have changed *)
                         val j = !bottom
                         val w = !work
                     in
                         if i = j then (* Still no task *)
                             (* if Tm.> (Tm.now (), endwait) then (* time out *)
                                 if compareAndSwap (c, waiting, notwaiting) then
                                     NONE
                                 else
                                     (* Got a new task in the meantime *)
                                     waitForTask' (!c)
                             else *)
                                 (yield ();
                                  communicate p;
                                  waitForTask ())
                         else
                             (* We have a task now; try to signal notwaiting *)
                             if compareAndSwap (c, waiting, notwaiting) then
                                 (* Succeeded; give back the bottom task *)
                                 (case A.sub (w, i - 1) of
                                     Empty => raise WorkQueue
                                   | Work (_, wrk) =>
                                     (top := i - 1;
                                      SOME (lat, wrk)))
                                 handle e => (print "here 409\n"; raise e)
                             else
                                 (* Got a new task in the meantime *)
                                 waitForTask' (!c)
                     end)
                else
                    waitForTask' p'
            end) before waitingForTask := false
    in
        if i = j then (* empty *)
            let val _ = block_sigs p
                val _ = c := waiting
                (* val _ = print "waiting for a task\n" *)
                val _ = (top := 0; bottom := 0)
            in
                case waitForTask () of
                    SOME (isLat, wrk) =>
                    ((* print "got a task\n"; *)
                     unblock_sigs p;
                     SOME (true, isLat, wrk))
                  | NONE => NONE
            end
        else (* pop bottom task *)
            (case A.sub (w, i - 1) of
                Empty => raise WorkQueue
              | Work (_, wrk) =>
                (top := i - 1;
                 SOME (false, lat, wrk)))
            handle e => (print "here 437\n"; raise e)
    end

fun checkP ln p =
    let val p' = processorNumber () in
        if p <> p' then
            print ("wrong p at " ^ (Int.toString ln) ^ ": " ^
                   (Int.toString p) ^ " != " ^ (Int.toString p') ^ "\n")
        else ()
    end

fun getWork p =
    (case getWorkLat true p of
        NONE => getWorkLat false p
      | SOME w => SOME w)
    before
    (if p <> processorNumber () then
         print "wrong p at 478\n"
     else ())

fun getWorkMaybeLat lat p =
    let val q = if lat then A.sub (fqueues, p) else A.sub (bqueues, p)
        val Queue {top, bottom, work, ...} = q
        val i = !top
        val j = !bottom
        val w = !work
    in
        if i = j then NONE
        else
            (case A.sub (w, i - 1) of
                Empty => raise WorkQueue
              | Work (_, wrk) =>
                (top := i - 1;
                 SOME (false, lat, wrk)))
            handle e => (print "here 460\n"; raise e)
    end

fun getWorkMaybe p =
    case getWorkMaybeLat true p of
        NONE => getWorkMaybeLat false p
      | SOME w => SOME w

fun startWork p = ()

fun finishWork p = ()

fun checkAndLoop p () =
    let fun loop () = (yield (); loop ())
    in
        (* print ("checking on " ^ (Int.toString p) ^ "\n");*)
        checkP 0 p;
        loop ()
    end

fun suspendWork p = NONE
  fun schedule countSuspends () =
    let
        fun busyloop p n =
            (yield ();
             (if n = 0 then print ((Int.toString p) ^ "\n")
              else ());
            busyloop p ((n + 1) mod 1000000) )
      fun loop (countSuspends, p) =
          let
              val p = processorNumber ()
              val _ = MLtonThread.atomically (fn () => procio p)
              val _ = communicate p
              (* val _ = Array.update (lookingForWork, p, true) *)
              fun setLFW () = Array.update (lookingForWork, p, true)
              fun clearLFW () = Array.update (lookingForWork, p, false)
              val _ = setLFW ()
              val _ = checkP 523 p
          in
            case atomically
                     (fn () => ((checkP 526 p;
                                 getWork p handle e => (print "here 483\n";
                                                        raise e))
                                before
                                ((* print "got work\n"; *)
                                  checkP 531 p (*;
                                  (*  clearLFW ();*)
                                 print "updated\n" *))))
             of NONE =>
                let (* val _ = print "didn't actually get work\n" *) in
                  (* if !enabled then (enabled := false; profileDisable ()) else (); *)
                  loop (countSuspends, p)
                end
              | SOME (nonlocal, lat, j) =>
                let
                    val _ = checkP 541 p
                    val _ = clearLFW ()
                    (* val p = processorNumber () *)
                  val _ = print ("starting " ^ (if nonlocal then "nonlocal "
                                                else "") ^ "thread\n")
(*                    val () = print ("updating latency to " ^
                                    (if lat then "true" else "false") ^ "\n") *)
                    val () = Array.update (workingOnLatency, p, lat)

                  val () = if countSuspends andalso nonlocal then incSuspends p else ()
                  (* val () = if not (!enabled) then (enabled := true; profileEnable ()) else (); *)
                  val () = startWork p

                  val () = (case j
                             of JWork w => (print "starting JWork\n"; w ())
                              | Thread k => (print "starting Thread\n";
                                             (T.switch (fn _ => T.prepare (k, ())))
                                            handle e => (print "here 472\n";
                                                         raise e))
                              | RThread (r, p') => (print "starting RThread\n";
                                                    (if p' <> p then
                                                         print ("RThread was stolen from " ^ (Int.toString p') ^ ". I'm " ^ (Int.toString p) ^ "\n")
                                                     else ());
                                              T.switch (fn _ => r)
                                              handle e => (print "here 512\n";
                                                         raise e)))
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
                  val () = finishWork p
                in
                  loop (false, p)
                end
          end
    in
        (* checkAndLoop (processorNumber ()) () *)
        loop (countSuspends, processorNumber ())
        (* busyloop (processorNumber ()) 0 *)
    end

  fun capture' (p, tail) =
      let val _ = print "capture'\n" in
      T.switch
          (fn k =>
              (* Note that we cannot call addWork on the current thread!
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
                    case getWorkMaybe p
                     of SOME (_, _, JWork w) => T.new (fn () => (startWork p; w ()))
                      | SOME (_, _, Thread k') =>
                        (print "before-prepend\n";
                         T.prepend (k', fn () => (startWork p))
                         before print "after-prepend\n")
                      | SOME (_, _,  RThread (r, _)) =>
                        T.new (fn () => (startWork p; T.switch (fn _ => r)))
                      | NONE => T.new (schedule false)
                (* to disable hijacking, use this instead
                val t = T.new schedule
                 *)
                fun add (lat, w) = addWork (lat, p, [(newWork p, JWork w)])
              in
                (* XX maybe this should move out (before suspend/finishWork) *)
                (* add any delayed work *)
                app add (rev (Array.sub (delayed, p)));
                Array.update (delayed, p, nil);
                (* return the new thread to switch to *)
                print "preparing\n";
                (T.prepare (T.prepend (t, tail), (p, k))
                handle e => (print "here 176\n"; raise e))
              end)
      end

  fun suspend f =
      let
        val p = processorNumber ()
        val _ = print "suspend\n"
        (* val _ = print ("suspend at " ^ (Int.toString p) ^ "\n") *)
        fun tail (p, k) =
            let
              val () = incSuspends p
              val q = suspendWork p
              (* Block signals so we don't preempt while we are suspending *)
              (* val () = block_sigs p *)
            in
              f (Suspend (false, k)) (* XXX *)
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
              val () = finishWork p
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
        if shouldYield p then
          capture' (p, fn (p, k) =>
                          let in
                            addWork (false, p, [(newWork p, Thread k)]); (* XXX *)
                            incSuspends p;
                            finishWork p
                          end)
        else
          ()
      end

  fun event f =
      let val p = processorNumber ()
          val _ = print "event\n"
      in
(*
          if Array.sub(workingOnLatency, p) then
              raise (Parallel "nested events")
          else
*)
              let
                  val _ = Array.update(workingOnLatency, p, true)
                  val res = f ()
                  val _ = Array.update(workingOnLatency, p, false)
              in
                  res
              end
      end

  fun addRightLat (lat, w) =
      let
        val p = processorNumber ()
        val _ = block_sigs p
        val t = newWork p
        val sw = ref false
        fun w' () = if !sw then raise Parallel "work run twice"
                    else (sw := true; w ())

      in
        if shouldYield p then
          (* Switch to a new thread *)
          capture' (p, fn (p, k) =>
                         let in
                           (* Add the continuation first -- it is higher priority *)
                           addWork (lat, p, [(newWork p, Thread (T.prepend (k, fn () => t))),
                                          (t, JWork w')])
                           handle e => (print "here 622\n"; raise e);
                           incSuspends p;
                           finishWork p
                         end)
        else
          let
            fun add (lat, w) = addWork (lat, p, [(newWork p, JWork w)])
          in
            (* add any delayed work *)
            (* XXX maybe should run delayed work and queue the currrent thread too? *)
            app add (rev (Array.sub (delayed, p)));
            Array.update (delayed, p, nil);
            addWork (lat, p, [(t, JWork w')]);
            unblock_sigs p;
            t
          end
      end

  fun addRight w = addRightLat (false, w)

  (* smuller: XXX This adds the current thread to the lqueue and so is totally
     wrong. Fix it. *)
  fun addLeftLat (lat, w) =
      let
        val p = processorNumber ()
        val t = newWork p
        val _ = print "addLeftLat\n"
      in
        if shouldYield p then
          capture' (p, fn (p, k) =>
                         let in
                           addWork (lat, p, [(newWork p, JWork w),
                                          (t, Thread (T.prepend (k, fn () => t)))])
                         handle e => (print "here 654\n"; raise e);
                           incSuspends p;
                           finishWork p
                         end)
        else
          T.switch (fn k =>
                       T.prepare
                       (T.new (fn () =>
                                  let
                                    fun add (lat, w) = addWork (lat, p, [(newWork p, JWork w)])
                                  in
                                    (* add any delayed work *)
                                    (* XXX maybe should run delayed work and queue the currrent thread too? *)
                                    app add (rev (Array.sub (delayed, p)));
                                    Array.update (delayed, p, nil);
                                    addWork (lat, p, [(t, Thread (T.prepend (k, fn () => t)))])
                                    handle e => (print "here 670\n"; raise e);
                                    w ()
                                  end), ()))
      end

  fun addLeft w = addLeftLat (false, w)

  fun removeLat lat t = removeWorkLat (lat, processorNumber (), t)
  fun remove t = removeWork (processorNumber (), t)

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
                    finishWork p;
                    schedule true ())
          | ws =>
            let
              val ((lat, w), ws) =
                  case rev ws of (lat, w)::ws => ((lat, w), ws) | nil => raise Match
              val () = Array.update (delayed, p, nil)
              fun add nil = ()
                | add ((lat, w)::ws) =
                  addWork (lat, p, [(newWork p, JWork (fn () => (add ws; w ())))])
              (* add any lower priority work *)
              val () = add ws
            in
              (* now what do to with w? *)
              if shouldYield p then
                (addWork (false, p, [(newWork p, JWork w)]); (* XXX *)
                 (* this is counted in schedule: incSuspends p; *)
                 finishWork p;
                 schedule true ())
              else
                w ()
            end
      end

  fun signalOthers p sg n =
      if n = numberOfProcessors - 1 then ()
      else if n = p - 1 then signalOthers p sg (n + 1)
      else
          ((* print ("signaling" ^ (Int.toString n) ^ "\n"); *)
           signalThread (n, Posix.Signal.toWord sg);
           signalOthers p sg (n + 1))

  fun interruptFst t =
      let val p = processorNumber ()
          val _ = if !waitingForTask then print "interrupt while waiting!!!\n"
                  else ()
          val _ = block_sigs p
          val _ = procio p
          val _ = communicate p
          val lat = Array.sub (workingOnLatency, p)
          val lfw = Array.sub (lookingForWork, p)
      in
       (print ("interruptFst on " ^ (Int.toString p) ^ "\n");
        signalOthers p sec_sig 0;
        (* signalThread (0, Posix.Signal.toWord sec_sig); *)
        (if not lfw then addWork (lat, p, [(newWork p, RThread (t, p))]) else ());
        unblock_sigs p;
        (* T.prepare (T.new (schedule true), ())) *)
        T.prepare (T.new (checkAndLoop p), ()))
      end

  fun interrupt t =
      let val p = processorNumber ()
          val _ = block_sigs p
          val _ = procio p
          val _ = communicate p
          val lat = Array.sub (workingOnLatency, p)
          val lfw = Array.sub (lookingForWork, p)
      in
       (print ("interrupt on " ^ (Int.toString p) ^ "\n");
        (if not lfw then addWork (lat, p, [(newWork p, RThread (t, p))]) else ());
        unblock_sigs p;
        T.prepare (T.new (schedule true), ()))
        (* if p = 1 then t (* *)
        else
            T.prepare (T.new (checkAndLoop p), ())) *)
      end

  fun simple () =
      print "interrupt\n"

  val p = processorNumber ()

 (*  val _ = S.setHandler (sg, (S.Handler.ignore)) *)

  fun init () =
      let val p = processorNumber ()
          (* val _ = print ("setting handler on " ^ (Int.toString p) ^ "\n") *)
          val _ = S.setHandler (prim_sig, (S.Handler.handler interruptFst))
          val _ = S.setHandler (sec_sig, (S.Handler.handler interrupt))
          (* val _ = print ("in init " ^ (Int.toString p) ^ "\n") *)
          val iv = Time.fromMilliseconds 1000
      in
          (if p = 0 then
               (MLtonItimer.set (MLtonItimer.Real,
                                 {interval = iv, value = iv});
                S.Mask.unblock (S.Mask.some [prim_sig]);
                S.Mask.block (S.Mask.some [sec_sig]))
           else
               (S.Mask.block (S.Mask.some [prim_sig]);
                S.Mask.unblock (S.Mask.some [sec_sig]))
          )
          (* print ("initialized " ^ (Int.toString p) ^ "\n") *)
          (* schedule false () *)
      end

  fun prun () =
      (init ();
       (* print ("initialized; starting"); *)
       schedule false ())

  val () = init ()

  val () = (_export "Parallel_run": (unit -> void) -> unit;) (prun)
  val () = (_export "Parallel_sched_init": (unit -> void) -> unit;) (init)
  (* init MUST come after schedulerLoop has been exported *)
  val () = (_import "Parallel_init" runtime private: unit -> unit;) ()

  val policyName = policyName
  val maxBytesLive = _import "Parallel_maxBytesLive" runtime private: unit -> Word64.word;
  val gcTime = _import "Parallel_getTimeInGC" runtime private: unit -> Word64.word;
  val successfulSteals = reportSuccessfulSteals
  val failedSteals = reportFailedSteals
  fun resetStatistics () =
      let
        val resetBytesLive = _import "Parallel_resetBytesLive" runtime private: unit -> unit;
      in
        resetSteals ();
        Array.modify (fn _ => 0) suspends;
        resetBytesLive ()
      end
  val suspends = fn () => Array.foldl op+ 0 suspends

end

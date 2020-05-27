(* Author: Sam Westrick (swestric@cs.cmu.edu) *)

(* Scheduler implements a single structure.
 *   ForkJoin : FORK_JOIN
 * It is pulled out of Scheduler at the bottom of this file. *)
structure Scheduler =
struct

  fun arraySub (a, i) = Array.sub (a, i)
  fun arrayUpdate (a, i, x) = Array.update (a, i, x)
  fun vectorSub (v, i) = Vector.sub (v, i)

  structure Queue = DequeABP (*ArrayQueue*)

  structure Thread = MLton.Thread.Basic
  fun threadSwitch t =
    ( Thread.atomicBegin ()
    ; Thread.switchTo t
    )

  structure HM = MLton.HM
  structure HH = MLton.Thread.HierarchicalHeap

  val P = MLton.Parallel.numberOfProcessors
  val myWorkerId = MLton.Parallel.processorNumber

  (* val vcas = MLton.Parallel.arrayCompareAndSwap *)
  (* fun cas (a, i) (old, new) = (vcas (a, i) (old, new) = old) *)
  fun faa (r, d) = MLton.Parallel.fetchAndAdd r d
  fun casRef r (old, new) =
    (MLton.Parallel.compareAndSwap r (old, new) = old)

  fun decrementHitsZero (x : int ref) : bool =
    faa (x, ~1) = 1

  (* ========================================================================
   * DEBUGGING
   *)

  fun die strfn =
    ( print (Int.toString (myWorkerId ()) ^ ": " ^ strfn ())
    ; OS.Process.exit OS.Process.failure
    )

  val doDebugMsg = false

  val printLock : Word32.word ref = ref 0w0
  val _ = MLton.Parallel.Deprecated.lockInit printLock
  fun dbgmsg m =
    if not doDebugMsg then () else
    let
      val p = myWorkerId ()
      val _ = MLton.Parallel.Deprecated.takeLock printLock
      val msg = String.concat ["[", Int.toString p, "] ", m(), "\n"]
    in
      ( TextIO.output (TextIO.stdErr, msg)
      ; TextIO.flushOut TextIO.stdErr
      ; MLton.Parallel.Deprecated.releaseLock printLock
      )
    end

  (* ========================================================================
   * IDLENESS TRACKING
   *)

  val idleTotals = Array.array (P, Time.zeroTime)
  fun getIdleTime p = arraySub (idleTotals, p)
  fun updateIdleTime (p, deltaTime) =
    arrayUpdate (idleTotals, p, Time.+ (getIdleTime p, deltaTime))

  val timerGrain = 256
  fun startTimer myId = (myId, 0, Time.now ())
  fun tickTimer (p, count, t) =
    if count < timerGrain then (p, count+1, t) else
    let
      val t' = Time.now ()
      val diff = Time.- (t', t)
      val _ = updateIdleTime (p, diff)
    in
      (p, 0, t')
    end
  fun stopTimer (p, _, t) =
    (tickTimer (p, timerGrain, t); ())

  (*
  fun startTimer _ = ()
  fun tickTimer _ = ()
  fun stopTimer _ = ()
  *)

  (* ========================================================================
   * CHILD TASK PROTOTYPE THREAD
   *
   * this widget makes it possible to create new "user" threads by copying
   * the prototype thread, which immediately pulls a task out of the
   * current worker's task-box and then executes it.
   *)

  local
    val amOriginal = ref true
    val taskBoxes = Array.array (P, NONE)
    fun upd i x = MLton.HM.arrayUpdateNoBarrier (taskBoxes, Int64.fromInt i, x)
  in
  val _ = Thread.copyCurrent ()
  val prototypeThread : Thread.p =
    if !amOriginal then
      (amOriginal := false; Thread.savedPre ())
    else
      case Array.sub (taskBoxes, myWorkerId ()) of
        NONE => die (fn _ => "scheduler bug: task box is empty")
      | SOME t =>
          ( upd (myWorkerId ()) NONE
          ; t () handle _ => ()
          ; die (fn _ => "scheduler bug: child task didn't exit properly")
          )
  fun setTaskBox p t =
    upd p (SOME t)
  end

  (* ========================================================================
   * SCHEDULER LOCAL DATA
   *)

  type worker_local_data =
    { queue : (unit -> unit) Queue.t
    , schedThread : Thread.t option ref
    }

  fun wldInit p : worker_local_data =
    { queue = Queue.new ()
    , schedThread = ref NONE
    }

  val workerLocalData = Vector.tabulate (P, wldInit)

  fun setQueueDepth p d =
    let
      val {queue, ...} = vectorSub (workerLocalData, p)
    in
      Queue.setDepth queue d
    end

  fun trySteal p =
    let
      val {queue, ...} = vectorSub (workerLocalData, p)
    in
      if not (Queue.pollHasWork queue) then
        NONE
      else
        Queue.tryPopTop queue
    end

  fun communicate () = ()

  fun push x =
    let
      val myId = myWorkerId ()
      val {queue, ...} = vectorSub (workerLocalData, myId)
    in
      Queue.pushBot queue x
    end

  fun clear () =
    let
      val myId = myWorkerId ()
      val {queue, ...} = vectorSub (workerLocalData, myId)
    in
      Queue.clear queue
    end

  fun popDiscard () =
    let
      val myId = myWorkerId ()
      val {queue, ...} = vectorSub (workerLocalData, myId)
    in
      case Queue.popBot queue of
          NONE => false
        | SOME _ => true
    end

  fun returnToSched () =
    let
      val myId = myWorkerId ()
      val {schedThread, ...} = vectorSub (workerLocalData, myId)
    in
      threadSwitch (Option.valOf (!schedThread))
    end

  (* ========================================================================
   * FORK JOIN
   *)

  structure ForkJoin =
  struct

    datatype 'a result =
      Finished of 'a
    | Raised of exn

    fun result f =
      Finished (f ()) handle e => Raised e

    fun extractResult r =
      case r of
        Finished x => x
      | Raised e => raise e

    val communicate = communicate
    val getIdleTime = getIdleTime

    (* Must be called from a "user" thread, which has an associated HH *)
    fun parfork thread depth (f : unit -> 'a, g : unit -> 'b) =
      let
        val rightSide = ref (NONE : ('b result * Thread.t) option)
        val incounter = ref 2
        fun g' () =
          let
            val gr = result g
            val t = Thread.current ()
          in
            rightSide := SOME (gr, t);
            if decrementHitsZero incounter then
              ( setQueueDepth (myWorkerId ()) (depth+1)
              ; threadSwitch thread
              )
            else
              returnToSched ()
          end
        val _ = push g'
        (*val cont_arr1 =  Array.array (1, SOME(f))*)
        (*val cont_arr2 =  Array.array (1, SOME(g))*)
        (*val cont_arr2 =  Array.array (1, SOME(g'))*)
        (*location?*)
        (*val _ = HH.registerCont(cont_arr1,  cont_arr2, thread)*)
        val _ = HH.setDepth (thread, depth + 1)
        (*force left heap must be after set Depth*)
        (*val _ = HH.forceLeftHeap(myWorkerId(), thread)*)
        val fr = result f
        val gr =
          if popDiscard () then
            ( HH.promoteChunks thread
            ; HH.setDepth (thread, depth)
            ; result g
            )
          else
            ( clear () (* this should be safe after popDiscard fails? *)
            ; if decrementHitsZero incounter then () else returnToSched ()
            ; case !rightSide of
                NONE => die (fn _ => "scheduler bug: join failed")
              | SOME (gr, t) =>
                  ( HH.mergeThreads (thread, t)
                  ; setQueueDepth (myWorkerId ()) depth
                  ; HH.promoteChunks thread
                  ; HH.setDepth (thread, depth)
                  ; gr
                  )
            )
      in
        (extractResult fr, extractResult gr)
      end

    fun forkGC (f : unit -> 'a, g : unit -> 'b) =
      let
        val thread = Thread.current ()
        val depth = HH.getDepth thread

        val rightSide = ref (NONE : (unit) option)
        val incounter = ref 2
        fun gcFunc() =
          let
            val _ = HH.collectThreadRoot(thread)
          in
            rightSide := SOME (());
            if decrementHitsZero incounter then
              ( setQueueDepth (myWorkerId ()) (depth+1)
              ; threadSwitch thread
              )
            else
              returnToSched ()
          end

        val cont_arr1 =  Array.array (1, SOME(f))
        val cont_arr2 =  Array.array (1, SOME(g))

        val _ = HH.registerCont(cont_arr1,  cont_arr2, thread)
        val _ = HH.setDepth (thread, depth + 1)
        (*force left heap must be after set Depth*)
        val _ = HH.forceLeftHeap(myWorkerId(), thread)
        val _ = push gcFunc
        val fr = fork(f, g)
        val start = ref(NONE : (Time.time) option)

        val gr =
          if popDiscard() then
            let
              val _ = HH.collectThreadRoot(thread)
              val _ = HH.promoteChunks thread
            in
              HH.setDepth (thread, depth)
              ; ()
            end
          else
            ( clear()
            ; if decrementHitsZero incounter then
              ()
              else
                (start := SOME(Time.now())
                ; returnToSched ()
                )
            ; case !rightSide of
                NONE => die (fn _ => "scheduler bug: GC-joinfailed")
              | SOME(a) =>
                  let
                    val _ = case (!start) of
                                SOME(t) => print (  "waited for the GC: " ^
                                                    LargeInt.toString (
                                                      Time.toMilliseconds (Time.- (Time.now(), t))
                                                    )
                                                    ^"\n"
                                                  )
                              | NONE => ()
                  in
                    ( setQueueDepth (myWorkerId ()) depth
                    ; HH.resetList (thread)
                    ; HH.promoteChunks thread
                    ; HH.setDepth (thread, depth)
                    ; ()
                    )
                end
            )
      in
        fr
      end

    and fork (f, g) =
      let
        val thread = Thread.current ()
        val depth = HH.getDepth thread
      in
        (* don't let us hit an error, just sequentialize instead *)
        if depth = 1 then
          forkGC(f, g)
        else if depth < Queue.capacity then
          parfork thread depth (f, g)
        else
          (f (), g ())
      end
  end

  (* ========================================================================
   * WORKER-LOCAL SETUP
   *
   * We maintain a distinction between
   *   - "scheduler" threads, which never are migrated between processors and
   *   are used to acquire new work when the processor becomes idle, and
   *   - "user" threads, which run user code and are migrated between processors
   *)

  fun setupSchedLoop () =
    let
      val myId = myWorkerId ()
      val myRand = SMLNJRandom.rand (0, myId)
      (*val myRand = SimpleRandom.rand myId*)
      val mySchedThread = Thread.current ()
      val {queue=myQueue, schedThread, ...} =
        vectorSub (workerLocalData, myId)
      val _ = schedThread := SOME mySchedThread

      val _ = Queue.setDepth myQueue 1
      val _ = Queue.register myQueue myId

      (* ------------------------------------------------------------------- *)

      fun randomOtherId () =
        (*let val other = SimpleRandom.boundedInt (0, P-1) myRand*)
        let val other = SMLNJRandom.randRange (0, P-2) myRand
        in if other < myId then other else other+1
        end

      fun request idleTimer =
        let
          fun loop tries it =
            if tries = P * 100 then
              (OS.Process.sleep (Time.fromNanoseconds (LargeInt.fromInt (P * 100)));
               loop 0 (tickTimer idleTimer))
            else
            let
              val friend = randomOtherId ()
            in
              case trySteal friend of
                NONE => loop (tries+1) (tickTimer idleTimer)
              | SOME (task, depth) => (task, depth, tickTimer idleTimer)
            end
        in
          loop 0 idleTimer
        end

      (* ------------------------------------------------------------------- *)

      fun acquireWork () : unit =
        let
          val idleTimer = startTimer myId
          val (task, depth, idleTimer') = request idleTimer
          val taskThread = Thread.copy prototypeThread
        in
          if depth >= 1 then () else
            die (fn _ => "scheduler bug: acquired with depth " ^ Int.toString depth ^ "\n");
          HH.setDepth (taskThread, depth+1);
          Queue.setDepth myQueue (depth+1);
          setTaskBox myId task;
          stopTimer idleTimer';
          threadSwitch taskThread;
          Queue.setDepth myQueue 1;
          acquireWork ()
        end

    in
      acquireWork
    end

  (* ========================================================================
   * INITIALIZATION
   *)

  fun sched () =
    let
      val acquireWork = setupSchedLoop ()
    in
      acquireWork ();
      die (fn _ => "scheduler bug: scheduler exited acquire-work loop")
    end
  val _ = MLton.Parallel.registerProcessorFunction sched

  val originalThread = Thread.current ()
  val _ =
    if HH.getDepth originalThread = 0 then ()
    else die (fn _ => "scheduler bug: root depth <> 0")
  val _ = HH.setDepth (originalThread, 1)

  (* implicitly attaches worker child heaps *)
  val _ = MLton.Parallel.initializeProcessors ()

  (* Copy the current thread in order to create a scheduler thread.
   * First, the `then` branch is executed by the original thread. Then we
   * switch to the fresh scheduler thread, which executes the `else` branch.
   * Finally, the scheduler switches back to the original thread, so that
   * it can continue exiting the main program. *)
  val amOriginal = ref true
  val _ = Thread.copyCurrent ()
  val _ =
    if !amOriginal then
      let
        val schedThread = Thread.copy (Thread.savedPre ())
        (* val schedHeap = HH.newHeap () *)
      in
        amOriginal := false;
        HH.setDepth (schedThread, 1);
        setQueueDepth (myWorkerId ()) 1;
        threadSwitch schedThread
      end
    else
      let
        val acquireWork = setupSchedLoop ()
      in
        threadSwitch originalThread;
        setQueueDepth (myWorkerId ()) 1;
        acquireWork ();
        die (fn _ => "scheduler bug: scheduler exited acquire-work loop")
      end

end

structure ForkJoin :> FORK_JOIN =
struct
  open Scheduler.ForkJoin

  val par = fork

  fun for (i, j) f = if i >= j then () else (f i; for (i+1, j) f)

  fun parfor grain (i, j) f =
    if j - i <= grain then
      for (i, j) f
    else
      let
        val mid = i + (j-i) div 2
      in
        par (fn _ => parfor grain (i, mid) f,
             fn _ => parfor grain (mid, j) f)
        ; ()
      end

  fun alloc n =
    let
      val a = ArrayExtra.Raw.alloc n
      val _ =
        if ArrayExtra.Raw.uninitIsNop a then ()
        else parfor 10000 (0, n) (fn i => ArrayExtra.Raw.unsafeUninit (a, i))
    in
      ArrayExtra.Raw.unsafeToArray a
    end
end

(* Author: Sam Westrick (swestric@cs.cmu.edu) *)

(* Scheduler implements a single structure.
 *   ForkJoin : FORK_JOIN
 * It is pulled out of Scheduler at the bottom of this file. *)
structure Scheduler =
struct

  fun arraySub (a, i) = Array.sub (a, i)
  fun arrayUpdate (a, i, x) = Array.update (a, i, x)
  fun vectorSub (v, i) = Vector.sub (v, i)

  val P = MLton.Parallel.numberOfProcessors
  val myWorkerId = MLton.Parallel.processorNumber

  fun die strfn =
    ( print (Int.toString (myWorkerId ()) ^ ": " ^ strfn ())
    ; OS.Process.exit OS.Process.failure
    )

  fun search key args =
    case args of
      [] => NONE
    | x :: args' =>
        if key = x
        then SOME args'
        else search key args'

  fun parseFlag key =
    case search ("--" ^ key) (CommandLine.arguments ()) of
      NONE => false
    | SOME _ => true

  fun parseInt key default =
    case search ("-" ^ key) (CommandLine.arguments ()) of
      NONE => default
    | SOME [] => die (fn _ => "Missing argument of \"-" ^ key ^ "\" ")
    | SOME (s :: _) =>
        case Int.fromString s of
          NONE => die (fn _ => "Cannot parse integer from \"-" ^ key ^ " " ^ s ^ "\"")
        | SOME x => x

  val activatePar = parseFlag "activate-par"
  val heartbeatInterval = parseInt "heartbeat-interval" 10

  structure Queue = DequeABP (*ArrayQueue*)

  structure Thread = MLton.Thread.Basic
  fun threadSwitch t =
    ( Thread.atomicBegin ()
    ; Thread.switchTo t
    )

  structure HM = MLton.HM
  structure HH = MLton.Thread.HierarchicalHeap
  type hh_address = Word64.word
  type gctask_data = Thread.t * (hh_address ref)

  structure DE = MLton.Thread.Disentanglement
  (** See MAX_FORK_DEPTH in runtime/gc/decheck.c *)
  val maxDisetanglementCheckDepth = 31

  val internalGCThresh = Real.toInt IEEEReal.TO_POSINF
                          ((Math.log10(Real.fromInt P)) / (Math.log10 (2.0)))

  (* val vcas = MLton.Parallel.arrayCompareAndSwap *)
  (* fun cas (a, i) (old, new) = (vcas (a, i) (old, new) = old) *)
  fun faa (r, d) = MLton.Parallel.fetchAndAdd r d
  fun casRef r (old, new) =
    (MLton.Parallel.compareAndSwap r (old, new) = old)

  fun decrementHitsZero (x : int ref) : bool =
    faa (x, ~1) = 1


  datatype gc_joinpoint =
    GCJ of {gcTaskData: gctask_data option}
    (** The fact that the gcTaskData is an option here is a questionable
      * hack... the data will always be SOME. But unwrapping it may affect
      * how many allocations occur when spawning a gc task, which in turn
      * affects the GC snapshot, which is already murky.
      *)

  datatype 'a joinpoint_data =
    JD of
      { rightSideThread: Thread.t option ref
      , rightSideResult: 'a Result.t option ref
      , incounter: int ref
      , tidRight: Word64.word
      , gcj: gc_joinpoint option
      }

  datatype 'a joinpoint =
    J of {data: 'a joinpoint_data option, func: unit -> 'a}


  (* ========================================================================
   * DEBUGGING
   *)

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

  (* fun dbgmsg' m =
    let
      val p = myWorkerId ()
      val _ = MLton.Parallel.Deprecated.takeLock printLock
      val msg = String.concat ["[", Int.toString p, "] ", m(), "\n"]
    in
      ( TextIO.output (TextIO.stdErr, msg)
      ; TextIO.flushOut TextIO.stdErr
      ; MLton.Parallel.Deprecated.releaseLock printLock
      )
    end *)

  fun dbgmsg' _ = ()


  (* ========================================================================
   * Activators and activator stacks
   *)

  datatype activation_stack =
    AStack of {stack: (unit -> unit) Stack.t, pushCounter: int ref}

  fun maybeActivateOne s =
    case Stack.popOldest s of
      SOME a => a ()
    | NONE => ()

  fun astackNew () =
    AStack {stack = Stack.new (), pushCounter = ref 0}

  val astacks: activation_stack array =
    Array.tabulate (P, fn _ => astackNew ())

  fun astackSetCurrent astack =
    Array.update (astacks, myWorkerId (), astack)

  fun astackSetCurrentNew () =
    Array.update (astacks, myWorkerId (), astackNew ())

  fun astackGetCurrent () =
    Array.sub (astacks, myWorkerId ())

  fun astackPush x =
    let
      val AStack {stack, pushCounter} = astackGetCurrent ()
      val c = !pushCounter
    in
      Stack.push (x, stack);

      if c < heartbeatInterval then
        pushCounter := c + 1
      else
        (maybeActivateOne stack; pushCounter := 0)
    end

  fun astackPop () =
    let
      val AStack {stack, ...} = astackGetCurrent ()
    in
      Stack.pop stack
    end


  val _ = (_export "CheckActivationStack": (unit -> Int64.int) -> unit;)
    (fn () =>
      let
        val AStack {stack, ...} = astackGetCurrent ()
      in
        Int64.fromInt (Stack.currentSize stack)
      end)



  structure Activator :>
  sig
    type 'a t
    datatype 'a status = Pending | Activated of 'a joinpoint
    val make: (unit -> 'a joinpoint) -> 'a t
    val cancel: 'a t -> 'a status
  end =
  struct
    datatype 'a status = Pending | Activated of 'a joinpoint
    datatype 'a t = T of 'a status ref

    fun make doSpawn =
      let
        val status = ref Pending

        fun activate () =
          case !status of
            Pending => status := Activated (doSpawn ())
          | _ => die (fn _ => "multiple activate")
      in
        astackPush activate;
        T status
      end

    fun cancel (T status) =
      ( astackPop ()
      ; !status
      )
  end


  (* ========================================================================
   * TASKS
   *)

  datatype task =
    NormalTask of unit -> unit
  | Continuation of Thread.t * activation_stack * int
  | GCTask of gctask_data

  (* ========================================================================
   * IDLENESS TRACKING
   *)

  val idleTotals = Array.array (P, Time.zeroTime)
  fun getIdleTime p = arraySub (idleTotals, p)
  fun updateIdleTime (p, deltaTime) =
    arrayUpdate (idleTotals, p, Time.+ (getIdleTime p, deltaTime))

(*
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
*)

  fun startTimer _ = ()
  fun tickTimer _ = ()
  fun stopTimer _ = ()

  (** ========================================================================
    * MAXIMUM FORK DEPTHS
    *)

  val maxForkDepths = Array.array (P, 0)

  fun maxForkDepthSoFar () =
    Array.foldl Int.max 0 maxForkDepths

  fun recordForkDepth d =
    let
      val p = myWorkerId ()
    in
      if arraySub (maxForkDepths, p) >= d then
        ()
      else
        arrayUpdate (maxForkDepths, p, d)
    end

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
    fun upd i x = HM.arrayUpdateNoBarrier (taskBoxes, Int64.fromInt i, x)
    fun sub i = HM.arraySubNoBarrier (taskBoxes, Int64.fromInt i)
  in
  val _ = Thread.copyCurrent ()
  val prototypeThread : Thread.p =
    if !amOriginal then
      (amOriginal := false; Thread.savedPre ())
    else
      case sub (myWorkerId ()) of
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
    { queue : task Queue.t
    , schedThread : Thread.t option ref
    , gcTask: gctask_data option ref
    }

  fun wldInit p : worker_local_data =
    { queue = Queue.new ()
    , schedThread = ref NONE
    , gcTask = ref NONE
    }

  val workerLocalData = Vector.tabulate (P, wldInit)

  fun setGCTask p data =
    #gcTask (vectorSub (workerLocalData, p)) := data

  fun getGCTask p =
    ! (#gcTask (vectorSub (workerLocalData, p)))

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
      threadSwitch (Option.valOf (HM.refDerefNoBarrier schedThread))
    end

  (* ========================================================================
   * FORK JOIN
   *)

  structure ForkJoin =
  struct

    val communicate = communicate
    val getIdleTime = getIdleTime

    val maxPermittedCCDepth = 3

    fun spawnGC () : gc_joinpoint option =
      let
        val thread = Thread.current ()
        val depth = HH.getDepth thread
      in
        if depth > maxPermittedCCDepth then
          NONE
        else
          let
            val heapId = ref (HH.getRoot thread)
            val gcTaskTuple = (thread, heapId)
            val gcTaskData = SOME gcTaskTuple
            val gcTask = GCTask gcTaskTuple
            val cont_arr1 = ref NONE
            val cont_arr2 = ref NONE
            val cont_arr3 = ref (SOME (fn _ => (gcTask, gcTaskData))) (* a hack, I hope it works. *)

            (** The above could trigger a local GC and invalidate the hh
              * identifier... :'(
              *)
            val _ = heapId := HH.getRoot thread
          in
            if not (HH.registerCont (cont_arr1, cont_arr2, cont_arr3, thread)) then
              NONE
            else
              let
                val _ = push gcTask
                val _ = HH.setDepth (thread, depth + 1)
                val _ = HH.forceLeftHeap(myWorkerId(), thread)
              in
                SOME (GCJ {gcTaskData = gcTaskData})
              end
          end
      end


    fun syncGC (GCJ {gcTaskData}) =
      let
        val thread = Thread.current ()
        val depth = HH.getDepth thread
        val newDepth = depth-1
      in
        if popDiscard() then
          ( setGCTask (myWorkerId ()) gcTaskData (* This communicates with the scheduler thread *)
          ; push (Continuation (thread, astackGetCurrent (), newDepth))
          ; returnToSched ()
          )
        else
          ( clear()
          ; setQueueDepth (myWorkerId ()) newDepth
          );

        HH.promoteChunks thread;
        HH.setDepth (thread, newDepth)
      end


    fun spawn (g: unit -> 'b) : 'b joinpoint =
      let
        val depth = HH.getDepth (Thread.current ())
      in
        if depth >= Queue.capacity orelse depth >= maxDisetanglementCheckDepth
        then
          J {data = NONE, func = g}
        else

        let
          val gcj = spawnGC ()

          val thread = Thread.current ()
          val astack = astackGetCurrent ()
          val depth = HH.getDepth thread

          val rightSideThread = ref (NONE: Thread.t option)
          val rightSideResult = ref (NONE: 'b Result.t option)
          val incounter = ref 2

          val (tidLeft, tidRight) = DE.decheckFork ()

          fun g' () =
            let
              val () = astackSetCurrentNew ()
              val () = DE.copySyncDepthsFromThread (thread, depth+1)
              val () = DE.decheckSetTid tidRight
              val gr = Result.result g
              val t = Thread.current ()
            in
              rightSideThread := SOME t;
              rightSideResult := SOME gr;
              if decrementHitsZero incounter then
                ( setQueueDepth (myWorkerId ()) (depth+1)
                ; astackSetCurrent astack
                ; threadSwitch thread
                )
              else
                returnToSched ()
            end
          val _ = push (NormalTask g')
          val _ = HH.setDepth (thread, depth + 1)

          (* NOTE: off-by-one on purpose. Runtime depths start at 1. *)
          val _ = recordForkDepth depth

          val _ = DE.decheckSetTid tidLeft
        in
          J { func = g
            , data = SOME (JD
                { rightSideThread = rightSideThread
                , rightSideResult = rightSideResult
                , incounter = incounter
                , tidRight = tidRight
                , gcj = gcj
                })
            }
        end
      end


    fun sync (J {data, func=g}) =
      case data of
        NONE => Result.result g
      | SOME (JD {rightSideThread, rightSideResult, incounter, tidRight, gcj}) =>
          let
            val thread = Thread.current ()
            val depth = HH.getDepth thread
            val newDepth = depth-1
            val tidLeft = DE.decheckGetTid thread

            val result =
              if popDiscard () then
                ( HH.promoteChunks thread
                ; HH.setDepth (thread, newDepth)
                ; DE.decheckJoin (tidLeft, tidRight)
                (* ; HH.forceNewChunk () *)
                ; let
                    val gr = Result.result g
                  in
                    (* (gr, DE.decheckGetTid thread) *)
                    gr
                  end
                )
              else
                ( clear () (* this should be safe after popDiscard fails? *)
                ; if decrementHitsZero incounter then () else returnToSched ()
                ; case HM.refDerefNoBarrier rightSideThread of
                    NONE => die (fn _ => "scheduler bug: join failed")
                  | SOME t =>
                      let
                        val tidRight = DE.decheckGetTid t
                      in
                        HH.mergeThreads (thread, t);
                        HH.promoteChunks thread;
                        HH.setDepth (thread, newDepth);
                        DE.decheckJoin (tidLeft, tidRight);
                        setQueueDepth (myWorkerId ()) newDepth;
                        case HM.refDerefNoBarrier rightSideResult of
                          NONE => die (fn _ => "scheduler bug: join failed: missing result")
                        | SOME gr => gr
                      end
                )
          in
            case gcj of
              NONE => ()
            | SOME gcj => syncGC gcj;

            result
          end


    fun simplefork (f, g) =
      let
        (** This code is a bit deceiving in the sense that spawn and sync, as
          * defined here, are not as general as they might seem. This code is
          * only correct because each spawn is paired with exactly one sync,
          * in a nested fashion (for every spawn, any spawn after it on the
          * same thread must be sync'ed before the original spawn is sync'ed).
          *
          * Deviating from this will cause terrible things to happen.
          *)
        val j = spawn g
        val fr = Result.result f
        val gr = sync j
      in
        (Result.extractResult fr, Result.extractResult gr)
      end


    fun contBasedFork (f: unit -> 'a, g: unit -> 'b) =
      let
        val cont: ('a Result.t -> ('a * 'b)) ref =
          ref (fn fr => (Result.extractResult fr, g()))

        fun activate () =
          let
            val j = spawn g
          in
            cont := (fn fr =>
              let
                val gr = sync j
              in
                (Result.extractResult fr, Result.extractResult gr)
              end)
          end

        val _ = if activatePar then activate () else ()
        val fr = Result.result f
      in
        (!cont) fr
      end


    fun activatorBasedFork (f: unit -> 'a, g: unit -> 'b) =
      let
        val x = Activator.make (fn _ => spawn g)
        val fr = Result.result f
      in
        case Activator.cancel x of
          Activator.Pending =>
            (Result.extractResult fr, g ())

        | Activator.Activated j =>
            let val gr = sync j
            in (Result.extractResult fr, Result.extractResult gr)
            end
      end


    fun fork (f, g) =
      (* contBasedFork (f, g) *)
      activatorBasedFork (f, g)

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
      val mySchedThread = Thread.current ()
      val _ = HH.setDepth (mySchedThread, 1)
      val _ = HH.setMinLocalCollectionDepth (mySchedThread, 2)

      val myId = myWorkerId ()
      val myRand = SMLNJRandom.rand (0, myId)
      (*val myRand = SimpleRandom.rand myId*)
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

      fun afterReturnToSched () =
        case getGCTask myId of
          NONE => (*dbgmsg' (fn _ => "back in sched; no GC task")*) ()
        | SOME (thread, hh) =>
            ( (*dbgmsg' (fn _ => "back in sched; found GC task")
            ;*) setGCTask myId NONE
            ; HH.collectThreadRoot (thread, !hh)
            ; if popDiscard () then
                ( (*dbgmsg' (fn _ => "resume task thread")
                ;*) threadSwitch thread
                ; afterReturnToSched ()
                )
              else
                ()
            )


      fun acquireWork () : unit =
        let
          val idleTimer = startTimer myId
          val (task, depth, idleTimer') = request idleTimer
          val _ = stopTimer idleTimer'
        in
          case task of
            GCTask (thread, hh) =>
              ( HH.collectThreadRoot (thread, !hh)
              ; acquireWork ()
              )
          | Continuation (thread, astack, depth) =>
              ( (*dbgmsg' (fn _ => "stole continuation (" ^ Int.toString depth ^ ")")
              ; dbgmsg' (fn _ => "resume task thread")
              ;*) Queue.setDepth myQueue depth
              ; astackSetCurrent astack
              ; threadSwitch thread
              ; afterReturnToSched ()
              ; Queue.setDepth myQueue 1
              ; acquireWork ()
              )
          | NormalTask t =>
              let
                val taskThread = Thread.copy prototypeThread
              in
                if depth >= 1 then () else
                  die (fn _ => "scheduler bug: acquired with depth " ^ Int.toString depth ^ "\n");
                Queue.setDepth myQueue (depth+1);
                HH.moveNewThreadToDepth (taskThread, depth);
                HH.setDepth (taskThread, depth+1);
                setTaskBox myId t;
                (* dbgmsg' (fn _ => "switch to new task thread"); *)
                threadSwitch taskThread;
                afterReturnToSched ();
                Queue.setDepth myQueue 1;
                acquireWork ()
              end
        end

    in
      (afterReturnToSched, acquireWork)
    end

  (* ========================================================================
   * INITIALIZATION
   *)

  fun sched () =
    let
      val (_, acquireWork) = setupSchedLoop ()
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
        setQueueDepth (myWorkerId ()) 1;
        threadSwitch schedThread
      end
    else
      let
        val (afterReturnToSched, acquireWork) = setupSchedLoop ()
      in
        threadSwitch originalThread;
        afterReturnToSched ();
        setQueueDepth (myWorkerId ()) 1;
        acquireWork ();
        die (fn _ => "scheduler bug: scheduler exited acquire-work loop")
      end

end

structure ForkJoin :> FORK_JOIN =
struct
  open Scheduler.ForkJoin

  fun par (f, g) =
    (* Primitive.MPL.ForkJoin.parWrapper (fork, f, g) *)
    fork (f, g)

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

  val maxForkDepthSoFar = Scheduler.maxForkDepthSoFar
end

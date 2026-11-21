(* Author: Sam Westrick (swestric@cs.cmu.edu)
 * Adapted by Colin McDonald (colinmcd@cs.cmu.edu)
 *)

structure Scheduler =
struct

  fun arraySub (a, i) = Array.sub (a, i)
  fun arrayUpdate (a, i, x) = Array.update (a, i, x)
  fun vectorSub (v, i) = Vector.sub (v, i)

  val maxCCDepth = MPL.GC.getControlMaxCCDepth ()
  val P = MLton.Parallel.numberOfProcessors
  val myWorkerId = MLton.Parallel.processorNumber

  fun die strfn =
    ( print (Int.toString (myWorkerId ()) ^ ": " ^ strfn () ^ "\n")
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

  type gcstate = MLton.Pointer.t
  val gcstate = _prim "GC_state": unit -> gcstate;

  datatype TokenPolicy =
      TokenPolicyFair (* 0w0 *)
    | TokenPolicyKeep (* 0w1 *)
    | TokenPolicyGive (* 0w2 *)

  val traceSchedIdleEnter = _import "GC_Trace_schedIdleEnter" private: gcstate -> unit; o gcstate
  val traceSchedIdleLeave = _import "GC_Trace_schedIdleLeave" private: gcstate -> unit; o gcstate
  val traceSchedWorkEnter = _import "GC_Trace_schedWorkEnter" private: gcstate -> unit; o gcstate
  val traceSchedWorkLeave = _import "GC_Trace_schedWorkLeave" private: gcstate -> unit; o gcstate
  val traceSchedSleepEnter = _import "GC_Trace_schedSleepEnter" private: gcstate -> unit; o gcstate
  val traceSchedSleepLeave = _import "GC_Trace_schedSleepLeave" private: gcstate -> unit; o gcstate
  val traceSchedSpawn = _import "GC_Trace_schedSpawn" private: gcstate -> unit; o gcstate
  val traceSchedJoin = _import "GC_Trace_schedJoin" private: gcstate -> unit; o gcstate
  val traceSchedJoinFast = _import "GC_Trace_schedJoinFast" private: gcstate -> unit; o gcstate

  structure Queue = DequeABP (*ArrayQueue*)
  structure Thread = MLton.Thread.Basic

  val nextPromotionTokenPolicy =
    _import "GC_HH_getNextPromotionTokenPolicy" runtime private: gcstate * Thread.t -> Word32.word;
  val nextPromotionTokenPolicy =
    (fn __inline_always__ thread => case nextPromotionTokenPolicy (gcstate (), thread) of
                  0w0 => TokenPolicyFair
                | 0w1 => TokenPolicyKeep
                | 0w2 => TokenPolicyGive
                | w => die (fn _ => "Unknown token policy " ^ Word32.toString w))
  
  val primSporkFair' =
      _prim "spork_fair"
        : ('aa -> 'ar)		(* body     *)
        * 'aa			(* body arg *)
        * ('ba * 'd -> 'br)	(* spwn     *)
        * 'ba			(* spwn arg *)
        * ('ar -> 'c)		(* seq      *)
        * ('ar * 'd -> 'c)	(* sync     *)
        * (exn -> 'c)		(* exn seq  *)
        * (exn * 'd -> 'c)	(* exn sync *)
        -> 'c;
  val primSporkKeep' =
      _prim "spork_keep"
        : ('aa -> 'ar)		(* body     *)
        * 'aa			(* body arg *)
        * ('ba * 'd -> 'br)	(* spwn     *)
        * 'ba			(* spwn arg *)
        * ('ar -> 'c)		(* seq      *)
        * ('ar * 'd -> 'c)	(* sync     *)
        * (exn -> 'c)		(* exn seq  *)
        * (exn * 'd -> 'c)	(* exn sync *)
        -> 'c;
  val primSporkGive' =
      _prim "spork_give"
        : ('aa -> 'ar)		(* body     *)
        * 'aa			(* body arg *)
        * ('ba * 'd -> 'br)	(* spwn     *)
        * 'ba			(* spwn arg *)
        * ('ar -> 'c)		(* seq      *)
        * ('ar * 'd -> 'c)	(* sync     *)
        * (exn -> 'c)		(* exn seq  *)
        * (exn * 'd -> 'c)	(* exn sync *)
        -> 'c;
  fun __inline_always__ primSporkFair (body, spwn, seq, sync, exnseq, exnsync) =
      __inline_always__ primSporkFair' (body, (), spwn, (), seq, sync, exnseq, exnsync)
  fun __inline_always__ primSporkKeep (body, spwn, seq, sync, exnseq, exnsync) =
      __inline_always__ primSporkKeep' (body, (), spwn, (), seq, sync, exnseq, exnsync)
  fun __inline_always__ primSporkGive (body, spwn, seq, sync, exnseq, exnsync) =
      __inline_always__ primSporkGive' (body, (), spwn, (), seq, sync, exnseq, exnsync)
  
  val primForkThreadAndSetData = _prim "spork_forkThreadAndSetData": Thread.t * 'a -> Thread.p;
  val primForkThreadAndSetData_youngest = _prim "spork_forkThreadAndSetData_youngest": Thread.t * 'a -> Thread.p;

  val findNextPromotableFrame =
    _import "GC_HH_findNextPromotableFrame" runtime private: gcstate * bool * Thread.t -> bool;
  val findNextPromotableFrame =
      (fn __inline_always__ ({youngestOptimization}, p) =>
          findNextPromotableFrame (gcstate (), youngestOptimization, p))
      : {youngestOptimization: bool} * Thread.t -> bool;

  fun assertAtomic msg x =
    let
      val ass = Word32.toInt (Thread.atomicState ())
    in
      if ass = x then ()
      else die (fn _ => "scheduler bug: " ^ msg ^ ": atomic " ^ Int.toString ass ^ " but expected " ^ Int.toString x)
    end

  val threadSwitchEndAtomic = Thread.switchTo

  structure HM = MLton.HM
  structure HH = MLton.Thread.HierarchicalHeap
  type hh_address = Word64.word

  structure DE = MLton.Thread.Disentanglement

  local
    (** See MAX_FORK_DEPTH in runtime/gc/decheck.c *)
    val maxDisetanglementCheckDepth = DE.decheckMaxDepth ()
  in
  fun depthOkayForDECheck depth =
    case maxDisetanglementCheckDepth of
      (* in this case, there is no entanglement detection, so no problem *)
      NONE => true

      (* entanglement checks are active, and the max depth is m *)
    | SOME m => depth < m
  end


  fun faa (r, d) = MLton.Parallel.fetchAndAdd r d
  fun casRef r (old, new) =
    (MLton.Parallel.compareAndSwap r (old, new) = old)
  fun decrementHitsZero (x : int ref) : bool =
    faa (x, ~1) = 1

  datatype 'a joinpoint =
    J of
      { leftSideThread: Thread.t
      , rightSideThread: Thread.t option ref
      , rightSideResult: 'a Result.t option ref
      , incounter: int ref
      , tidRight: Word64.word
      , spareHeartbeatsGiven: Heartbeat.token_count
      , tokenPolicy: TokenPolicy
      }


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

  fun dbgmsg' m =
    let
      val p = myWorkerId ()
      (* val _ = MLton.Parallel.Deprecated.takeLock printLock *)
      val msg = String.concat ["[", Int.toString p, "] ", m(), "\n"]
    in
      ( TextIO.output (TextIO.stdErr, msg)
      ; TextIO.flushOut TextIO.stdErr
      (* ; MLton.Parallel.Deprecated.releaseLock printLock *)
      )
    end

  fun dbgmsg' _ = ()


  fun dbgmsg''' m =
    let
      val p = myWorkerId ()
      (* val _ = MLton.Parallel.Deprecated.takeLock printLock *)
      val msg = String.concat ["[", Int.toString p, "] ", m(), "\n"]
    in
      ( TextIO.output (TextIO.stdErr, msg)
      ; TextIO.flushOut TextIO.stdErr
      (* ; MLton.Parallel.Deprecated.releaseLock printLock *)
      )
    end

  fun dbgmsg'' _ = ()
  (* fun dbgmsg'' m = dbgmsg''' m *)

  fun assertTokenInvariants thread msg =
    let
      val depth = HH.getDepth (Thread.current ())
      val notOkay =
        depth < Queue.capacity
        andalso depthOkayForDECheck depth
        andalso Heartbeat.enoughToSpawn ()
        andalso HH.canForkThread thread
    in
      if notOkay then
        die (fn _ => "scheduler bug: " ^ msg ^ ": assertTokenInvariants: thread at depth " ^ Int.toString depth ^ " can fork but has tokens")
      else
        ()
    end

  (* ========================================================================
   * TASKS
   *)

  (* In the case of NormalTask and NewThread, the Word64 is the decheck id that
   * we should use for the chunks allocated for these tasks.
   *)
  datatype task =
    NormalTask of (unit -> unit) * Word64.word * int
  | NewThread of Thread.p * Word64.word * int
  | Continuation of Thread.t
    (* A thread that was suspended for a CC of its heap. Its depth is read
     * from the thread when it is resumed. *)

  (* A registered CC, handed from a task thread to its worker's scheduler
   * thread: the suspended thread, the heap to collect, and the task that
   * the scheduler thread pushes to offer the suspended thread up for steal.
   *
   * The task is allocated here, by the thread it belongs to (in the heap that
   * is about to be collected, where the collection keeps it alive, and
   * nothing frees it before the thread runs again). The scheduler thread
   * must not allocate it: a thief reads the task after it has left the
   * deque, and by then nothing would stop the scheduler thread's own local
   * collections from moving or freeing an object in the scheduler's heap. *)
  type gctask_data = Thread.t * (hh_address ref) * task option

  (* ========================================================================
   * STATS
   *)

  val numSpawns = Array.array (P, 0)
  val numEagerSpawns = Array.array (P, 0)
  val numHeartbeats = Array.array (P, 0)
  val numSkippedHeartbeats = Array.array (P, 0)
  val numSteals = Array.array (P, 0)
  val numSlowJoins = Array.array (P, 0)
  val numFastJoins = Array.array (P, 0)

  fun incrementNumSpawns () =
    let
      val p = myWorkerId ()
      val c = arraySub (numSpawns, p)
    in
      arrayUpdate (numSpawns, p, c+1)
    end

  fun addEagerSpawns d =
    let
      val p = myWorkerId ()
      val c = arraySub (numEagerSpawns, p)
    in
      arrayUpdate (numEagerSpawns, p, c+d)
    end

  fun incrementNumHeartbeats () =
    let
      val p = myWorkerId ()
      val c = arraySub (numHeartbeats, p)
    in
      arrayUpdate (numHeartbeats, p, c+1)
    end

  fun incrementNumSkippedHeartbeats () =
    let
      val p = myWorkerId ()
      val c = arraySub (numSkippedHeartbeats, p)
    in
      arrayUpdate (numSkippedHeartbeats, p, c+1)
    end

  fun incrementNumSteals () =
    let
      val p = myWorkerId ()
      val c = arraySub (numSteals, p)
    in
      arrayUpdate (numSteals, p, c+1)
    end

  fun incrementNumSlowJoins () =
    let
      val p = myWorkerId ()
      val c = arraySub (numSlowJoins, p)
    in
      arrayUpdate (numSlowJoins, p, c+1)
      (* MLton.Parallel.arrayFetchAndAdd (numSlowJoins, p) 1 *)
    end

  fun incrementNumFastJoins () =
    let
      val p = myWorkerId ()
      val c = arraySub (numFastJoins, p)
    in
      (* MLton.Parallel.arrayFetchAndAdd (numFastJoins, p) 1 *)
      arrayUpdate (numFastJoins, p, c+1)
    end

  fun numSpawnsSoFar () =
    Array.foldl op+ 0 numSpawns

  fun numEagerSpawnsSoFar () =
    Array.foldl op+ 0 numEagerSpawns

  fun numHeartbeatsSoFar () =
    Array.foldl op+ 0 numHeartbeats

  fun numSkippedHeartbeatsSoFar () =
    Array.foldl op+ 0 numSkippedHeartbeats

  fun numStealsSoFar () =
    Array.foldl op+ 0 numSteals

  fun numSlowJoinsSoFar () =
    Array.foldl op+ 0 numSlowJoins

  fun numFastJoinsSoFar () =
    Array.foldl op+ 0 numFastJoins

  (** ========================================================================
    * TIMERS
    *)

  structure IdleTimer = CumulativePerProcTimer(val timerName = "idle")
  structure WorkTimer = CumulativePerProcTimer(val timerName = "work")

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
        ( (*print ("max increased: " ^ Int.toString d ^ "\n")*) ()
        ; arrayUpdate (maxForkDepths, p, d)
        )
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
    fun upd i x = HM.arrayUpdateNoBarrier (taskBoxes, i, x)
    fun sub i = HM.arraySubNoBarrier (taskBoxes, i)
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

  (* gcTask hands a GC task from a worker's task thread to its scheduler
   * thread: trySpawnGC stores it, and afterReturnToSched reads and clears it.
   * workerLocalData is allocated at boot, at depth 0. For a depth-0 ref the
   * write barrier's only lasting effect is to pin the stored task with unpin
   * depth 0 and remember it, and no collection runs at depth 0 to unpin it,
   * so each task handed over this way stayed pinned and remembered for good.
   * Skip the barrier, as for taskBoxes above, and as when another worker
   * steals the task through the deque. Until the read, the task's heap has a
   * registered CC that hasn't started (afterReturnToSched starts it, after
   * the read), and local collections skip that depth and every shallower
   * one, so nothing moves or frees the task in between. *)
  fun setGCTask p data =
    HM.refAssignNoBarrier (#gcTask (vectorSub (workerLocalData, p)), data)

  fun getGCTask p =
    HM.refDerefNoBarrier (#gcTask (vectorSub (workerLocalData, p)))

  fun getSchedThread () =
    let
      val myId = myWorkerId ()
      val {schedThread, ...} = vectorSub (workerLocalData, myId)
    in
      HM.refDerefNoBarrier schedThread
    end

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
      Queue.tryPopTop queue
    end

  fun communicate () = ()

  fun queueSize () =
    let
      val myId = myWorkerId ()
      val {queue, ...} = vectorSub (workerLocalData, myId)
    in
      Queue.size queue
    end

  fun push x =
    let
      val myId = myWorkerId ()
      val {queue, ...} = vectorSub (workerLocalData, myId)
    in
      Queue.pushBot queue x
    end

  fun pushPreallocated xo =
    let
      val myId = myWorkerId ()
      val {queue, ...} = vectorSub (workerLocalData, myId)
    in
      Queue.pushBotPreallocated queue xo
    end

  fun clear () =
    let
      val myId = myWorkerId ()
      val {queue, ...} = vectorSub (workerLocalData, myId)
    in
      Queue.clear queue
    end

  fun pop () =
    let
      val myId = myWorkerId ()
      val {queue, ...} = vectorSub (workerLocalData, myId)
    in
      Queue.popBot queue
    end

  fun popDiscard () =
    case pop () of
      NONE => false
    | SOME _ => true

  fun returnToSchedEndAtomic () =
    let
      val myId = myWorkerId ()
      val {schedThread, ...} = vectorSub (workerLocalData, myId)
      val _ = dbgmsg'' (fn _ => "return to sched")
    in
      threadSwitchEndAtomic (Option.valOf (HM.refDerefNoBarrier schedThread))
    end

  (* ========================================================================
   * SPORK JOIN
   *)

  structure SporkJoin =
  struct

    fun gcTaskPending () =
      Option.isSome (getGCTask (myWorkerId ()))

    (* Try to register a concurrent collection of the current heap. Called
     * in two places, which differ in the threshold the runtime checks the
     * CC policy against (see checkPolicyforRoot):
     *   - at a par, just before spawning ({atPar = true}): the par
     *     threshold. Either in the heartbeat handler, or in an eager spawn.
     *   - in the GC signal handler ({atPar = false}): the seq threshold. The
     *     runtime raises the GC signal when the allocation clock of the
     *     interrupted thread says that its heap is due (see
     *     HM_HH_maybeRequestCC).
     * On success, the GC task is left for this worker's scheduler thread
     * (see afterReturnToSched), and the caller must finish by switching to
     * the scheduler thread, which offers interruptedThread up for steal and
     * does the collection.
     *
     * The collection is not tied to a fork: there is no join for it. The
     * runtime notices that it has finished the next time it looks at this
     * heap (mergeCompletedCCs).
     *)
    fun trySpawnGC {atPar: bool} (interruptedThread: Thread.t) : bool =
      let
        val thread = Thread.current ()
      in
        (* A GC task can already be pending here only when the heartbeat
         * handler spawns more than once in the same beat: the first spawn
         * registered a CC, and we haven't gotten back to the scheduler
         * thread yet. One CC per trip to the scheduler thread. *)
        if HH.getDepth thread > maxCCDepth orelse gcTaskPending () then
          false
        else
          let
            val heapId = ref (HH.getRoot thread)
            val gcTaskData =
              SOME (interruptedThread, heapId, SOME (Continuation interruptedThread))
            val cont_arr1 = ref NONE
            val cont_arr2 = ref NONE
            (* keeps the task data alive through the collection *)
            val cont_arr3 = ref gcTaskData

            (** The above could trigger a local GC and invalidate the hh
              * identifier... :'(
              *)
            val _ = heapId := HH.getRoot thread
          in
            HH.registerCont (cont_arr1, cont_arr2, cont_arr3, thread, {atPar = atPar})
            andalso
            ( setGCTask (myWorkerId ()) gcTaskData (* This communicates with the scheduler thread *)
            ; true
            )
          end
      end

    (* runs in signal handler *)
    fun doSpawn {youngestOptimization: bool} (interruptedLeftThread: Thread.t) : unit =
      let
        val thread = Thread.current ()
        val depth = HH.getDepth thread

        val _ = dbgmsg'' (fn _ => "spawning at depth " ^ Int.toString depth)

        (* We use a ref here instead of using rightSideThread directly.
         * The rightSideThread is a Thread.p (it doesn't have a heap yet).
         * The thief will convert it into a Thread.t and give it a heap,
         * and then write it into this slot. *)
        val rightSideThreadSlot = ref (NONE: Thread.t option)
        val rightSideResult = ref (NONE: Universal.t Result.t option)
        val incounter = ref 2

        val tidParent = DE.decheckGetTid thread
        val (tidLeft, tidRight) = DE.decheckFork ()

        val _ = Heartbeat.consumeSpare Heartbeat.spawnCost

        val tokenPolicy = nextPromotionTokenPolicy interruptedLeftThread
        val giveTokens = case tokenPolicy of
                             TokenPolicyFair => Heartbeat.halfOfCurrent ()
                           | TokenPolicyKeep => Heartbeat.zero
                           | TokenPolicyGive => Heartbeat.currentSpare ()
        val _ = Heartbeat.consumeSpare giveTokens
        (* val spareBefore = currentSpareHeartbeatTokens () *)
        (* val spareHB = ref 0w0 *)
        val jp =
          J { leftSideThread = interruptedLeftThread
            , rightSideThread = rightSideThreadSlot
            , rightSideResult = rightSideResult
            , incounter = incounter
            , tidRight = tidRight
            , spareHeartbeatsGiven = giveTokens
            , tokenPolicy = tokenPolicy
            }

        (* this sets the join for both threads (left and right) *)
        val rightSideThread =
          if youngestOptimization then
            primForkThreadAndSetData_youngest (interruptedLeftThread, jp)
          else
            primForkThreadAndSetData (interruptedLeftThread, jp)

        (* determine how many heartbeats given to rhs from difference vs before *)
        (* val _ = spareHB := spareBefore - currentSpareHeartbeatTokens () *)

        (* double check... hopefully correct, not off by one? *)
        val _ = push (NewThread (rightSideThread, tidParent, depth))
        val _ = HH.setDepth (thread, depth + 1)

        (* NOTE: off-by-one on purpose. Runtime depths start at 1. *)
        val _ = recordForkDepth depth

        val _ = incrementNumSpawns ()
        val _ = traceSchedSpawn ()

        val _ = DE.decheckSetTid tidLeft
        val _ = assertAtomic "spawn done" 1
      in
        ()
      end


    (* runs in signal handler *)
    fun maybeSpawn youngestOptimization (interruptedLeftThread: Thread.t) : bool =
      let
        val depth = HH.getDepth (Thread.current ())
      in
        if depth >= Queue.capacity orelse not (depthOkayForDECheck depth) then
          false
        else if not (findNextPromotableFrame (youngestOptimization, interruptedLeftThread)) then
          false
        else
          ( (* At a par: check the CC policy against the par threshold, on
             * the heap at the depth of the fork (before the spawn increments
             * the depth), as in the fork-time policy. If it registers a CC,
             * the caller must switch to the scheduler thread when it is done
             * spawning (see gcTaskPending). *)
            ignore (trySpawnGC {atPar = true} interruptedLeftThread)
          ; doSpawn youngestOptimization interruptedLeftThread
          ; true
          )
      end


    fun doSpawnFunc (g: unit -> 'a) : 'a joinpoint =
      let
        val _ = Thread.atomicBegin ()
        val thread = Thread.current ()
        val _ = assertTokenInvariants thread "doSpawnFunc"

        val depth = HH.getDepth thread

        val _ = dbgmsg'' (fn _ => "spawning at depth " ^ Int.toString depth)

        (* We use a ref here instead of using rightSideThread directly.
         * The rightSideThread is a Thread.p (it doesn't have a heap yet).
         * The thief will convert it into a Thread.t and give it a heap,
         * and then write it into this slot. *)
        val rightSideThreadSlot = ref (NONE: Thread.t option)
        val rightSideResult = ref (NONE: 'a Result.t option)
        val incounter = ref 2

        val tidParent = DE.decheckGetTid thread
        val (tidLeft, tidRight) = DE.decheckFork ()

        val half = Heartbeat.halfOfCurrent ()
        val _ = Heartbeat.consumeSpare half

        fun g' () =
          let
            val () = DE.copySyncDepthsFromThread (thread, Thread.current (), depth+1)
            val () = DE.decheckSetTid tidRight
            val () = HH.forceLeftHeap(myWorkerId(), Thread.current ())
            val _ = Heartbeat.addSpare half
            val _ = Thread.atomicEnd()

            val gr = Result.result g

            val _ = Thread.atomicBegin ()
            val t = Thread.current ()
          in
            rightSideThreadSlot := SOME t;
            rightSideResult := SOME gr;

            if decrementHitsZero incounter then
              ( ()
              ; setQueueDepth (myWorkerId ()) depth
                (** Atomic 1 *)
              ; Thread.atomicBegin ()

                (** Atomic 2 *)

                (** (When sibling is resumed, it needs to be atomic 1.
                  * Switching threads is implicit atomicEnd(), so we need
                  * to be at atomic2
                  *)
              ; assertAtomic "rightside switch-to-left" 2
              ; threadSwitchEndAtomic thread
              )
            else
              ( assertAtomic "rightside before returnToSched" 1
              ; returnToSchedEndAtomic ()
              )
          end

        (* double check... hopefully correct, not off by one? *)
        val _ = push (NormalTask (g', tidParent, depth))
        val _ = HH.setDepth (thread, depth + 1)

        (* NOTE: off-by-one on purpose. Runtime depths start at 1. *)
        val _ = recordForkDepth depth

        val _ = incrementNumSpawns ()
        val _ = traceSchedSpawn ()

        val _ = DE.decheckSetTid tidLeft
        val _ = assertAtomic "spawn done" 1
        val _ = Thread.atomicEnd ()
      in
        J { leftSideThread = thread
          , rightSideThread = rightSideThreadSlot
          , rightSideResult = rightSideResult
          , incounter = incounter
          , tidRight = tidRight
          , spareHeartbeatsGiven = half
          , tokenPolicy = TokenPolicyFair
          }
      end


    fun maybeSpawnFunc (g: unit -> 'a) : 'a joinpoint option =
      let
        val depth = HH.getDepth (Thread.current ())
      in
        if depth >= Queue.capacity orelse not (depthOkayForDECheck depth) then
          NONE
        else
          SOME (doSpawnFunc g)
      end


    (** Must be called in an atomic section. Implicit atomicEnd() *)
    fun syncEndAtomic
        (doClearSuspects: Thread.t * int -> unit)
        (J {rightSideThread, rightSideResult, incounter, tidRight, spareHeartbeatsGiven, tokenPolicy, ...} : 'a joinpoint)
        : 'a Result.t option
      =
      let
        val _ = assertAtomic "syncEndAtomic begin" 1

        val thread = Thread.current ()
        val depth = HH.getDepth thread
        val newDepth = depth-1
        val tidLeft = DE.decheckGetTid thread

        val result =
          (* Might seem like a space leak here, because we don't clean up the
           * thread that was spawned and added to the deque. But this is okay:
           * the thread hasn't been stolen, so it hasn't yet been converted
           * into a full thread. (The discarded thread is located in the current
           * heap, not in some other heap, so it will be garbage-collected
           * appropriately.)
           *)
          if popDiscard () then
            let val _ = dbgmsg'' (fn _ => "popDiscard success at depth " ^ Int.toString depth)
                (* promote chunks into parent, update depth->newDepth, update
                 * decheck state by joining tidLeft and tidRight.
                 *)
                val _ = HH.joinIntoParentBeforeFastClone
                          {thread=thread, newDepth=newDepth, tidLeft=tidLeft, tidRight=tidRight}
                val _ = traceSchedJoinFast ()
                (* per-worker counter: update it before the atomic section
                 * ends, while we can't be moved to another worker *)
                val _ = incrementNumFastJoins ()
                val _ = Thread.atomicEnd ()
                val _ = doClearSuspects (thread, newDepth)
                val _ = if newDepth <> 1 then () else HH.updateBytesPinnedEntangledWatermark ()
                val _ = case tokenPolicy of
                            TokenPolicyGive => Heartbeat.addSpare spareHeartbeatsGiven
                          | _ => Heartbeat.zero
            in
              NONE
            end
          else
            ( if decrementHitsZero incounter then
                ()
              else
                ( ()
                  (** Atomic 1 *)
                ; assertAtomic "syncEndAtomic before returnToSched" 1
                ; returnToSchedEndAtomic ()
                ; assertAtomic "syncEndAtomic after returnToSched" 1
                )

            ; case HM.refDerefNoBarrier rightSideThread of
                NONE => die (fn _ => "scheduler bug: join failed")
              | SOME rightSideThread =>
                  let
                    val tidRight = DE.decheckGetTid rightSideThread

                    (* merge the two threads, promote chunks into parent, 
                     * update depth->newDepth, update the decheck state
                     *)
                    val _ = HH.joinIntoParent
                      { thread = thread
                      , rightSideThread = rightSideThread
                      , newDepth = newDepth
                      , tidLeft = tidLeft
                      , tidRight = tidRight
                      }

                    val _ = incrementNumSlowJoins ()

                    val _ = traceSchedJoin ()

                    (* SAM_NOTE: TODO: we really ought to make this part of
                     * the HH.joinIntoParent call, above. Is that possible?
                     *)
                    val _ = setQueueDepth (myWorkerId ()) newDepth

                    val result = 
                      case HM.refDerefNoBarrier rightSideResult of
                        NONE => die (fn _ => "scheduler bug: join failed: missing result")
                      | SOME gr =>
                          ( ()
                          ; assertAtomic "syncEndAtomic after merge" 1
                          ; Thread.atomicEnd ()
                          ; gr
                          )
                  in
                    doClearSuspects (thread, newDepth);
                    if newDepth <> 1 then () else HH.updateBytesPinnedEntangledWatermark ();
                    SOME result
                  end
            )
      in
        result
      end

    (* ===================================================================
     * handler fn definitions
     *)

    fun heartbeatHandler (thread: Thread.t) =
      let
        (* NOTE: we can't assert the token invariants here! We can only do
         * so after the heartbeatHandler finishes. It's possible for the
         * token invariants to be briefly violated, in which case the
         * handler restores them.
         *)

        val hadEnoughToSpawnBefore = Heartbeat.enoughToSpawn ()

        val _ = Heartbeat.addSpare Heartbeat.tokensPerBeat

        fun loop i =
          if
            Heartbeat.enoughToSpawn ()
            andalso maybeSpawn {youngestOptimization = false} thread
          then
            loop (i+1)
          else
            i

        val numSpawned = loop 0

        val _ = assertTokenInvariants thread "heartbeatHandler"
        val _ =
          (* If the hearbeat handler intervenes immediately before the eager
           * check at each `par`, then there should be exactly one spawn.
           *)
          if hadEnoughToSpawnBefore andalso numSpawned > 1 then
            die (fn _ => "scheduler bug: more than one eager fork was missed")
          else
            ()
      in
        incrementNumHeartbeats ()
      end


    (* Signal handlers are given the interrupted thread, and return the
     * thread to switch to when the handler finishes.
     *)
    (* The heartbeat might spawn, and spawning checks the CC policy against
     * the par threshold (see maybeSpawn). If that registers a CC, then
     * finish by switching to our scheduler thread, as in gcHandler below. *)
    fun heartbeatSignalHandler (thread: Thread.t) : Thread.t =
      case getSchedThread () of
        NONE => thread
      | SOME schedThread =>
          if MLton.eq (thread, schedThread) then
            thread
          else
            ( heartbeatHandler thread
            ; if gcTaskPending () then schedThread else thread
            )


    (* If we manage to spawn a concurrent collection of the interrupted
     * thread's heap, then finish the handler by switching to our scheduler
     * thread instead of back to the interrupted thread. The scheduler thread
     * offers the interrupted thread up for steal, and does the collection
     * (see afterReturnToSched).
     *
     * This handler doesn't touch the tokens of the interrupted thread or
     * look at its promotable frames, so the token invariants are whatever
     * they were when the thread was interrupted.
     *)
    fun gcHandler (thread: Thread.t) : Thread.t =
      case getSchedThread () of
        NONE => thread
      | SOME schedThread =>
          if MLton.eq (thread, schedThread)
             orelse not (trySpawnGC {atPar = false} thread)
          then thread
          else schedThread

    val _ = MLton.Signal.setGCHandler
      (MLton.Signal.Handler.inspectInterrupted gcHandler)


    (** itimer is used to deliver signals regularly. sigusr1 is used to relay
      * these to all processes
      *)
    val _ =
      if P > Heartbeat.relayerThreshold then () else
        MLton.Signal.setHandler
          ( MLton.Itimer.signal MLton.Itimer.Real
          , MLton.Signal.Handler.inspectInterrupted
              heartbeatSignalHandler
          )

    val _ = MLton.Signal.setHandler
      ( Posix.Signal.usr1
      , MLton.Signal.Handler.inspectInterrupted
          heartbeatSignalHandler
      )


    (* =======================================================================
     *)


    fun simpleParFork (f: unit -> unit, g: unit -> unit) : unit =
      case maybeSpawnFunc g of
        NONE => (f (); g ())
      | SOME gj =>
          let
            val fr = Result.result f
            val _ = Thread.atomicBegin ()
            val gro = syncEndAtomic maybeParClearSuspectsAtDepth gj
          in
            Result.extractResult fr;
            case gro of
                NONE => g ()
              | SOME gr => Result.extractResult gr
          end

    and maybeParClearSuspectsAtDepth (t, d) =
      if HH.numSuspectsAtDepth (t, d) <= 10000 then
        HH.clearSuspectsAtDepth (t, d)
      else
        let
          val cs = HH.takeClearSetAtDepth (t, d)
          val count = HH.numChunksInClearSet cs
          (* val _ = print ("maybeParClearSuspectsAtDepth: " ^ Int.toString count ^ " chunks\n") *)
          val grainSize = 20
          val numGrains = 1 + (count-1) div grainSize
          val results = ArrayExtra.alloc numGrains
          fun start i = i*grainSize
          fun stop i = Int.min (grainSize + start i, count)

          fun processLoop i j =
            if j-i = 1 then
              Array.update (results, i, HH.processClearSetGrain (cs, start i, stop i))
            else
              let
                val mid = i + (j-i) div 2
              in
                simpleParFork
                  (fn () => processLoop i mid,
                   fn () => processLoop mid j)
              end

          fun commitLoop i =
            if i >= numGrains then () else
            ( HH.commitFinishedClearSetGrain (t, Array.sub (results, i))
            ; commitLoop (i+1)
            )
        in
          processLoop 0 numGrains;
          commitLoop 0;
          HH.deleteClearSet cs;
          maybeParClearSuspectsAtDepth (t, d) (* need to go again, just in case *)
        end

  
    (* After an eager spawn (at atomic 1). The spawn checked the CC policy
     * against the par threshold (see maybeSpawn). If that registered a CC,
     * offer this thread up for steal and do the collection, by switching to
     * the scheduler thread (see afterReturnToSched). Either way, we end at
     * atomic 0: the switch is an implicit atomicEnd, and whoever resumes us
     * does so from atomic 1, as for threads interrupted by a signal.
     * The thread resumes right here, at the fork point of the par. *)
    fun endAtomicAfterEagerSpawn () =
      if gcTaskPending () then
        ( assertAtomic "endAtomicAfterEagerSpawn before returnToSched" 1
        ; returnToSchedEndAtomic ()
        )
      else
        Thread.atomicEnd ()

    val sched_package_data = ref
      { syncEndAtomic = syncEndAtomic maybeParClearSuspectsAtDepth
      , endAtomicAfterEagerSpawn = endAtomicAfterEagerSpawn
      , maybeSpawn = maybeSpawn
      , setQueueDepth = setQueueDepth
      , returnToSchedEndAtomic = returnToSchedEndAtomic
      , tryConsumeSpareHeartbeats = Heartbeat.consumeSpare
      , addEagerSpawns = addEagerSpawns
      , assertAtomic = assertAtomic
      , error = (fn s => die (fn _ => s)) : string -> unit
      }

    fun sched_package () = !sched_package_data

    exception SchedulerError

    (* ===================================================================
     * spork definition
     *)

    fun __inline_always__ tryPromoteNow yo =
      ( Thread.atomicBegin ()
      ; if
          Heartbeat.enoughToSpawn () andalso
          #maybeSpawn (sched_package ()) yo (Thread.current ())
        then
          ( #addEagerSpawns (sched_package ()) 1
          ; #endAtomicAfterEagerSpawn (sched_package ()) ()
          )
        else
          Thread.atomicEnd ()
      )

    type ('a, 'c) sporkT =
           (unit -> 'a)
         * (unit * Universal.t joinpoint -> unit)
         * ('a -> 'c)
         * ('a * Universal.t joinpoint -> 'c)
         * (exn -> 'c)
         * (exn * Universal.t joinpoint -> 'c)
         -> 'c

    fun __inline_always__ sporkBase (primSpork: ('a, 'c) sporkT,
                                     body: unit -> 'a,
                                     spwn: unit -> 'b,
                                     seq: 'a -> 'c,
                                     sync: 'a * 'b -> 'c,
                                     unstolen: 'a -> 'c): 'c =
      let
        val (inject, project) = Universal.embed ()

        fun __inline_always__ body' (): 'a =
            ((if not (Heartbeat.enoughToSpawn ()) then () else tryPromoteNow {youngestOptimization = true});
             __inline_always__ body ())

        fun spwn' ((), J jp): unit =
          let
            val _ = #assertAtomic (sched_package ()) "spork rightside begin" 1
            val () = DE.decheckSetTid (#tidRight jp)

            val thread = Thread.current ()
            val depth = HH.getDepth thread
            val _ = dbgmsg'' (fn _ => "rightside begin at depth " ^ Int.toString depth)

            val _ = HH.forceLeftHeap(myWorkerId(), thread)
            val _ = Heartbeat.addSpare (#spareHeartbeatsGiven jp)
            val _ = #assertAtomic (sched_package ()) "spork rightSide before execute" 1
            val _ = Thread.atomicEnd()

            val spwnr = Result.result (inject o spwn)

            val _ = Thread.atomicBegin ()
            val depth' = HH.getDepth (Thread.current ())
            val _ =
              if depth = depth' then ()
              else #error (sched_package ()) ("scheduler bug: rightide depth mismatch: " ^ Int.toString depth ^ " vs " ^ Int.toString depth')
            val _ = dbgmsg'' (fn _ => "rightside done! at depth " ^ Int.toString depth')
            val _ = #assertAtomic (sched_package ()) "spork rightside begin synchronize" 1
          in
            #rightSideThread jp := SOME thread;
            #rightSideResult jp := SOME spwnr;

            if decrementHitsZero (#incounter jp) then
              ( ()
              ; dbgmsg'' (fn _ => "rightside synchronize: become left")
              ; #setQueueDepth (sched_package ()) (myWorkerId ()) depth
                (** Atomic 1 *)
              ; Thread.atomicBegin ()

                (** Atomic 2 *)

                (** (When sibling is resumed, it needs to be atomic 1.
                  * Switching threads is implicit atomicEnd(), so we need
                  * to be at atomic2
                  *)
              ; #assertAtomic (sched_package ()) "spork rightside switch-to-left" 2
              ; threadSwitchEndAtomic (#leftSideThread jp)
              )
            else
              ( dbgmsg'' (fn _ => "rightside synchronize: back to sched")
              ; #assertAtomic (sched_package ()) "spork rightside before returnToSched" 1
              ; #returnToSchedEndAtomic (sched_package ()) ()
              )
          end

        fun __inline_always__ seq' (bodyr: 'a): 'c =
            __inline_always__ seq bodyr

        fun __inline_always__ sync' (bodyr: 'a, jp: Universal.t joinpoint): 'c =
          let
            val _ = dbgmsg'' (fn _ => "hello from sync continuation")
            val _ = Thread.atomicBegin ()
            val _ = #assertAtomic (sched_package ()) "sync continuation" 1
            val spwnrOpt = #syncEndAtomic (sched_package ()) jp
            val bodyr' = bodyr
          in
            case spwnrOpt of
              (* spwn was unstolen *)
                NONE => unstolen bodyr'
              (* spwn was stolen and synced in syncEndAtomic *)
              | SOME spwnr =>
                case project (Result.extractResult spwnr) of
                    SOME r => sync (bodyr', r)
                  | NONE => (#error (sched_package ())
                                    "scheduler bug: spork sync: failed project right-side result";
                             raise SchedulerError)
          end

        fun __inline_always__ exnseq' (e: exn): 'c = raise e

        fun __inline_always__ exnsync' (e: exn, jp: Universal.t joinpoint): 'c =
            let val _ = dbgmsg'' (fn _ => "hello from exn sync continuation")
                val _ = Thread.atomicBegin ()
                val _ = #assertAtomic (sched_package ()) "exn sync continuation" 1
                val _ = #syncEndAtomic (sched_package ()) jp
            in
              raise e
            end
      in
        __inline_always__ primSpork (body', spwn', seq', sync', exnseq', exnsync')
      end

    fun __inline_always__ spork
                          {tokenPolicy: TokenPolicy,
                           body: unit -> 'a,
                           spwn: unit -> 'b,
                           seq: 'a -> 'c,
                           sync: 'a * 'b -> 'c,
                           unstolen: ('a -> 'c) option} =
        let val primSpork = case tokenPolicy of
                                TokenPolicyFair => primSporkFair
                              | TokenPolicyGive => primSporkGive
                              | TokenPolicyKeep => primSporkKeep
            val unstolen = case unstolen of
                               NONE => seq
                             | SOME unstolen => unstolen
        in
          sporkBase (primSpork, body, spwn, seq, sync, unstolen)
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
      val mySchedThread = Thread.current ()
      val _ = HH.setDepth (mySchedThread, 1)
      val _ = HH.setMinLocalCollectionDepth (mySchedThread, 1)

      val myId = myWorkerId ()
      val myRand = SimpleRandom.rand myId
      val {queue=myQueue, schedThread, ...} =
        vectorSub (workerLocalData, myId)
      val _ = schedThread := SOME mySchedThread

      val _ = Queue.setDepth myQueue 1
      val _ = Queue.register myQueue myId

      (* ------------------------------------------------------------------- *)

      fun randomOtherId () =
        let val other = SimpleRandom.boundedInt (0, P-1) myRand
        in if other < myId then other else other+1
        end

      fun stealLoop () =
        let
          fun loop tries =
            if tries = P * 100 then
              ( MLton.GC.collect ()
              ; IdleTimer.tick ()
              ; traceSchedSleepEnter ()
              ; OS.Process.sleep (Time.fromNanoseconds (LargeInt.fromInt (P * 100)))
              ; traceSchedSleepLeave ()
              ; loop 0 )
            else
            let
              val friend = randomOtherId ()
            in
              case trySteal friend of
                NONE => loop (tries+1)
              | SOME task => task
            end

          val result = loop 0
        in
          result
        end

      (* ------------------------------------------------------------------- *)

      fun afterReturnToSched () =
        case getGCTask myId of
          NONE => ( dbgmsg'' (fn _ => "back in sched; no GC task"); () )
        | SOME (thread, hh, cont) =>
            ( dbgmsg'' (fn _ => "back in sched; found GC task")
            ; setGCTask myId NONE
              (* The task thread was suspended either by a signal handler
               * (at an arbitrary GC_collect call) or by an eager spawn (at
               * the fork point of a par), and can be resumed by any worker:
               * let someone else pick it up while we are busy collecting. *)
            ; pushPreallocated cont
            ; traceSchedIdleLeave ()
            ; traceSchedWorkEnter ()
            ; IdleTimer.stop ()
            ; WorkTimer.start ()
            ; HH.collectThreadRoot (thread, !hh)
            (* ; print ("afterReturnToSched: done with GC\n") *)
            ; case pop () of
                NONE =>
                  ( WorkTimer.stop ()
                  ; IdleTimer.start ()
                  ; traceSchedWorkLeave ()
                  ; traceSchedIdleEnter ()
                  )
              | SOME (Continuation thread) =>
                  ( ()
                  ; dbgmsg'' (fn _ => "resume task thread")
                    (* Continuations are threads that were suspended for a
                     * CC, and expect to resume at atomic 0. *)
                  ; Thread.atomicBegin ()
                  ; assertAtomic "afterReturnToSched before thread switch" 1
                  ; threadSwitchEndAtomic thread
                  ; WorkTimer.stop ()
                  ; IdleTimer.start ()
                  ; traceSchedWorkLeave ()
                  ; traceSchedIdleEnter ()
                  ; afterReturnToSched ()
                  )
              | SOME _ =>
                  die (fn _ => "bug: Scheduler.afterReturnToSched: impossible")
            )

      fun acquireWork () : unit =
        let
          val task = stealLoop ()
          val _ = incrementNumSteals ()
        in
          case task of
            Continuation thread =>
              let
                (* the thread is suspended, so its depth is stable *)
                val depth = HH.getDepth thread
              in
              ( ()
              ; dbgmsg'' (fn _ => "stole continuation (" ^ Int.toString depth ^ ")")
              (* ; dbgmsg' (fn _ => "resume task thread") *)
              ; Queue.setDepth myQueue depth
              ; traceSchedIdleLeave ()
              ; traceSchedWorkEnter ()
              ; IdleTimer.stop ()
              ; WorkTimer.start ()
              ; Thread.atomicBegin ()
              ; assertAtomic "acquireWork before thread switch" 1
              ; threadSwitchEndAtomic thread
              ; WorkTimer.stop ()
              ; IdleTimer.start ()
              ; traceSchedWorkLeave ()
              ; traceSchedIdleEnter ()
              ; afterReturnToSched ()
              ; Queue.setDepth myQueue 1
              ; acquireWork ()
              )
              end
          | NormalTask (taskFn, tidParent, depth) =>
              let
                val taskThread = Thread.copy prototypeThread
              in
                if depth >= 1 then () else
                  die (fn _ => "scheduler bug: acquired with depth " ^ Int.toString depth);
                Queue.setDepth myQueue (depth+1);
                HH.moveNewThreadToDepth (taskThread, tidParent, depth);
                HH.setDepth (taskThread, depth+1);
                setTaskBox myId taskFn;
                traceSchedIdleLeave ();
                traceSchedWorkEnter ();
                IdleTimer.stop ();
                WorkTimer.start ();
                Thread.atomicBegin ();
                Thread.atomicBegin ();
                assertAtomic "acquireWork before thread switch" 2;
                threadSwitchEndAtomic taskThread;
                WorkTimer.stop ();
                IdleTimer.start ();
                traceSchedWorkLeave ();
                traceSchedIdleEnter ();
                afterReturnToSched ();
                Queue.setDepth myQueue 1;
                acquireWork ()
              end
          | NewThread (thread, tidParent, depth) =>
              let
                val taskThread = Thread.copy thread
              in
                if depth >= 1 then () else
                  die (fn _ => "scheduler bug: acquired with depth " ^ Int.toString depth);
                Queue.setDepth myQueue (depth+1);
                HH.moveNewThreadToDepth (taskThread, tidParent, depth);
                HH.setDepth (taskThread, depth+1);
                (* setTaskBox myId t; *)
                traceSchedIdleLeave ();
                traceSchedWorkEnter ();
                IdleTimer.stop ();
                WorkTimer.start ();
                Thread.atomicBegin ();
                Thread.atomicBegin ();
                assertAtomic "acquireWork before thread switch" 2;
                threadSwitchEndAtomic taskThread;
                WorkTimer.stop ();
                IdleTimer.start ();
                traceSchedWorkLeave ();
                traceSchedIdleEnter ();
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
      traceSchedIdleEnter ();
      IdleTimer.start ();
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
        Thread.atomicBegin ();
        threadSwitchEndAtomic schedThread
      end
    else
      let
        val (afterReturnToSched, acquireWork) = setupSchedLoop ()
      in
        traceSchedWorkEnter ();
        WorkTimer.start ();
        Thread.atomicBegin ();
        threadSwitchEndAtomic originalThread;
        WorkTimer.stop ();
        IdleTimer.start ();
        traceSchedWorkLeave ();
        traceSchedIdleEnter ();
        afterReturnToSched ();
        setQueueDepth (myWorkerId ()) 1;
        acquireWork ();
        die (fn _ => "scheduler bug: scheduler exited acquire-work loop")
      end


  val _ =
    if P > Heartbeat.relayerThreshold then () else
      MLton.Itimer.set (MLton.Itimer.Real,
        { interval = Time.fromMicroseconds Heartbeat.interval
        , value = Time.fromMicroseconds Heartbeat.interval
        })


  (* This might look silly, but don't remove it! See here:
   *   https://github.com/MPLLang/mpl/issues/190
   * We have to ensure that there is always at least one use of spork_getData
   * in the program, otherwise the data argument of spork_forkThreadAndSetData
   * will be optimized away, causing the compiler to crash because it doesn't
   * know how to pass a useless argument to the corresponding runtime func.
   *)
  val () = SporkJoin.spork {
        tokenPolicy = TokenPolicyFair,
        body = fn () => (),
        spwn = fn () => (),
        seq  = fn () => (),
        sync = fn ((), ()) => (),
        unstolen = NONE
      }
end

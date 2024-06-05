(* Author: Sam Westrick (swestric@cs.cmu.edu) *)

functor MkScheduler() =
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

  val spawnCost = Word32.fromInt (parseInt "sched-spawn-cost" 1)

  type gcstate = MLton.Pointer.t
  val gcstate = _prim "GC_state": unit -> gcstate;

  val getHeartbeatMicroseconds =
    _import "GC_getHeartbeatMicroseconds" runtime private: gcstate -> Word32.word;
  val heartbeatMicroseconds =
    LargeInt.fromInt (Word32.toInt (getHeartbeatMicroseconds (gcstate())))

  val getHeartbeatRelayerThreshold =
    _import "GC_getHeartbeatRelayerThreshold" runtime private: gcstate -> Word32.word;
  val relayerThreshold =
    Word32.toInt (getHeartbeatRelayerThreshold (gcstate ()))

  val getWealthPerHeartbeat =
    _import "GC_getHeartbeatTokens" runtime private: gcstate -> Word32.word;
  val wealthPerHeartbeat =
    Word32.toInt (getWealthPerHeartbeat (gcstate()))

  (* val sendHeartbeatToOtherProc =
    _import "GC_sendHeartbeatToOtherProc" runtime private: gcstate * Word32.word -> unit;
  val sendHeartbeatToOtherProc =
    (fn p => sendHeartbeatToOtherProc (gcstate (), Word32.fromInt p)) *)

  (* val sendHeartbeatToSelf =
    _import "GC_sendHeartbeatToSelf" runtime private: gcstate -> unit;
  val sendHeartbeatToSelf =
    (fn () => sendHeartbeatToSelf (gcstate ())) *)


  val tryConsumeSpareHeartbeats =
    _import "GC_tryConsumeSpareHeartbeats" runtime private: gcstate * Word32.word -> bool;
  val tryConsumeSpareHeartbeats =
    (fn w => tryConsumeSpareHeartbeats (gcstate (), w))

  
  val addSpareHeartbeats =
    _import "GC_addSpareHeartbeats" runtime private: gcstate * Word32.word -> Word32.word;
  val addSpareHeartbeats =
    (fn i => Word32.toInt (addSpareHeartbeats (gcstate (), Word32.fromInt i)))

  
  val currentSpareHeartbeatTokens = _prim "Heartbeat_tokens": unit -> Word32.word;


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

  val pcall = _prim "PCall":
    ('a -> 'b)      (* left side *)
    * 'a            (* left side argument *)
    * ('b -> 'c)    (* sequential left-side continuation *)
    * ('b -> 'c)    (* parallel left-side continuation (left-side sync code) *)
    * ('d -> 'e)    (* parallel right-side task (+right-side sync code), no return allowed! *)
    * 'd            (* parallel right-side argument *)
    -> 'c;

  val primGetData = _prim "PCall_getData": unit -> 'a;

  (* Could add boolean to prim: indicator for
   *   findOldestPromotable     (promote at heartbeats)
   *   vs
   *   findYoungestPromotable   (promote at pcalls -- youngest is the ONLY promotable)
   *)
  val primForkThreadAndSetData = _prim "PCall_forkThreadAndSetData": Thread.t * 'a -> Thread.p;

  fun assertAtomic msg x =
    let
      val ass = Word32.toInt (Thread.atomicState ())
    in
      if ass = x then ()
      else die (fn _ => "scheduler bug: " ^ msg ^ ": atomic " ^ Int.toString ass ^ " but expected " ^ Int.toString x)
    end

  fun threadSwitchEndAtomic t =
    ( if Thread.atomicState () <> 0w0 then ()
      else die (fn _ => "scheduler bug: threadSwitchEndAtomic while non-atomic")
    ; Thread.switchTo t
    )


(*
  fun doPromoteNow () =
    ( assertAtomic "start doPromoteNow" 0
    ; Thread.atomicBegin ()
    ; sendHeartbeatToSelf ()
    (* a hack to make signal handler happen now *)
    ; threadSwitchEndAtomic (Thread.current ())
    (* ; tryConsumeSpareHeartbeats (Word32.fromInt wealthPerHeartbeat) *)
    ; assertAtomic "end doPromoteNow" 0
    )
*)

  structure HM = MLton.HM
  structure HH = MLton.Thread.HierarchicalHeap
  type hh_address = Word64.word
  type gctask_data = Thread.t * (hh_address ref)

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


  (* datatype gc_joinpoint =
    GCJ of {gcTaskData: gctask_data option, tidRight: Word64.word} *)
    (** The fact that the gcTaskData is an option here is a questionable
      * hack... the data will always be SOME. But unwrapping it may affect
      * how many allocations occur when spawning a gc task, which in turn
      * affects the GC snapshot, which is already murky.
      *)

  datatype 'a joinpoint =
    J of
      { leftSideThread: Thread.t
      , rightSideThread: Thread.t option ref
      , rightSideResult: 'a Result.t option ref
      , incounter: int ref
      , tidRight: Word64.word
      , spareHeartbeatsGiven: int
      (* , gcj: gc_joinpoint option *)
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


  (* ========================================================================
   * TASKS
   *)

  (* In the case of NormalTask and NewThread, the Word64 is the decheck id that
   * we should use for the chunks allocated for these tasks.
   *)
  datatype task =
    NormalTask of (unit -> unit) * Word64.word * int
  | NewThread of Thread.p * Word64.word * int
  | Continuation of Thread.t * int
  | GCTask of gctask_data

  (* ========================================================================
   * STATS
   *)

  val numSpawns = Array.array (P, 0)
  val numEagerSpawns = Array.array (P, 0)
  val numHeartbeats = Array.array (P, 0)
  val numSkippedHeartbeats = Array.array (P, 0)
  val numSteals = Array.array (P, 0)

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

  (** ========================================================================
    * SPARE HEARTBEATS
    *)

  fun splitSpares w =
    Word32.>> (w, 0w1)

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

  fun setGCTask p data =
    #gcTask (vectorSub (workerLocalData, p)) := data

  fun getGCTask p =
    ! (#gcTask (vectorSub (workerLocalData, p)))

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
      if not (Queue.pollHasWork queue) then
        NONE
      else
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
   * FORK JOIN
   *)

  structure ForkJoin =
  struct

    fun trySpawnGC interruptedThread : bool =
      let
        val thread = Thread.current ()
        val depth = HH.getDepth thread
      in
        if depth > maxCCDepth then
          false
        else
          let
            val heapId = ref (HH.getRoot thread)
            val gcTaskTuple = (interruptedThread, heapId)
            val gcTaskData = SOME gcTaskTuple
            val gcTask = GCTask gcTaskTuple
            val cont_arr1 = ref NONE
            val cont_arr2 = ref NONE
            val cont_arr3 = ref (SOME (fn _ => (gcTask, gcTaskData))) (* a hack, I hope it works. *)

            (** The above could trigger a local GC and invalidate the hh
              * identifier... :'(  ???????
              *)
            val _ = heapId := HH.getRoot thread
          in
            if not (HH.registerCont (cont_arr1, cont_arr2, cont_arr3, thread)) then
              false
            else
              ( setGCTask (myWorkerId ()) gcTaskData (* This communicates with the scheduler thread *)
              ; true
              )
          end
      end

(*
    fun spawnGC interruptedThread : gc_joinpoint option =
      let
        val thread = Thread.current ()
        val depth = HH.getDepth thread
      in
        if depth > maxCCDepth then
          NONE
        else
          let
            (** SAM_NOTE: atomic begin/end not needed here, becuase this is
              * already run in signal handler.
              *)

            val heapId = ref (HH.getRoot thread)
            val gcTaskTuple = (interruptedThread, heapId)
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
                val (tidLeft, tidRight) = DE.decheckFork ()
                val _ = push gcTask
                val _ = HH.setDepth (thread, depth + 1)
                val _ = DE.decheckSetTid tidLeft
                val _ = HH.forceLeftHeap(myWorkerId(), thread)
              in
                SOME (GCJ {gcTaskData = gcTaskData, tidRight = tidRight})
              end
          end
      end


    fun syncGC doClearSuspects (GCJ {gcTaskData, tidRight}) =
      let
        val _ = Thread.atomicBegin ()
        val thread = Thread.current ()
        val depth = HH.getDepth thread
        val newDepth = depth-1
      in
        if popDiscard() then
          ( ()
          ; dbgmsg' (fn _ => "switching to do some GC stuff")
          ; setGCTask (myWorkerId ()) gcTaskData (* This communicates with the scheduler thread *)
          ; push (Continuation (thread, newDepth))
          ; assertAtomic "syncGC before returnToSched" 1
          ; returnToSchedEndAtomic ()
          ; assertAtomic "syncGC after returnToSched" 1
          ; dbgmsg' (fn _ => "back from GC stuff")
          )
        else
          ( setQueueDepth (myWorkerId ()) newDepth
          );

        (* This can be reused here... the name isn't appropriate in this
         * context, but the functionality is the same:
         *   - promote chunks into parent
         *   - update depth->newDepth
         *   - update decheck state by joining tidLeft and tidRight.
         *)
        HH.joinIntoParentBeforeFastClone
          { thread = thread
          , newDepth = newDepth
          , tidLeft = DE.decheckGetTid thread
          , tidRight = tidRight
          };

        assertAtomic "syncGC done" 1;
        Thread.atomicEnd ();

        doClearSuspects (thread, newDepth)
      end
*)

    (* runs in signal handler *)
    fun doSpawn (interruptedLeftThread: Thread.t) : unit =
      let
        (* val gcj = spawnGC interruptedLeftThread *)
        val _ = assertAtomic "spawn after spawnGC" 1

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

        val _ = tryConsumeSpareHeartbeats spawnCost
        val currentSpare = currentSpareHeartbeatTokens ()
        val halfSpare =
          let
            val halfSpare = splitSpares currentSpare
          in
            ( tryConsumeSpareHeartbeats halfSpare
            ; halfSpare
            )
          end


        val jp =
          J { leftSideThread = interruptedLeftThread
            , rightSideThread = rightSideThreadSlot
            , rightSideResult = rightSideResult
            , incounter = incounter
            , tidRight = tidRight
            , spareHeartbeatsGiven = Word32.toInt halfSpare
            (* , gcj = gcj *)
            }

        (* this sets the join for both threads (left and right) *)
        val rightSideThread = primForkThreadAndSetData (interruptedLeftThread, jp)

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
    fun maybeSpawn (interruptedLeftThread: Thread.t) : bool =
      let
        val depth = HH.getDepth (Thread.current ())
      in
        if depth >= Queue.capacity orelse not (depthOkayForDECheck depth) then
          false
        else if not (HH.canForkThread interruptedLeftThread) then
          false
        else
          ( doSpawn interruptedLeftThread
          ; true
          )
      end


    fun doSpawnFunc {allowCGC: bool} (g: unit -> 'a) : 'a joinpoint =
      let
        val _ = Thread.atomicBegin ()
        val thread = Thread.current ()
        (* val gcj =
          if allowCGC then spawnGC thread else NONE *)

        val _ = assertAtomic "spawn after spawnGC" 1

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

        val currentSpare = currentSpareHeartbeatTokens ()
        val halfSpare = splitSpares currentSpare
        val _ = tryConsumeSpareHeartbeats halfSpare

        fun g' () =
          let
            val () = DE.copySyncDepthsFromThread (thread, Thread.current (), depth+1)
            val () = DE.decheckSetTid tidRight
            val () = HH.forceLeftHeap(myWorkerId(), Thread.current ())
            val _ = addSpareHeartbeats (Word32.toInt halfSpare)
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
          , spareHeartbeatsGiven = Word32.toInt halfSpare
          (* , gcj = gcj *)
          }
      end


    fun maybeSpawnFunc {allowCGC: bool} (g: unit -> 'a) : 'a joinpoint option =
      let
        val depth = HH.getDepth (Thread.current ())
      in
        if depth >= Queue.capacity orelse not (depthOkayForDECheck depth) then
          NONE
        else
          SOME (doSpawnFunc {allowCGC=allowCGC} g)
      end


    (** Must be called in an atomic section. Implicit atomicEnd() *)
    fun syncEndAtomic
        (doClearSuspects: Thread.t * int -> unit)
        (J {rightSideThread, rightSideResult, incounter, tidRight, (*gcj,*) spareHeartbeatsGiven, ...} : 'a joinpoint)
        (g: unit -> 'a)
        : 'a Result.t
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
            ( dbgmsg'' (fn _ => "popDiscard success at depth " ^ Int.toString depth)

            (* promote chunks into parent, update depth->newDepth, update
             * decheck state by joining tidLeft and tidRight.
             *)
            ; HH.joinIntoParentBeforeFastClone
                {thread=thread, newDepth=newDepth, tidLeft=tidLeft, tidRight=tidRight}

            ; traceSchedJoinFast ()
            ; Thread.atomicEnd ()

            ; doClearSuspects (thread, newDepth)
            ; if newDepth <> 1 then () else HH.updateBytesPinnedEntangledWatermark ()

            ; Result.result g
            )
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
                    result
                  end
            )
      in
        (* case gcj of
          NONE => ()
        | SOME gcj => syncGC doClearSuspects gcj; *)

        result
      end

    (* ===================================================================
     * handler fn definitions
     *)

    fun heartbeatHandler generateWealth (thread: Thread.t) =
      let
        (* generateWealth = true: this is a heartbeat
         * generateWealth = false: this is a pcall check
         *
         * at heartbeat, if we had at least one excess token already, then
         * no need to walk the stack -- no pcall frames exist.
         *
         * at a pcall check, it should be possible to not walk the whole
         * stack. there should be exactly one pcall frame at the top of the
         * stack.
         *)

        val hadEnoughToSpawnBefore =
          (currentSpareHeartbeatTokens () >= spawnCost)

        val _ =
          if generateWealth then
            (addSpareHeartbeats wealthPerHeartbeat; ())
          else ()

        val schedThread = getSchedThread ()
      in
        if Option.isSome schedThread andalso trySpawnGC thread then
          (* If we successfully spawned a CGC, then we need to immediately 
           * switch back to our scheduler thread to start working on the
           * CGC. 
           *
           * To preserve invariants, we'll empty out all our tokens.
           * (Invariant of signal handler is: if tokens are leftover,
           * then no promotable frames in the call-stack. Here we're spawning
           * a CGC and haven't looked at the stack at all...)
           *
           * TODO: figure out if there's a way to keep our tokens but still
           * preserve invariants...
           *)
          ( tryConsumeSpareHeartbeats (currentSpareHeartbeatTokens ())
          ; Option.valOf (getSchedThread ())
          )
        else
          let
            fun loop i =
              if
                currentSpareHeartbeatTokens () >= spawnCost
                andalso maybeSpawn thread
              then
                loop (i+1)
              else
                i

            val numSpawned =
              if generateWealth andalso hadEnoughToSpawnBefore then
                (incrementNumSkippedHeartbeats (); 0)
              else
                loop 0
          in
            if generateWealth then incrementNumHeartbeats () else ();

            if (not generateWealth) andalso numSpawned > 0
            then addEagerSpawns numSpawned
            else ();

            thread
          end
      end

    (* fun handler msg =
      MLton.Signal.Handler.inspectInterrupted heartbeatHandler *)

    fun doIfArgIsNotSchedulerThread (f: Thread.t -> Thread.t) (arg: Thread.t) =
      case getSchedThread () of
        NONE => arg
      | SOME t =>
          if MLton.eq (arg, t) then arg
          else f arg


    (** itimer is used to deliver signals regularly. sigusr1 is used to relay
      * these to all processes
      *)
    val _ =
      if P > relayerThreshold then () else
        MLton.Signal.setHandler
          ( MLton.Itimer.signal MLton.Itimer.Real
          , MLton.Signal.Handler.inspectInterrupted
              (doIfArgIsNotSchedulerThread (heartbeatHandler true))
          )

    val _ = MLton.Signal.setHandler
      ( Posix.Signal.usr1
      , MLton.Signal.Handler.inspectInterrupted
          (doIfArgIsNotSchedulerThread (heartbeatHandler true))
      )

    val _ = MLton.Signal.setHandler
      ( Posix.Signal.usr2
      , MLton.Signal.Handler.inspectInterrupted
          (doIfArgIsNotSchedulerThread (heartbeatHandler false))
      )


    (* =======================================================================
     *)


    fun simpleParFork (f: unit -> unit, g: unit -> unit) : unit =
      case maybeSpawnFunc {allowCGC = false} g of
        NONE => (f (); g ())
      | SOME gj =>
          let
            val fr = Result.result f

            val _ = Thread.atomicBegin ()
            val gr = syncEndAtomic maybeParClearSuspectsAtDepth gj g
          in
            (Result.extractResult fr; Result.extractResult gr)
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
                  (fn _ => processLoop i mid,
                   fn _ => processLoop mid j)
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

  
    (* ===================================================================
     * fork definition
     *)


    fun pcallFork (f: unit -> 'a, g: unit -> 'b) : 'a * 'b =
      let
        val (inject, project) = Universal.embed ()

        fun leftSide () =
          Result.result f

        fun leftSideSequentialCont fres =
          (Result.extractResult fres, g ())

        fun leftSideParCont fres =
          let
            val _ = dbgmsg'' (fn _ => "hello from left-side par continuation")
            val _ = Thread.atomicBegin ()
            val _ = assertAtomic "leftSideParCont" 1
            val jp = primGetData ()
            val gres =
              syncEndAtomic maybeParClearSuspectsAtDepth jp (inject o g)
          in
            (Result.extractResult fres,
             case project (Result.extractResult gres) of
                SOME gres => gres
              | _ => die (fn _ => "scheduler bug: leftSideParCont: failed project right-side result"))
          end

        fun rightSide () =
          let
            val _ = assertAtomic "pcallFork rightside begin" 1
            val J {leftSideThread, rightSideThread, rightSideResult, tidRight, incounter, spareHeartbeatsGiven, ...} =
              primGetData ()
            val () = DE.decheckSetTid tidRight

            val thread = Thread.current ()
            val depth = HH.getDepth thread
            val _ = dbgmsg'' (fn _ => "rightside begin at depth " ^ Int.toString depth)

            val _ = HH.forceLeftHeap(myWorkerId(), thread)
            val _ = addSpareHeartbeats spareHeartbeatsGiven
            val _ = assertAtomic "pcallfork rightSide before execute" 1
            val _ = Thread.atomicEnd()

            val gr = Result.result (inject o g)

            val _ = Thread.atomicBegin ()
            val depth' = HH.getDepth (Thread.current ())
            val _ =
              if depth = depth' then ()
              else die (fn _ => "scheduler bug: depth mismatch: rightside began at depth " ^ Int.toString depth ^ " and ended at " ^ Int.toString depth')

            val _ = dbgmsg'' (fn _ => "rightside done! at depth " ^ Int.toString depth')
            val _ = assertAtomic "pcallFork rightside begin synchronize" 1
          in
            rightSideThread := SOME thread;
            rightSideResult := SOME gr;

            if decrementHitsZero incounter then
              ( ()
              ; dbgmsg'' (fn _ => "rightside synchronize: become left")
              ; setQueueDepth (myWorkerId ()) depth
                (** Atomic 1 *)
              ; Thread.atomicBegin ()

                (** Atomic 2 *)

                (** (When sibling is resumed, it needs to be atomic 1.
                  * Switching threads is implicit atomicEnd(), so we need
                  * to be at atomic2
                  *)
              ; assertAtomic "pcallFork rightside switch-to-left" 2
              ; threadSwitchEndAtomic leftSideThread
              )
            else
              ( dbgmsg'' (fn _ => "rightside synchronize: back to sched")
              ; assertAtomic "pcallFork rightside before returnToSched" 1
              ; returnToSchedEndAtomic ()
              )
          end
      in
        pcall
          ( leftSide
          , ()
          , leftSideSequentialCont
          , leftSideParCont
          , rightSide
          , ()
          )
      end


    fun greedyWorkAmortizedFork (f: unit -> 'a, g: unit -> 'b) : 'a * 'b =
      if currentSpareHeartbeatTokens () < spawnCost then
        pcallFork (f, g)
      else
        case maybeSpawnFunc {allowCGC = true} g of
          NONE => (f (), g ())
        | SOME gj =>
            let
              val _ = tryConsumeSpareHeartbeats spawnCost
              val _ = addEagerSpawns 1
              val fr = Result.result f
              val _ = Thread.atomicBegin ()
              val gr = syncEndAtomic maybeParClearSuspectsAtDepth gj g
            in
              (Result.extractResult fr, Result.extractResult gr)
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

      fun stealLoop () =
        let
          fun loop tries =
            if tries = P * 100 then
              ( IdleTimer.tick ()
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
              | SOME (task, depth) => (task, depth)
            end

          val result = loop 0
        in
          result
        end

      (* ------------------------------------------------------------------- *)

      fun afterReturnToSched () =
        case getGCTask myId of
          NONE => ( dbgmsg'' (fn _ => "back in sched; no GC task"); () )
        | SOME (thread, hh) =>
            ( dbgmsg'' (fn _ => "back in sched; found GC task")
            ; setGCTask myId NONE
            ; push (Continuation (thread, HH.getDepth thread))
            (* ; print ("afterReturnToSched: found GC task\n") *)
            (* ; print ("found GC task; atomicState = " ^ Int.toString (Word32.toInt (Thread.atomicState ())) ^ "\n") *)
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
              | SOME (Continuation (thread, _)) =>
                  ( ()
                  ; dbgmsg'' (fn _ => "resume task thread")
                  (* ; Thread.atomicBegin () *)
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
          val (task, depth) = stealLoop ()
          val _ = incrementNumSteals ()
        in
          case task of
            GCTask (thread, hh) =>
              ( dbgmsg'' (fn _ => "starting GCTask")
              ; traceSchedIdleLeave ()
              ; traceSchedWorkEnter ()
              ; IdleTimer.stop ()
              ; WorkTimer.start ()
              ; HH.collectThreadRoot (thread, !hh)
              ; WorkTimer.stop ()
              ; IdleTimer.start ()
              ; traceSchedWorkLeave ()
              ; traceSchedIdleEnter ()
              ; acquireWork ()
              )
          | Continuation (thread, depth) =>
              ( ()
              ; dbgmsg'' (fn _ => "stole continuation (" ^ Int.toString depth ^ ")")
              (* ; dbgmsg' (fn _ => "resume task thread") *)
              ; Queue.setDepth myQueue depth
              ; traceSchedIdleLeave ()
              ; traceSchedWorkEnter ()
              ; IdleTimer.stop ()
              ; WorkTimer.start ()
              (* ; Thread.atomicBegin () *)
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
    if P > relayerThreshold then () else
      MLton.Itimer.set (MLton.Itimer.Real,
        { interval = Time.fromMicroseconds heartbeatMicroseconds
        , value = Time.fromMicroseconds heartbeatMicroseconds
        })

end

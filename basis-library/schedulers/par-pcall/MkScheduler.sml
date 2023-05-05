(* Author: Sam Westrick (swestric@cs.cmu.edu) *)

functor MkScheduler(val forkStrategy: ForkStrategy.t) =
struct

  val _ = print ("J DEBUGGING\n")

  fun arraySub (a, i) = Array.sub (a, i)
  fun arrayUpdate (a, i, x) = Array.update (a, i, x)
  fun vectorSub (v, i) = Vector.sub (v, i)

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

  fun floorLog2 x =
    let
      (* acc = 2^i *)
      fun loop acc i = if 2*acc > x then i else loop (2*acc) (i+1)
    in
      loop 1 0
    end

  val maxEagerForkDepth = 1 + floorLog2 P

  (* val maxEagerForkDepth = parseInt "sched-max-eager-fork-depth" 5 *)
  (* val skipHeartbeatThreshold = parseInt "sched-skip-heartbeat-threshold" 10 *)
  (* val numSpawnsPerHeartbeat = parseInt "sched-num-spawns-per-heartbeat" 1 *)

  val wealthPerHeartbeat = parseInt "sched-wealth-per-heartbeat" 300
  val spawnCost = Word32.fromInt (parseInt "sched-spawn-cost" 10)
  val joinCost = Word32.fromInt (parseInt "sched-join-cost" 0)
  val localJoinCost = Word32.fromInt (parseInt "sched-local-join-cost" 0)

  (* val activatePar = parseFlag "activate-par" *)
  (* val heartbeatMicroseconds =
    LargeInt.fromInt (parseInt "heartbeat-us" 300) *)

  type gcstate = MLton.Pointer.t
  val gcstate = _prim "GC_state": unit -> gcstate;
  val getHeartbeatMicroseconds =
    _import "GC_getHeartbeatMicroseconds" runtime private: gcstate -> Word32.word;
  val heartbeatMicroseconds =
    LargeInt.fromInt (Word32.toInt (getHeartbeatMicroseconds (gcstate())))

  val sendHeartbeatToOtherProc =
    _import "GC_sendHeartbeatToOtherProc" runtime private: gcstate * Word32.word -> unit;
  val sendHeartbeatToOtherProc =
    (fn p => sendHeartbeatToOtherProc (gcstate (), Word32.fromInt p))

  val sendHeartbeatToSelf =
    _import "GC_sendHeartbeatToSelf" runtime private: gcstate -> unit;
  val sendHeartbeatToSelf =
    (fn () => sendHeartbeatToSelf (gcstate ()))


  val tryConsumeSpareHeartbeats =
    _import "GC_tryConsumeSpareHeartbeats" runtime private: gcstate * Word32.word -> bool;
  val tryConsumeSpareHeartbeats =
    (fn w => tryConsumeSpareHeartbeats (gcstate (), w))

  
  val addSpareHeartbeats =
    _import "GC_addSpareHeartbeats" runtime private: gcstate * Word32.word -> Word32.word;
  val addSpareHeartbeats =
    (fn i => Word32.toInt (addSpareHeartbeats (gcstate (), Word32.fromInt i)))

  
  val currentSpareHeartbeats =
    _import "GC_currentSpareHeartbeats" private: gcstate -> Word32.word;
  val currentSpareHeartbeats =
    (fn () => currentSpareHeartbeats (gcstate ()))

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

  (* Matthew will implement. Make sure 'a is objptr. *)
  val primGetJoin = _prim "PCall_getJoin": unit -> 'a;
  (* Replacement for setJoin primitive. *)

  (* Could add boolean to prim: indicator for
   *   findOldestPromotable     (promote at heartbeats)
   *   vs
   *   findYoungestPromotable   (promote at pcalls -- youngest is the ONLY promotable)
   *)
  val primForkThread = _prim "PCall_forkThread": Thread.t * 'a -> Thread.p;

  (* val setSimpleSignalHandler = MLton.Thread.setSimpleSignalHandler *)
  (* fun threadSwitch t =
    ( Thread.atomicBegin ()
    ; Thread.switchTo t
    ) *)

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


  fun doPromoteNow () =
    ( assertAtomic "start doPromoteNow" 0
    ; Thread.atomicBegin ()
    ; sendHeartbeatToSelf ()
    (* a hack to make signal handler happen now *)
    ; threadSwitchEndAtomic (Thread.current ())
    (* ; tryConsumeSpareHeartbeats (Word32.fromInt wealthPerHeartbeat) *)
    ; assertAtomic "end doPromoteNow" 0
    )


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

  datatype 'a joinpoint =
    J of
      { leftSideThread: Thread.t
      , rightSideThread: Thread.t option ref
      , rightSideResult: 'a Result.t option ref
      , incounter: int ref
      , tidRight: Word64.word
      , spareHeartbeatsGiven: int
      , gcj: gc_joinpoint option
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


  (* ========================================================================
   * TASKS
   *)

  datatype task =
    NormalTask of (unit -> unit) * int
  | Continuation of Thread.t * int
  | NewThread of Thread.p * int
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

    val communicate = communicate
    val getIdleTime = getIdleTime

    val maxPermittedCCDepth = 3

    fun spawnGC interruptedThread : gc_joinpoint option =
      let
        val thread = Thread.current ()
        val depth = HH.getDepth thread
      in
        if depth > maxPermittedCCDepth then
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
          ( clear()
          ; setQueueDepth (myWorkerId ()) newDepth
          );

        HH.promoteChunks thread;
        HH.setDepth (thread, newDepth);
        assertAtomic "syncGC done" 1;
        Thread.atomicEnd ()
      end


    (* runs in signal handler *)
    fun doSpawn (interruptedLeftThread: Thread.t) : unit =
      let
        val gcj = spawnGC interruptedLeftThread
        (* val _ = tryConsumeSpareHeartbeats 0w1 *)

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

        val (tidLeft, tidRight) = DE.decheckFork ()

        val _ = tryConsumeSpareHeartbeats spawnCost
        val currentSpare = currentSpareHeartbeats ()
        val halfSpare =
          case forkStrategy of
            ForkStrategy.GreedyWorkAmortized =>
              let
                val halfSpare = splitSpares currentSpare
              in
                ( tryConsumeSpareHeartbeats halfSpare
                ; halfSpare
                )
              end

          | _ =>
              ( tryConsumeSpareHeartbeats currentSpare
              ; 0w0
              )


        val jp =
          J { leftSideThread = interruptedLeftThread
            , rightSideThread = rightSideThreadSlot
            , rightSideResult = rightSideResult
            , incounter = incounter
            , tidRight = tidRight
            , spareHeartbeatsGiven = Word32.toInt halfSpare
            , gcj = gcj
            }

        (* this sets the join for both threads (left and right) *)
        val rightSideThread = primForkThread (interruptedLeftThread, jp)

        (* double check... hopefully correct, not off by one? *)
        val _ = push (NewThread (rightSideThread, depth))
        val _ = HH.setDepth (thread, depth + 1)

        (* NOTE: off-by-one on purpose. Runtime depths start at 1. *)
        val _ = recordForkDepth depth

        val _ = incrementNumSpawns ()

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


(*
    fun doSpawnFunc (g: unit -> 'a) : 'a joinpoint =
      let
        val _ = Thread.atomicBegin ()
        (* val _ = tryConsumeSpareHeartbeats 0w1 *)
        val thread = Thread.current ()
        val gcj = spawnGC thread

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

        val (tidLeft, tidRight) = DE.decheckFork ()

        val currentSpare = currentSpareHeartbeats ()
        val halfSpare = splitSpares currentSpare
        val _ = tryConsumeSpareHeartbeats halfSpare

        fun g' () =
          let
            val () = HH.forceLeftHeap(myWorkerId(), Thread.current ())
            val () = DE.copySyncDepthsFromThread (thread, Thread.current (), depth+1)
            val () = DE.decheckSetTid tidRight
            val _ = addSpareHeartbeats (Word32.toInt halfSpare)
            (* val _ = addSpareHeartbeats 1 *)
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
        val _ = push (NormalTask (g', depth))
        val _ = HH.setDepth (thread, depth + 1)

        (* NOTE: off-by-one on purpose. Runtime depths start at 1. *)
        val _ = recordForkDepth depth

        val _ = incrementNumSpawns ()

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
          , gcj = gcj
          }
      end
*)

(*
    fun maybeSpawnFunc (g: unit -> 'a) : 'a joinpoint option =
      let
        val depth = HH.getDepth (Thread.current ())
      in
        if depth >= Queue.capacity orelse not (depthOkayForDECheck depth) then
          NONE
        else
          SOME (doSpawnFunc g)
      end
*)


    (** Must be called in an atomic section. Implicit atomicEnd() *)
    fun syncEndAtomic
        (J {rightSideThread, rightSideResult, incounter, tidRight, gcj, spareHeartbeatsGiven, ...} : 'a joinpoint)
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
            ; HH.promoteChunks thread
            ; HH.setDepth (thread, newDepth)
            ; DE.decheckJoin (tidLeft, tidRight)
            (* ; addSpareHeartbeats spareHeartbeatsGiven *)
            ; tryConsumeSpareHeartbeats localJoinCost
            (* ; tryConsumeSpareHeartbeats 0w1 *)
            ; Thread.atomicEnd ()
            ; let
                val gr = Result.result g
              in
                (* (gr, DE.decheckGetTid thread) *)
                gr
              end
            )
          else
            ( clear () (* this should be safe after popDiscard fails? *)

            ; if decrementHitsZero incounter then
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
                  in
                    HH.mergeThreads (thread, rightSideThread);
                    tryConsumeSpareHeartbeats joinCost;
                    HH.promoteChunks thread;
                    HH.setDepth (thread, newDepth);
                    DE.decheckJoin (tidLeft, tidRight);
                    setQueueDepth (myWorkerId ()) newDepth;
                    (* tryConsumeSpareHeartbeats 0w1; *)
                    case HM.refDerefNoBarrier rightSideResult of
                      NONE => die (fn _ => "scheduler bug: join failed: missing result")
                    | SOME gr =>
                        ( ()
                        ; assertAtomic "syncEndAtomic after merge" 1
                        ; Thread.atomicEnd ()
                        ; gr
                        )
                  end
            )
      in
        case gcj of
          NONE => ()
        | SOME gcj => syncGC gcj;

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
          (currentSpareHeartbeats () >= spawnCost)

        val _ =
          if generateWealth then
            (addSpareHeartbeats wealthPerHeartbeat; ())
          else ()

        fun loop i =
          if
            currentSpareHeartbeats () >= spawnCost
            andalso maybeSpawn thread
          then
            loop (i+1)
          else
            i

        val numSpawned =
          case forkStrategy of
            ForkStrategy.GreedyWorkAmortized =>
              if generateWealth andalso hadEnoughToSpawnBefore then
                (incrementNumSkippedHeartbeats (); 0)
              else
                loop 0
          
          | ForkStrategy.NaivePCall =>
              loop 0

          | ForkStrategy.EagerHeuristic =>
              if not generateWealth then
                ( addSpareHeartbeats (Word32.toInt spawnCost); loop 0 )
              else
                loop 0
      in
        if generateWealth then incrementNumHeartbeats () else ();

        if (not generateWealth) andalso numSpawned > 0
        then addEagerSpawns numSpawned
        else ()
      end

    (* fun handler msg =
      MLton.Signal.Handler.inspectInterrupted heartbeatHandler *)

    fun doIfArgIsNotSchedulerThread (f: Thread.t -> unit) (arg: Thread.t) =
      case getSchedThread () of
        NONE => ()
      | SOME t =>
          if MLton.eq (arg, t) then ()
          else f arg

    (** itimer is used to deliver signals regularly. sigusr1 is used to relay
      * these to all processes
      *)
    val _ = MLton.Signal.setHandler
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
  
    (* ===================================================================
     * fork definition
     *)


    fun pcallFork (f: unit -> 'a, g: unit -> 'b) =
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
            val jp = primGetJoin ()
            val gres = syncEndAtomic jp (inject o g)
          in
            (Result.extractResult fres,
             case project (Result.extractResult gres) of
                SOME gres => gres
              | _ => die (fn _ => "scheduler bug: leftSideParCont: failed project right-side result"))
          end

        fun rightSide () =
          let
            val _ = assertAtomic "pcallFork rightside begin" 1

            val thread = Thread.current ()
            val depth = HH.getDepth thread
            val _ = HH.forceLeftHeap(myWorkerId(), thread)

            (* val _ =
              dbgmsg''' (fn _ => "depth " ^ Int.toString depth ^ " begin with " ^ Int.toString (Word32.toInt (currentSpareHeartbeats ())) ^ " spares") *)

            (* val _ = addSpareHeartbeats 1 *)
            (* val _ = addSpareHeartbeats (1 + takeSpares {depth=depth}) *)

            val _ = dbgmsg'' (fn _ => "rightside begin at depth " ^ Int.toString depth)
            val J {leftSideThread, rightSideThread, rightSideResult, tidRight, incounter, spareHeartbeatsGiven, ...} =
              primGetJoin ()
            val () = DE.decheckSetTid tidRight
            val _ = addSpareHeartbeats spareHeartbeatsGiven
            val _ = Thread.atomicEnd()

            val gr = Result.result (inject o g)

            val _ = Thread.atomicBegin ()
            val depth' = HH.getDepth (Thread.current ())
            val _ =
              if depth = depth' then ()
              else die (fn _ => "scheduler bug: depth mismatch: rightside began at depth " ^ Int.toString depth ^ " and ended at " ^ Int.toString depth')

            val _ = dbgmsg'' (fn _ => "rightside done at depth " ^ Int.toString depth')
            val _ = assertAtomic "pcallFork rightside begin synchronize" 1
          in
            rightSideThread := SOME thread;
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
              ; assertAtomic "pcallFork rightside switch-to-left" 2
              ; threadSwitchEndAtomic leftSideThread
              )
            else
              ( assertAtomic "pcallFork rightside before returnToSched" 1
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


    (* fun eagerFork1 (f, g) =
      case maybeSpawnFunc g of
        SOME jp =>
          let
            val fres = Result.result f
            val _ = Thread.atomicBegin ()
            val gres = syncEndAtomic jp g
          in
            (Result.extractResult fres, Result.extractResult gres)
          end

      | NONE => (f (), g ()) *)
 

    fun eagerFork (f, g) =
      let
        fun f' () = ( doPromoteNow (); f () )
      in
        pcallFork (f', g)
      end

    
    fun eagerHeuristicFork (f, g) =
      if HH.getDepth (Thread.current ()) <= maxEagerForkDepth then
        eagerFork (f, g)
      else
        pcallFork (f, g)

    
    fun greedyWorkAmortizedFork (f, g) =
      let
        fun f' () =
          ( if currentSpareHeartbeats () < spawnCost then
              ()
            else
              doPromoteNow ()

          ; f ()
          )
      in
        pcallFork (f', g)
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
          NONE => ( dbgmsg'' (fn _ => "back in sched; no GC task"); () )
        | SOME (thread, hh) =>
            ( dbgmsg'' (fn _ => "back in sched; found GC task")
            ; setGCTask myId NONE
            (* ; print ("afterReturnToSched: found GC task\n") *)
            ; HH.collectThreadRoot (thread, !hh)
            (* ; print ("afterReturnToSched: done with GC\n") *)
            ; case pop () of
                NONE => ()
              | SOME (Continuation (thread, _)) =>
                  ( ()
                  ; dbgmsg'' (fn _ => "resume task thread")
                  ; Thread.atomicBegin ()
                  ; Thread.atomicBegin ()
                  ; assertAtomic "afterReturnToSched before thread switch" 2
                  ; threadSwitchEndAtomic thread
                  ; afterReturnToSched ()
                  )
              | SOME _ =>
                  die (fn _ => "bug: Scheduler.afterReturnToSched: impossible")
            )

      fun acquireWork () : unit =
        let
          val idleTimer = startTimer myId
          val (task, depth, idleTimer') = request idleTimer
          val _ = stopTimer idleTimer'
          val _ = incrementNumSteals ()
        in
          case task of
            GCTask (thread, hh) =>
              ( dbgmsg'' (fn _ => "starting GCTask")
              ; HH.collectThreadRoot (thread, !hh)
              ; acquireWork ()
              )
          | Continuation (thread, depth) =>
              ( ()
              ; dbgmsg'' (fn _ => "stole continuation (" ^ Int.toString depth ^ ")")
              (* ; dbgmsg' (fn _ => "resume task thread") *)
              ; Queue.setDepth myQueue depth
              ; Thread.atomicBegin ()
              ; Thread.atomicBegin ()
              ; assertAtomic "acquireWork before thread switch" 2
              ; threadSwitchEndAtomic thread
              ; afterReturnToSched ()
              ; Queue.setDepth myQueue 1
              ; acquireWork ()
              )
          | NormalTask (taskFn, depth) =>
              let
                val taskThread = Thread.copy prototypeThread
              in
                if depth >= 1 then () else
                  die (fn _ => "scheduler bug: acquired with depth " ^ Int.toString depth);
                Queue.setDepth myQueue (depth+1);
                HH.moveNewThreadToDepth (taskThread, depth);
                HH.setDepth (taskThread, depth+1);
                setTaskBox myId taskFn;
                Thread.atomicBegin ();
                Thread.atomicBegin ();
                assertAtomic "acquireWork before thread switch" 2;
                threadSwitchEndAtomic taskThread;
                afterReturnToSched ();
                Queue.setDepth myQueue 1;
                acquireWork ()
              end
          | NewThread (thread, depth) =>
              let
                val taskThread = Thread.copy thread
              in
                if depth >= 1 then () else
                  die (fn _ => "scheduler bug: acquired with depth " ^ Int.toString depth);
                Queue.setDepth myQueue (depth+1);
                HH.moveNewThreadToDepth (taskThread, depth);
                HH.setDepth (taskThread, depth+1);
                (* setTaskBox myId t; *)
                Thread.atomicBegin ();
                Thread.atomicBegin ();
                assertAtomic "acquireWork before thread switch" 2;
                threadSwitchEndAtomic taskThread;
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
        Thread.atomicBegin ();
        threadSwitchEndAtomic schedThread
      end
    else
      let
        val (afterReturnToSched, acquireWork) = setupSchedLoop ()
      in
        Thread.atomicBegin ();
        threadSwitchEndAtomic originalThread;
        afterReturnToSched ();
        setQueueDepth (myWorkerId ()) 1;
        acquireWork ();
        die (fn _ => "scheduler bug: scheduler exited acquire-work loop")
      end


  val _ =
    MLton.Itimer.set (MLton.Itimer.Real,
      { interval = Time.fromMicroseconds heartbeatMicroseconds
      , value = Time.fromMicroseconds heartbeatMicroseconds
      })

end

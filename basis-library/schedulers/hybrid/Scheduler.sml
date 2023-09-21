(* Author: Sam Westrick (swestric@cs.cmu.edu) *)

(* Scheduler implements a single structure.
 *   ForkJoin : FORK_JOIN
 * It is pulled out of Scheduler at the bottom of this file. *)
structure Scheduler =
struct

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

  fun parseReal key default =
    case search ("-" ^ key) (CommandLine.arguments ()) of
      NONE => default
    | SOME [] => die (fn _ => "Missing argument of \"-" ^ key ^ "\" ")
    | SOME (s :: _) =>
        case Real.fromString s of
          NONE => die (fn _ => "Cannot parse real from \"-" ^ key ^ " " ^ s ^ "\"")
        | SOME x => x

  fun parseString key default =
    case search ("-" ^ key) (CommandLine.arguments ()) of
      NONE => default
    | SOME [] => die (fn _ => "Missing argument of \"-" ^ key ^ "\" ")
    | SOME (s :: _) => s

  fun timeNowMicroseconds () : LargeInt.int =
    let
      val sec = ref (C_Time.castFromFixedInt 0)
      val usec = ref (C_SUSeconds.castFromFixedInt 0)
      val _ =
        if ~1 <> PrimitiveFFI.Time.getTimeOfDay (sec, usec) then ()
        else die (fn _ => "scheduler bug: getTimeOfDay failed")
    in
      (1000000 * C_Time.toLargeInt (!sec))
      +
      C_SUSeconds.toLargeInt (!usec)
    end

  val programStartTimeMicroseconds = timeNowMicroseconds ()

  fun timeNowMicrosecondsSinceProgramStart () : Int64.int =
    Int64.fromLarge (timeNowMicroseconds () - programStartTimeMicroseconds)

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
  type hh_address = Word64.word
  type gctask_data = Thread.t * (hh_address ref)

  datatype task =
    NormalTask of (unit -> unit) * int (* function and depth *)
  | Continuation of Thread.t * int
  | GCTask of gctask_data


  fun cmpContinuationTaskDepth (t1, t2) =
    case (t1, t2) of
      (Continuation (_, d1), Continuation (_, d2)) => Int.compare (d1, d2)
    | _ => die (fn _ => "scheduler bug: cmpContinuationTaskDepth: not both continuations")

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

  val maxCCDepth = MPL.GC.getControlMaxCCDepth ()

  val internalGCThresh = Real.toInt IEEEReal.TO_POSINF
                          ((Math.log10(Real.fromInt P)) / (Math.log10 (2.0)))

  (* val vcas = MLton.Parallel.arrayCompareAndSwap *)
  (* fun cas (a, i) (old, new) = (vcas (a, i) (old, new) = old) *)
  fun faa (r, d) = MLton.Parallel.fetchAndAdd r d
  fun casRef r (old, new) =
    MLton.eq (MLton.Parallel.compareAndSwap r (old, new), old)

  fun decrementHitsZero (x : int ref) : bool =
    faa (x, ~1) = 1

  (* val gpuPayout = parseReal "sched-gpu-payout" 1.0 *)
  (* val keepDamper = parseReal "sched-keep-damper" 500.0 *)
  val dumpGpuInfo = parseFlag "sched-dump-gpu-info"


  (*
  val gpuSelectPolicyStr = parseString "sched-gpu-select" "min-depth"

  datatype gpu_select_policy =
    MinDepth
  | RandomSteal

  val gpuSelectPolicy =
    case gpuSelectPolicyStr of
      "min-depth" => MinDepth
    | "random-steal" => RandomSteal
    | _ => die (fn _ => "unknown sched-gpu-select: " ^ gpuSelectPolicyStr)
  *)


  val hiccupTime = Time.fromMicroseconds 10
  fun hiccup () = OS.Process.sleep hiccupTime

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

  fun dbgmsg'' m =
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
   * HYBRID SCHEDULER DATA
   *)

  (* hybridStartTime = ~1 when no current hybrid region is active.
   * hybridNumChoicesKeptOnCPU is only used when a hybrid region is active. *)

  (*
  val hybridStartTime: Int64.int ref = ref (~1)
  val hybridIdlenessStart: Int64.int ref = ref 0
  val hybridNumChoicesKeptOnCPU: Int64.int ref = ref 0
  *)

  (* SAM_NOTE: if/when we switch to the par+choice+gpu interface, we can add
   * an explicit queue to send `gpu ...` expressions to the gpu manager, but
   * otherwise execute everything else
   *
   *   choice {
   *     cpu = (fn _ => ...),                (* exec by worker *)
   *     gpu = (fn _ =>
   *       copyToGPU...;                     (* exec by worker *)
   *       gpu (fn i => callCudaOnDevice i)  (* exec by GPU manager *)
   *       copyFromGPU...;                   (* exec by worker *)
   *   }
   *)

  (* val hybridTaskQueue: task RingBuffer.t =
    RingBuffer.new {capacity=1000} *)

  (* val hybridDoneQueue: task RingBuffer.t =
    RingBuffer.new {capacity=1000} *)

  (*
  val stealTimeCounters: Int64.int array = Array.tabulate (P, fn _ => 0)

  fun addStealTime delta =
    let
      val myId = myWorkerId ()
    in
      Array.update (stealTimeCounters, myId, delta + Array.sub (stealTimeCounters, myId))
    end

  fun currentStealTimeSpent () =
    Array.foldl op+ 0 stealTimeCounters
  *)

  (* val idlenessCounter: int ref = ref 0
  
  fun updateIdlenessCounterWith (f: int -> int) =
    let
      fun loop old =
        let
          val new = f old
          val old' = MLton.Parallel.compareAndSwap idlenessCounter (old, new)
        in
          if MLton.eq (old, old') then
            ()
          else
            loop old'
        end
        (* handle Overflow => () *)
    in
      loop (!idlenessCounter)
    end *)


  fun i64ToReal x = Real.fromInt (Int64.toInt x)
  fun realToi64 r = Int64.fromLarge (Real.toLargeInt IEEEReal.TO_NEAREST r)

(*
  fun pendingChoiceBoundToKeep () =
    let
      val hstart = !hybridStartTime
    in
      if hstart < 0 then
        0
      else
        let
          val idleness =
            i64ToReal (currentStealTimeSpent () - !hybridIdlenessStart)
          val elapsed =
            i64ToReal (timeNowMicrosecondsSinceProgramStart () - hstart)
        in
          Real.floor ((idleness - (gpuPayout * elapsed)) / keepDamper)
        end
    end
*)

  (* ========================================================================
   * SCHEDULER LOCAL DATA
   *)

  type worker_local_data =
    { queue : task Queue.t
    , sideQueue : task RingBuffer.t
    , pendingChoices: task ConcurrentBinaryHeap.t
    , schedThread : Thread.t option ref
    , gcTask: gctask_data option ref
    }

  fun wldInit p : worker_local_data =
    { queue = Queue.new ()
    , sideQueue = RingBuffer.new {capacity=200}
    , pendingChoices = ConcurrentBinaryHeap.new {capacity=200, cmp=cmpContinuationTaskDepth}
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
        case Queue.tryPopTop queue of
          NONE => NONE
        | SOME (t, _) => SOME t
    end

  fun tryStealSide p =
    let
      val {sideQueue, ...} = vectorSub (workerLocalData, p)
    in
      RingBuffer.popTop sideQueue  
    end

  fun tryPopSide p =
    let
      val {sideQueue, ...} = vectorSub (workerLocalData, p)
    in
      RingBuffer.popBot sideQueue  
    end

  fun tryStealPendingChoice p =
    let
      val {pendingChoices, ...} = vectorSub (workerLocalData, p)
    in
      ConcurrentBinaryHeap.popMin pendingChoices
    end

  fun tryKeepPendingChoice p =
    let
      val {pendingChoices, ...} = vectorSub (workerLocalData, p)
    in
      ConcurrentBinaryHeap.popNearMax pendingChoices
    end

  fun peekPendingChoice p =
    let
      val {pendingChoices, ...} = vectorSub (workerLocalData, p)
    in
      ConcurrentBinaryHeap.peekMin pendingChoices
    end

  fun peekBotPendingChoice p =
    let
      val {pendingChoices, ...} = vectorSub (workerLocalData, p)
    in
      ConcurrentBinaryHeap.peekNearMaxAllowBackoff pendingChoices
    end

  fun tryPushPendingChoice p x =
    let
      val {pendingChoices, ...} = vectorSub (workerLocalData, p)
    in
      ConcurrentBinaryHeap.push pendingChoices x
    end


  (* fun tryKeepPendingChoice p =
    let
      val numKept = !hybridNumChoicesKeptOnCPU
      val keepBound = pendingChoiceBoundToKeep ()
      val {pendingChoices, ...} = vectorSub (workerLocalData, p)
    in
      if numKept >= keepBound orelse RingBuffer.size pendingChoices = 0 then
        NONE
      else if casRef hybridNumChoicesKeptOnCPU (numKept, numKept+1) then
        RingBuffer.popBot pendingChoices
        (* if p = myWorkerId () then
          RingBuffer.popBot pendingChoices
        else
          RingBuffer.popTop pendingChoices *)
      else
        tryKeepPendingChoice p
    end *)


  fun communicate () = ()

  fun push x =
    let
      val myId = myWorkerId ()
      val {queue, ...} = vectorSub (workerLocalData, myId)
    in
      Queue.pushBot queue x
    end

  fun pushSide x =
    let
      val myId = myWorkerId ()
      val {sideQueue, ...} = vectorSub (workerLocalData, myId)
    in
      RingBuffer.pushBot (sideQueue, x)
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

  fun queueSize () =
    let
      val myId = myWorkerId ()
      val {queue, ...} = vectorSub (workerLocalData, myId)
    in
      Queue.size queue
    end

  fun returnToSched () =
    let
      val myId = myWorkerId ()
      val {schedThread, ...} = vectorSub (workerLocalData, myId)
    in
      threadSwitch (Option.valOf (HM.refDerefNoBarrier schedThread))
    end


  fun moveAllTasksIntoSideQueue () =
    let
      val myId = myWorkerId ()
      val {sideQueue, ...} = vectorSub (workerLocalData, myId)
      fun loop () =
        if queueSize () = 0 then ()
        else
        case trySteal myId of
          NONE => loop ()
        | SOME task =>
            ( if RingBuffer.pushBot (sideQueue, task) then ()
              else die (fn _ => "scheduler bug: side queue full")
            ; loop ()
            )
    in
      loop ()
    end


  fun existsPendingChoice () =
    let
      fun loop i =
        if i >= P then false else
        case peekPendingChoice i of
          SOME _ => true
        | NONE => loop (i+1)
    in
      loop 0
    end


  (* ========================================================================
   * FORK JOIN
   *)
  
  local
    datatype choice_status =
      NoPendingChoice
    | SearchingForChoice
    | ActiveChoice of Thread.t

    val currentChoiceStatus = ref NoPendingChoice
    val currentSearcher = ref ~1
  in

  fun tryBecomeChoiceSearcher () =
    let
      val success =
        MLton.eq (!currentChoiceStatus, NoPendingChoice) andalso
        casRef currentChoiceStatus (NoPendingChoice, SearchingForChoice)
    in
      if success then
        ( (*print ("[" ^ Int.toString (myWorkerId ()) ^ "] became GPU manager\n")*) ()
        ; currentSearcher := myWorkerId ()
        ; true
        )
      else
        false
    end


  fun startActiveChoice task =
    case task of
      Continuation (t, _) => 
        ( currentSearcher := ~1
        ; HM.refAssignNoBarrier (currentChoiceStatus, ActiveChoice t)
        )
    | _ => die (fn _ => "scheduler bug: startActiveChoice: expected Continuation(...)")

  
  fun finishActiveChoice () =
    ( HM.refAssignNoBarrier (currentChoiceStatus, SearchingForChoice)
    ; currentSearcher := myWorkerId ()
    )


  fun isActiveChoice t =
    case HM.refDerefNoBarrier currentChoiceStatus of
      ActiveChoice t' => MLton.eq (t, t')
    | _ => false
  

  (* fun isGpuManager id =
    !currentGpuManager = id

  
  fun amGpuManager () =
    isGpuManager (myWorkerId ()) *)


  fun shouldSearchForChoices () = 
    !currentSearcher = myWorkerId ()

  
  fun markNoPendingChoices () =
    ( currentSearcher := ~1
    ; HM.refAssignNoBarrier (currentChoiceStatus, NoPendingChoice)
    )

  end

  (* ========================================================================
   * FORK JOIN
   *)

  structure ForkJoin =
  struct

    datatype ('package, 'result) gpu_task =
      G of
        { spawn: unit -> 'package
        , poll: 'package -> bool
        , finish: 'package -> (unit -> 'result)
        }

    fun simpleFinisher finish pkg =
      let
        val result = finish pkg
      in
        fn () => result
      end

    fun cleanupFinisher finish cleanup pkg =
      let
        val result = finish pkg
      in
        fn () => cleanup result
      end

    fun gpu {spawn, poll, finish} =
      G {spawn = spawn, poll = poll, finish = simpleFinisher finish}

    fun gpuWithCleanup {spawn, poll, finish, cleanup} =
      G {spawn = spawn, poll = poll, finish = cleanupFinisher finish cleanup}

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
        (* val _ = dbgmsg' (fn _ => "fork at depth " ^ Int.toString depth) *)

        (** NOTE: these cannot be safely combined into a single ref. After
          * the join, reading the thread is safe because the thread object
          * is stored at a safe depth. But reading the result is entangled
          * until after the merge happens.
          *)
        val rightSideThread = ref (NONE: Thread.t option)
        val rightSideResult = ref (NONE: 'b result option)
        val incounter = ref 2

        val (tidLeft, tidRight) = DE.decheckFork ()

        fun g' () =
          let
            val () = DE.copySyncDepthsFromThread (thread, depth+1)
            val () = DE.decheckSetTid tidRight
            val gr = result g
            val t = Thread.current ()
          in
            rightSideThread := SOME t;
            rightSideResult := SOME gr;
            if decrementHitsZero incounter then
              ( setQueueDepth (myWorkerId ()) (depth+1)
              ; threadSwitch thread
              )
            else
              returnToSched ()
          end
        val _ = push (NormalTask (g', depth))
        (* val _ =
              if (depth < internalGCThresh) then
                let
                  val cont_arr1 =  Array.array (1, SOME(f))
                  val cont_arr2 =  Array.array (1, SOME(g))
                  val cont_arr3 =  Array.array (0, NONE)
                in
                    HH.registerCont(cont_arr1,  cont_arr2, cont_arr3, thread)
                  ; HH.setDepth (thread, depth + 1)
                  ; HH.forceLeftHeap(myWorkerId(), thread)
                end
              else
                (HH.setDepth (thread, depth + 1)) *)
        val _ = HH.setDepth (thread, depth + 1)

        (* val _ =
          if depth <= 3 then HH.forceLeftHeap(myWorkerId(), thread) else () *)

        (* NOTE: off-by-one on purpose. Runtime depths start at 1. *)
        val _ = recordForkDepth depth

        val _ = DE.decheckSetTid tidLeft
        val fr = result f
        val tidLeft = DE.decheckGetTid thread

        val gr =
          if popDiscard () then
            ( HH.promoteChunks thread
            ; HH.setDepth (thread, depth)
            ; DE.decheckJoin (tidLeft, tidRight)
            (* ; dbgmsg' (fn _ => "join fast at depth " ^ Int.toString depth) *)
            (* ; HH.forceNewChunk () *)
            ; let
                val gr = result g
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
                    HH.setDepth (thread, depth);
                    DE.decheckJoin (tidLeft, tidRight);
                    setQueueDepth (myWorkerId ()) depth;
                    (* dbgmsg' (fn _ => "join slow at depth " ^ Int.toString depth); *)
                    case HM.refDerefNoBarrier rightSideResult of
                      NONE => die (fn _ => "scheduler bug: join failed: missing result")
                    | SOME gr => gr
                  end
            )

        (* val () = DE.decheckJoin (tidLeft, tidRight) *)
      in
        (extractResult fr, extractResult gr)
      end


    fun forkGC thread depth continuation =
      let
        val heapId = ref (HH.getRoot thread)
        val gcTaskTuple = (thread, heapId)
        val gcTaskData = SOME gcTaskTuple
        val gcTask = GCTask gcTaskTuple
        val cont_arr1 = ref (SOME continuation)
        val cont_arr2 = ref (SOME (fn _ => (gcTask, gcTaskData))) (* a hack, I hope it works. *)
        val cont_arr3 = ref NONE

        (** The above could trigger a local GC and invalidate the hh
          * identifier... :'(
          *)
        val _ = heapId := HH.getRoot thread
      in
        if not (HH.registerCont (cont_arr1, cont_arr2, cont_arr3, thread)) then
          continuation ()
        else
          let
            val _ = push gcTask
            val _ = HH.setDepth (thread, depth + 1)
            val _ = HH.forceLeftHeap(myWorkerId(), thread)
            (* val _ = dbgmsg' (fn _ => "fork CC at depth " ^ Int.toString depth) *)
            val result = continuation ()

            val _ =
              if popDiscard() then
                ( (*dbgmsg' (fn _ => "push current (" ^ Int.toString depth ^ ") and switch to scheduler for GCtask")
                ;*)
                  setGCTask (myWorkerId ()) gcTaskData (* This communicates with the scheduler thread *)
                ; push (Continuation (thread, depth))
                ; returnToSched ()
                )
                (* HH.collectThreadRoot (thread, !heapId) *)
              else
                ( clear()
                ; setQueueDepth (myWorkerId ()) depth
                )

            val _ = HH.promoteChunks thread
            val _ = HH.setDepth (thread, depth)
            (* val _ = dbgmsg' (fn _ => "join CC at depth " ^ Int.toString depth) *)
          in
            result
          end
      end

    and fork' {ccOkayAtThisDepth} (f, g) =
      let
        val thread = Thread.current ()
        val depth = HH.getDepth thread
      in
        (* if amGpuManager () then
          (f (), g ()) *)
        (* if ccOkayAtThisDepth andalso depth = 1 then *)
        (*else*) if ccOkayAtThisDepth andalso depth >= 1 andalso depth <= maxCCDepth then
          forkGC thread depth (fn _ => fork' {ccOkayAtThisDepth=false} (f, g))
        else if depth < Queue.capacity andalso depthOkayForDECheck depth then
          parfork thread depth (f, g)
        else
          (* don't let us hit an error, just sequentialize instead *)
          (f (), g ())
      end


    fun fork (f, g) = fork' {ccOkayAtThisDepth=true} (f, g)


    fun doGpuTask (G {spawn, poll, finish}) =
      let
        val t0 = timeNowMicrosecondsSinceProgramStart ()
        (* val _ = updateIdlenessCounterWith (fn _ => 0) *)

        (*
        val _ = hybridNumChoicesKeptOnCPU := 0
        val _ = hybridIdlenessStart := currentStealTimeSpent ()
        val startTime = timeNowMicrosecondsSinceProgramStart ()
        val _ =
          if casRef hybridStartTime (~1, startTime) then ()
          else die (fn _ => "scheduler bug: start hybrid section failed")
        *)

        val thread = Thread.current ()
        val depth = HH.getDepth thread

        val package = spawn ()

        val _ = HH.setDepth (thread, depth + 1)
        val _ = HH.forceLeftHeap(myWorkerId(), thread)
        val _ = setQueueDepth (myWorkerId ()) (depth+1)

        (* TODO: might be unsafety here? What happens if a local GC is
         * triggered during this loop? Perhaps we need to advance the depth
         * and use a fresh heap while waiting for the GPU to finish?
         *)
        fun loopWaitForGpu () =
          ( hiccup ()
          ; if not (poll package) then
              loopWaitForGpu ()
            else
              ()
          )

        val _ = loopWaitForGpu ()
        val _ = setQueueDepth (myWorkerId ()) depth
        val _ = HH.promoteChunks thread
        val _ = HH.setDepth (thread, depth)
        val result = finish package

        val t1 = timeNowMicrosecondsSinceProgramStart ()
      in
        print ("gpu manager exec work elapsed: " ^ Int64.toString (t1-t0) ^ "us\n");
        result
      end


    fun announceCurrentThreadPendingChoice () =
      if queueSize () > 0 then
        ( moveAllTasksIntoSideQueue ()
        ; setQueueDepth (myWorkerId ()) (HH.getDepth (Thread.current ()))
        ; clear ()
        ; announceCurrentThreadPendingChoice () (* this should succeed on second go, now that main deque is empty *)
        )
      else
        let
          val thread = Thread.current ()
          val depth = HH.getDepth thread
          val selfTask = Continuation (thread, depth)
          val sendSucceeded = tryPushPendingChoice (myWorkerId ()) selfTask
          (* val sendSucceeded = RingBuffer.pushBot (hybridTaskQueue, selfTask) *)
        in
          if sendSucceeded then returnToSched () else ()
        end


    fun choice (args as {cpu, gpu = gpuTask}) =
      let
        val _ = tryBecomeChoiceSearcher ()
        val _ = announceCurrentThreadPendingChoice ()
      in
        (* When we resume here, the gpu manager has already had a chance to
         * to consider this choice point. Resuming on a cpu means that the
         * manager decided to return the task to the cpus.
         *)
        if not (isActiveChoice (Thread.current ())) then
          cpu ()
        else
          let
            (* val _ = dbgmsg'' (fn _ => "choice: gpu after send") *)
            val cleanup = doGpuTask gpuTask

            (* after finishing gpu task, we expect to run CPU code again, so try to
             * send this task back to a CPU thread.
             *)
            val thread = Thread.current ()
            val depth = HH.getDepth thread
            val selfTask = Continuation (thread, depth)
            val _ = finishActiveChoice ()
            val sendSucceeded =
              (*RingBuffer.pushBot (hybridDoneQueue, selfTask)*)
              pushSide selfTask
          in
            if sendSucceeded then
              ((*dbgmsg'' (fn _ => "choice: send back succeeded");*) returnToSched ())
            else
              die (fn _ => "scheduler: choice: send back failed\n");

            (* dbgmsg'' (fn _ => "choice: after send back"); *)
            cleanup ()
          end
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

      
      fun randomId () =
        SMLNJRandom.randRange (0, P-1) myRand

      
      fun gpuManager_tryFindWorkLoop_policy_minDepth () =
        let
          fun check (bestFriend, bestDepth, holding) friend =
            case peekPendingChoice friend of
              SOME (Continuation (_, d)) =>
                if d >= bestDepth then
                  (bestFriend, bestDepth, holding)
                else
                  ( case tryStealPendingChoice friend of
                      SOME (holding' as Continuation (_, d')) =>
                        if d' >= bestDepth then
                          ( tryPushPendingChoice friend holding'
                          ; (bestFriend, bestDepth, holding)
                          )
                        else
                          ( Option.app
                              (fn t =>
                                (tryPushPendingChoice bestFriend t; ()))
                              holding
                          ; (friend, d', SOME holding')
                          )

                    | NONE => (bestFriend, bestDepth, holding)
                  )

            | _ => (bestFriend, bestDepth, holding)
          

          fun loop xxx i =
            if i >= P then xxx else loop (check xxx i) (i+1)

          val (_, _, holding) = loop (~1, valOf Int.maxInt, NONE) 0
        in
          case holding of
            SOME t => SOME t
          | NONE => NONE
        end

(*
      fun gpuManagerFindWorkLoop_policy_randomSteal tries =
        if tries = P * 100 then
          ( hiccup ()
          ; gpuManagerFindWorkLoop_policy_randomSteal 0
          )
        else
          let
            val friend = randomOtherId ()
          in
            case tryStealPendingChoice friend of
              NONE => gpuManagerFindWorkLoop_policy_randomSteal (tries+1)
            | SOME t => t
          end
*)


      (* SAM_NOTE: TODO: currently we don't guarantee that the pending choice
       * queues are sorted by depth! When it goes idle, a worker might have
       * pending choices, but then might steal other CPU work which generates
       * more pending choices, but at different depths. These all go into
       * the same queue, so the order is not maintained.
       *)
      fun workerFindWork_tryKeepChoicePoint_maxdepth () =
        let
          fun check (bestFriend, bestDepth) friend =
            if friend = bestFriend then (false, (bestFriend, bestDepth)) else
            case peekBotPendingChoice friend of
              SOME (Continuation (_, d)) =>
                ( true
                , if d > bestDepth then (friend, d)
                  else (bestFriend, bestDepth)
                )
            | _ => (false, (bestFriend, bestDepth))
          
          fun loop (bf, bd) numFound tries =
            if tries >= 100 * P orelse numFound >= 10 then
              (bf, bd)
            else
              let
                val (foundOne, (bf', bd')) = check (bf, bd) (randomId ())
                val numFound' = if foundOne then numFound+1 else numFound
              in
                loop (bf', bd') numFound' (tries+1)
              end

          val (bf, bd) = loop (~1, valOf Int.minInt) 0 0
        in
          if bf < 0 then
            NONE
          else
            (* tryKeepPendingChoice bf *)
            case tryKeepPendingChoice bf of
              NONE => NONE
            | SOME (t as Continuation (_, d)) =>
                if d >= bd then SOME t
                else if tryPushPendingChoice bf t then NONE
                else die (fn _ => "scheduler error: push pending back failed\n")
        end


      fun workerFindWork () =
        let
          fun stealLoop tries lastTick =
            if tries >= P * 100 then
              NONE
            else
            let
              val friend = randomOtherId ()
              
              (*
              val lastTick =
                if tries mod 10 <> 9 then
                  lastTick
                else
                let
                  val tickNow = timeNowMicrosecondsSinceProgramStart ()
                  val delta = tickNow - lastTick
                in
                  addStealTime delta;
                  tickNow
                end
              *)
            in
              case trySteal friend of
                SOME task => SOME task
              | NONE =>
              case tryStealSide friend of
                SOME task => SOME task
              | NONE => stealLoop (tries+1) lastTick
            end


          fun actualStealLoop rounds =
            if rounds >= 2 then
              case workerFindWork_tryKeepChoicePoint_maxdepth () of
                SOME t => t
              | NONE => (hiccup (); actualStealLoop 0)
            else
              case stealLoop 0 (timeNowMicrosecondsSinceProgramStart ()) of
                SOME t => t
              | NONE => (hiccup (); actualStealLoop (rounds+1))
        in
          case tryPopSide myId of
            SOME task => task
          | NONE => actualStealLoop 0
        end


      fun findWork () =
        if not (shouldSearchForChoices ()) then
          workerFindWork ()
        else
          (* let
            val t0 = timeNowMicrosecondsSinceProgramStart ()
            val result = 
            val t1 = timeNowMicrosecondsSinceProgramStart ()
          in
            print ("gpu manager find work elapsed: " ^ Int64.toString (t1-t0) ^ "us\n");
            result
          end *)
          case gpuManager_tryFindWorkLoop_policy_minDepth () of
            SOME t =>
              ( startActiveChoice t
              ; t
              )
          | NONE =>
              ( markNoPendingChoices ()
              ; if existsPendingChoice () andalso tryBecomeChoiceSearcher () then
                  findWork ()
                else
                  workerFindWork ()
              )
          

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
          val task = findWork ()
        in
          case task of
            GCTask (thread, hh) =>
              ( HH.collectThreadRoot (thread, !hh)
              ; acquireWork ()
              )
          | Continuation (thread, depth) =>
              ( (*dbgmsg' (fn _ => "stole continuation (" ^ Int.toString depth ^ ")")
              ; dbgmsg' (fn _ => "resume task thread")
              ;*) Queue.setDepth myQueue depth
              ; threadSwitch thread
              ; afterReturnToSched ()
              ; Queue.setDepth myQueue 1
              ; acquireWork ()
              )
          | NormalTask (t, depth) =>
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
  val _ = HH.forceLeftHeap (myWorkerId (), originalThread)

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

  val maxForkDepthSoFar = Scheduler.maxForkDepthSoFar
end

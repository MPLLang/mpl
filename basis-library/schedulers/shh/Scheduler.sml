(* Author: Sam Westrick (swestric@cs.cmu.edu) *)

(* Scheduler implements a single structure.
 *   ForkJoin : FORK_JOIN
 * It is pulled out of Scheduler at the bottom of this file. *)
structure Scheduler =
struct

  structure Thread = MLton.Thread.Basic
  fun threadSwitch t =
    ( Thread.atomicBegin ()
    ; Thread.switchTo t
    )

  structure HM = MLton.HM
  structure HH = MLton.Thread.HierarchicalHeap

  val P = MLton.Parallel.numberOfProcessors
  val myWorkerId = MLton.Parallel.processorNumber

  (* ----------------------------------------------------------------------- *
   * ------------------------------ DEBUGGING ------------------------------ *
   * ----------------------------------------------------------------------- *)

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

  (* ----------------------------------------------------------------------- *
   * ----------------------------------------------------------------------- *
   * ----------------------------------------------------------------------- *)

  val vcas = MLton.Parallel.arrayCompareAndSwap
  fun cas (a, i) (old, new) = (vcas (a, i) (old, new) = old)

  fun faa (r, d) = MLton.Parallel.fetchAndAdd r d

  (* TODO: Implement a faster queue? Is this necessary? *)
  (*structure Queue = SimpleQueue*)
  (* structure Queue = DoublyLinkedList *)
  (* structure Queue = MkRingBuffer (val initialCapacity = 1024) *)
  structure Queue = ArrayQueue

  fun newIncounter () = ref 2

  fun decrementHitsZero (x : int ref) : bool =
    faa (x, ~1) = 1

  fun arraySub (a, i) = Array.sub (a, i)
  fun arrayUpdate (a, i, x) = Array.update (a, i, x)
  fun vectorSub (v, i) = Vector.sub (v, i)

  (* SAM_NOTE: TODO: does padding statuses and requestCells actually improve
   * performance? The intuition is that it avoids false sharing. *)
  val padding = 16

  (* A request is either NO_REQUEST, REQUEST_BLOCKED, or a processor id.
   * Workers request work by writing their own id into another worker's
   * request cell. If a worker is idle, it blocks requests from other workers
   * by changing its own request cell to REQUEST_BLOCKED. *)
  val NO_REQUEST = ~1
  val REQUEST_BLOCKED = ~2
  val requestCells = Array.array (P*padding, NO_REQUEST)
  fun getRequest p = arraySub (requestCells, p*padding)
  fun setRequest (p, r) = arrayUpdate (requestCells, p*padding, r)
  fun casRequest (p, r, r') = cas (requestCells, p*padding) (r, r')

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

  (* fun startTimer _ = ()
  fun tickTimer _ = ()
  fun stopTimer _ = () *)

  (* Statuses are updated locally to indicate whether or not work is available
   * to be stolen. This allows idle workers to only request work from victims
   * who are unlikely to reject. *)
  val statuses = Array.array (P*padding, false)
  fun getStatus p = arraySub (statuses, p*padding)
  fun setStatus (p, s) = arrayUpdate (statuses, p*padding, s)

  val mailboxes : ((unit -> unit) * HH.t) option Mailboxes.t =
    Mailboxes.new NONE

  (* When each worker becomes idle, it "preps" a new thread that it plans to
   * switch to as soon as it receives work. When work is dealt from worker A
   * to worker B, worker A will attach B's prepped thread as a child of A's
   * current hierarchical heap. *)
  (* val preppedThreads = Array.array (P, NONE) *)

  (* val push : task -> unit
   * push onto the current work queue *)
  val pushFuncs = Array.array (P, fn _ => die (fn _ => "Error: dummy push"))
  fun push x = arraySub (pushFuncs, myWorkerId ()) x

  (* val popDiscard : unit -> bool
   * Attempts to pop a task off the task queue. If it fails (because the queue
   * is empty) then the desired task must have been served to another worker. *)
  val popDiscardFuncs = Array.array (P, fn _ => (die (fn _ => "Error: dummy popDiscard"); false))
  fun popDiscard () = arraySub (popDiscardFuncs, myWorkerId ()) ()

  val communicateFuncs = Array.array (P, fn _ => die (fn _ => "Error: dummy communicate"))
  fun schedCommunicate () = arraySub (communicateFuncs, myWorkerId ()) ()

  val returnToScheds = Array.array (P, fn _ => die (fn _ => "Error: dummy returnToSched"))
  fun returnToSched x = arraySub (returnToScheds, myWorkerId ()) x

  (* ----------------------------------------------------------------------- *
   * ------------------------------ FORK-JOIN ------------------------------ *
   * ------------------------------------------------------------------------*)

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

    val communicate = schedCommunicate
    val getIdleTime = getIdleTime

    (* Must be called from a "user" thread, which has an associated HH *)
    fun fork (f : unit -> 'a, g : unit -> 'b) =
      let
        val thread = Thread.current ()
        val level = HH.getLevel thread

        val rightSide = ref (NONE : 'b result option)
        val incounter = newIncounter ()

        fun g' () =
          ( rightSide := SOME (result g)
          ; returnToSched (incounter, thread)
          )

        val _ = push (g', level)
        val _ = HH.setLevel (thread, level + 1)

        val _ = communicate ()
        val fr = result f
        val _ = communicate ()

        val gr =
          if popDiscard () then
            result g
          else
            ( returnToSched (incounter, thread)
            ; HH.mergeDeepestChild thread
            ; case !rightSide of
                NONE => die (fn _ => "scheduler bug: join failed")
              | SOME gr => gr
            )

        val _ = HH.promoteChunks thread
        val _ = HH.setLevel (thread, level)
      in
        (extractResult fr, extractResult gr)
      end

  end

  (* ----------------------------------------------------------------------- *
   * ------------------------- WORKER-LOCAL SETUP -------------------------- *
   * ----------------------------------------------------------------------- *)

  (* We maintain a distinction between
   *   - "scheduler" threads, which never are migrated between processors and
   *   are used to acquire new work when the processor becomes idle, and
   *   - "user" threads, which run user code and are migrated between processors
   *)
  fun setupSchedLoop () =
    let
      val myId = myWorkerId ()
      val myQueue = Queue.new myId
      val myRand = SimpleRandom.rand myId
      val mySchedThread = Thread.current ()
      val myRetArg = ref NONE

      (* this widget makes it possible to create new "user" threads by copying
       * the prototype thread and writing the piece of work which should be
       * executed into the `myTodo` cell *)
      val myTodo : (unit -> unit) option ref = ref NONE
      val _ = Thread.copyCurrent ()
      val prototype : Thread.p =
        case !myTodo of
          NONE => Thread.savedPre ()
        | SOME f =>
            ( myTodo := NONE
            ; f () handle e => MLton.Exn.topLevelHandler e
            ; die (fn _ => "scheduler bug: thread didn't exit properly")
            )

      (* the lock is not necessary for private deques, but need to do this to
       * play nice with runtime. *)
      val dummyLock : Word32.word ref = ref 0w0
      val _ = MLton.HM.registerQueueLock (Word32.fromInt myId, dummyLock)
      val _ = MLton.Parallel.Deprecated.lockInit dummyLock

      fun communicate () =
        ( let
            val r = getRequest myId
          in
            if r = NO_REQUEST then
              ()
            else if r = REQUEST_BLOCKED then
              die (fn _ => "scheduler bug: serve while blocked")
            else
              (* r is a friendly processor id which is requesting work *)
              ( setRequest (myId, NO_REQUEST)
              ; case Queue.popBack myQueue of
                  NONE => Mailboxes.sendMail mailboxes (r, NONE)
                | SOME (task, level) =>
                    let
                      val ch = HH.newHeap ()
                    in
                      HH.attachChild (Thread.current (), ch, level);
                      Mailboxes.sendMail mailboxes (r, SOME (task, ch))
                    end
              )
          end
        ; setStatus (myId, not (Queue.empty myQueue))
        )

      fun push x =
        Queue.pushFront (x, myQueue)

      fun popDiscard () =
        case Queue.popFront myQueue of
          NONE => false
        | SOME _ => true

      (* ------------------------------------------------------------------- *)

      fun verifyStatus () =
        if getStatus myId = false then ()
        else die (fn _ => "scheduler bug: status not set correctly while idle")

      fun randomOtherId () =
        let val other = SimpleRandom.boundedInt (0, P-1) myRand
        in if other < myId then other else other+1
        end

      fun blockRequests () =
        let
          val r = getRequest myId
        in
          if r = NO_REQUEST then
            if casRequest (myId, NO_REQUEST, REQUEST_BLOCKED) then ()
            else blockRequests () (* recurs at most once *)
          else if r = REQUEST_BLOCKED then
            die (fn _ => "scheduler bug: attempted to block while already blocked")
          else
            ( setRequest (myId, REQUEST_BLOCKED)
            ; Mailboxes.sendMail mailboxes (r, NONE)
            )
        end

      fun unblockRequests () = setRequest (myId, NO_REQUEST)

      fun request idleTimer =
        let
          val friend = randomOtherId ()
          val hasWork = getStatus friend
          val available = (getRequest friend = NO_REQUEST)
        in
          if not (available andalso hasWork andalso casRequest (friend, NO_REQUEST, myId))
          then (verifyStatus (); request (tickTimer idleTimer))
          else case Mailboxes.getMail mailboxes myId of
                 NONE => (verifyStatus (); request (tickTimer idleTimer))
               | SOME m => (m, idleTimer)
        end

      (* ------------------------------------------------------------------- *)

      fun acquireWork () : unit =
        let
          val idleTimer = startTimer myId
          val _ = setStatus (myId, false)
          val _ = blockRequests ()

          (* find work from another worker. Eventually we receive
           *   - a task to execute
           *   - a hierarchical heap to execute it in
           *)
          val ((task, hh), idleTimer') = request idleTimer

          val _ = unblockRequests ()
          val _ = stopTimer idleTimer'

          val taskThread = Thread.copy prototype
          val _ = HH.attachHeap (taskThread, hh);

          (* The taskThread is a copy of this worker's prototype thread, which
           * is set up to immediately check myTodo to look for a function to
           * execute. So, by setting myTodo and then switching, we execute the
           * given task. *)
          val _ = myTodo := SOME task
          val _ = threadSwitch taskThread
        in
          returnFromExecute ()
        end

      and returnFromExecute () =
        case !myRetArg of
          NONE => die (fn _ => "scheduler bug: no arg when returning to scheduler")
        | SOME (incounter, cont) =>
            ( myRetArg := NONE
            ; if decrementHitsZero incounter
              then (communicate (); threadSwitch cont; returnFromExecute ())
              else acquireWork ()
            )

      fun returnToSched (c, k) =
        ( myRetArg := SOME (c, k)
        ; threadSwitch mySchedThread
        )

      (* ------------------------------------------------------------------- *)

      val _ = arrayUpdate (pushFuncs, myId, push)
      val _ = arrayUpdate (popDiscardFuncs, myId, popDiscard)
      val _ = arrayUpdate (communicateFuncs, myId, communicate)
      val _ = arrayUpdate (returnToScheds, myId, returnToSched)

      (* val _ = dbgmsg (fn _ => "sched " ^ Int.toString myId ^ " finished init") *)

    in
      if myId = 0 then returnFromExecute else acquireWork
    end

  (* ----------------------------------------------------------------------- *
   * --------------------------- INITIALIZATION ---------------------------- *
   * ----------------------------------------------------------------------- *)

  fun sched () =
    let val acquireWork = setupSchedLoop ()
    in acquireWork ()
    end

  val _ = MLton.Parallel.registerProcessorFunction sched
  val _ = MLton.Parallel.initializeProcessors ()

  (* Initializes scheduler-local data for proc 0, including remembering the
   * current thread as the "scheduler thread" for this worker. In order to
   * keep "user" threads separate from "scheduler" threads, we need to copy
   * the current thread and use the COPY as the main program thread, which
   * happens below. *)
  val returnFromExecute = setupSchedLoop ()

  (* This manages to hijack the "original" program thread as the scheduler
   * thread, while the copied thread is used to execute the actual program.
   * Before switching to the copy, we give the copy a hierarchical heap. *)
  val executeMain = ref false
  val _ = Thread.copyCurrent ()
  val _ =
    if !executeMain then ()
    else let
           val t = Thread.copy (Thread.savedPre ())
         in
           ( executeMain := true
           ; HH.attachHeap (t, HH.newHeap ())
           ; threadSwitch t
           ; returnFromExecute ()
           )
         end

end

structure ForkJoin :> FORK_JOIN =
struct
  open Scheduler.ForkJoin

  fun for (i, j) f = if i = j then () else (f i; for (i+1, j) f)

  fun parfor grain (i, j) f =
    let val n = j - i
    in if n <= grain
       then for (i, j) f
       else ( fork ( fn _ => parfor grain (i, i + n div 2) f
                   , fn _ => parfor grain (i + n div 2, j) f
                   )
            ; ()
            )
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

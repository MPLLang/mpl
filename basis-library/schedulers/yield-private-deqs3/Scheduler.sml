(* Author: Sam Westrick (swestric@cs.cmu.edu) *)

(* Scheduler implements a single structure.
 *   ForkJoin : FORK_JOIN
 * It is pulled out of Scheduler at the bottom of this file. *)
structure Scheduler =
struct

  val myYield  = _import "Parallel_myYield"  runtime private : unit -> unit;
  (* val mySemWait = _import "Parallel_mySemWait" runtime private : int -> unit; *)

  val myLLSleep      = _import "Parallel_myLLSleep"      runtime private : int -> int;
  val myLLTimedSleep = _import "Parallel_myLLTimedSleep" runtime private : int * int -> int;
  val myLLSignal     = _import "Parallel_myLLSignal"     runtime private : int -> unit;
  val myLLFindChild  = _import "Parallel_myLLFindChild"  runtime private : int -> int;
  val myLLSignalSpec = _import "Parallel_myLLSignalSpec" runtime private : int -> unit;
  (* val myLLWait       = _import "Parallel_myLLWait"       runtime private : int -> int; *)
  (* val myLLLock       = _import "Parallel_myLLLock"       runtime private : int -> unit; *)
  (* val myLLUnlock     = _import "Parallel_myLLUnlock"     runtime private : int -> unit; *)

  val myProbe  = _import "Parallel_myProbe" runtime private : unit -> unit;

  val SLEEP_PERIOD = 500 (* us *)

  val P = MLton.Parallel.numberOfProcessors
  val myWorkerId = MLton.Parallel.processorNumber

  (* ----------------------------------------------------------------------- *
   * ------------------------------ DEBUGGING ------------------------------ *
   * ----------------------------------------------------------------------- *)

  fun die strfn =
    ( print (Int.toString (myWorkerId ()) ^ ": " ^ strfn ())
    ; OS.Process.exit OS.Process.failure
    ; ()
    )

  (* ----------------------------------------------------------------------- *
   * ----------------------------------------------------------------------- *
   * ----------------------------------------------------------------------- *)

  structure Thread = MLton.Thread
  val vcas = MLton.Parallel.compareAndSwap
  fun cas (r, old, new) = vcas r (old, new) = old

  fun faa (r, d) = MLton.Parallel.fetchAndAdd r d

  (* TODO: Implement a faster queue? Is this necessary? *)
  (*structure Queue = SimpleQueue*)
  structure Queue = DoublyLinkedList

  type vertex = int ref * unit Thread.t ref
  type task = unit -> unit

  fun dummyTask () = die (fn _ => "Error: dummy task")
  val dummyThread = Thread.new dummyTask

  fun runnable (k : unit Thread.t) = Thread.prepare (k, ())
  fun jumpTo (k : unit Thread.t) = Thread.switch (fn _ => runnable k)

  fun decrementHitsZero (x : int ref) : bool =
    faa (x, ~1) = 1

  (*fun increment (x : int ref) : unit =
    ignore (faa (x, 1))*)

  (*
  fun arraySub str (a, i) = Array.sub (a, i) handle e => (Atomic.print (fn _ => "Array.sub (" ^ str ^ ", " ^ Int.toString i ^ ")\n"); raise e)
  fun arrayUpdate str (a, i, x) = Array.update (a, i, x) handle e => (Atomic.print (fn _ => "Array.update (" ^ str ^ ", " ^ Int.toString i ^ ", ...)\n"); raise e)
  fun vectorSub str (v, i) = Vector.sub (v, i) handle e => (Atomic.print (fn _ => "Vector.sub (" ^ str ^ ", " ^ Int.toString i ^ ")\n"); raise e)
  *)

  fun arraySub str (a, i) = Array.sub (a, i)
  fun arrayUpdate str (a, i, x) = Array.update (a, i, x)
  fun vectorSub str (v, i) = Vector.sub (v, i)

  (* A request is either NO_REQUEST, REQUEST_BLOCKED, or a processor id.
   * Workers request work by writing their own id into another worker's
   * request cell. If a worker is idle, it blocks requests from other workers
   * by changing its own request cell to REQUEST_BLOCKED. *)
  val NO_REQUEST = ~1
  val REQUEST_BLOCKED = ~2
  val requestCells = Vector.tabulate (P, fn _ => ref NO_REQUEST)
  fun requestCell p = vectorSub "requestCells" (requestCells, p)

  (* Statuses are updated locally to indicate whether or not work is available
   * to be stolen. This allows idle workers to only request work from victims
   * who are unlikely to reject.
   * TODO: does padding statuses actually improve performance? *)
  val statuses = Array.array (P*16, false)
  fun getStatus p = arraySub "statuses" (statuses, p*16)
  fun setStatus (p, s) = arrayUpdate "statuses" (statuses, p*16, s)

  val mailboxes : task option Mailboxes.t = Mailboxes.new NONE

  (* val push : task * vertex -> unit
   * `push (t,v)` registers t as a dependency for v, and pushes t onto the task
   * stack. v should be thought of as a join vertex which has not yet been
   * assigned some computation. *)
  val pushFuncs = Array.array (P, fn _ => die (fn _ => "Error: dummy push"))
  fun push (t, v) = arraySub "pushFuncs" (pushFuncs, myWorkerId ()) (t, v)

  (* val popDiscard : unit -> bool
   * Attempts to pop a task off the task stack. If it fails (because the stack
   * is empty) then the desired task must have been served to another worker. *)
  val popDiscardFuncs = Array.array (P, fn _ => (die (fn _ => "Error: dummy popDiscard"); false))
  fun popDiscard () = arraySub "popDiscardFuncs" (popDiscardFuncs, myWorkerId ()) ()

  (* val returnToSched : vertex -> unit
   * `returnToSched v` is the same as `sync v`, except it doesn't assign the
   * current continuation to v. Typically for a single v, there is one call to
   * `sync v` and one or more calls to `returnToSched v`. Whoever gets there
   * last will be the one who executes v. *)
  val returnFuncs = Array.array (P, fn _ => die (fn _ => "Error: dummy return"))
  fun returnToSched v = arraySub "returnFuncs" (returnFuncs, myWorkerId ()) v

  (* val sync : vertex -> unit
   * `sync v` assigns the current continuation to v, decrements its counter,
   * and executes v if the counter hits zero. *)
  (* TODO: the Thread.switch bug has been fixed, so it should be possible to
   * not create a new thread in the case that our decrement hits zero. So, some
   * of the code from `return` should be blended here. *)
  fun sync (counter, cont) =
    Thread.switch (fn k =>
      ( cont := k (* this must happen before decrementing the counter! *)
      ; runnable (Thread.new (fn _ => returnToSched (counter, cont)))
      ))

  (* Create a new vertex (for join points) *)
  fun new () = (ref 2, ref dummyThread)

  (* ----------------------------------------------------------------------- *
   * ------------------------------ FORK-JOIN ------------------------------ *
   * ------------------------------------------------------------------------*)

  structure ForkJoin :> FORK_JOIN =
  struct

    exception ForkJoin

    datatype 'a result =
      Waiting
    | Finished of 'a
    | Raised of exn

    fun writeResult fr f () =
      fr := (Finished (f ()) handle e => Raised e)

    fun fork (f : unit -> 'a, g : unit -> 'b) =
      let
        val gr = ref Waiting
        val join = new ()
        val _ = push (writeResult gr g, join)
        val a = f ()
      in
        if popDiscard () then (a, g ())
        else ( sync join
             ; case !gr of
                 Finished b => (a, b)
               | Raised e => raise e
               | Waiting => raise ForkJoin
             )
      end

  end

  (* ----------------------------------------------------------------------- *
   * ------------------------- WORKER-LOCAL SETUP -------------------------- *
   * ----------------------------------------------------------------------- *)

  fun init myId =
    let
      val myQueue = Queue.new ()
      val myRand = SimpleRandom.rand myId
      val myRequestCell = requestCell myId

      fun communicate () =
        ( let
            val friend = !myRequestCell
          in
            if friend = NO_REQUEST then ()
            else if friend = REQUEST_BLOCKED then die (fn _ => "Error: serve while blocked")
            else ( myRequestCell := NO_REQUEST
                 ; let val mail =
                         case Queue.popTop myQueue of
                           NONE => NONE
                         | SOME (task, v) =>
                             (* New counters start at 2 now, so we don't need to
                              * increment the counter at a steal. *)
                             SOME (fn () => (task (); returnToSched v))
                   in Mailboxes.sendMail mailboxes (friend, mail)
                   end
                 )
          end
        ; setStatus (myId, not (Queue.empty myQueue))
        )

      (* derictly send mail to sleeping children *)
      fun push (t, v) =
        let
          val chld = myLLFindChild (myId)
        in
          if chld = ~1 then
            Queue.pushBot ((t, v), myQueue)
            before communicate ()
          else
            let
              val _ = Queue.pushBot ((t, v), myQueue) (* push the task into the bottom of the deque *)
              val mail = 
                case Queue.popTop myQueue of (* give the top task to the selected child, this matters *)
                  NONE => NONE
                | SOME (task, v) =>
                    SOME (fn () => (task (); returnToSched v))
            in (Mailboxes.sendMailLockFree mailboxes (chld, mail); (* now the child must be sleeping, so we won't need locks here *)
                myLLSignalSpec (chld); (* wake up the selected child *)
                communicate ();
                ())
            end
        end

      fun popDiscard () =
        Queue.popBotDiscard myQueue
        before communicate ()

      (* ------------------------------------------------------------------- *)

      fun verifyStatus () =
        if getStatus myId = false then ()
        else die (fn _ => "Error: status not set correctly\n")

      fun randomOtherId () =
        let val other = SimpleRandom.boundedInt (0, P-1) myRand
        in if other < myId then other else other+1
        end

      fun blockRequests () =
        let
          val friend = !myRequestCell
        in
          if friend = NO_REQUEST then
            if cas (myRequestCell, NO_REQUEST, REQUEST_BLOCKED) then ()
            else blockRequests () (* recurs at most once *)
          else if friend = REQUEST_BLOCKED then die (fn _ => "Error: block while blocked")
          else
            ( myRequestCell := REQUEST_BLOCKED
            ; Mailboxes.sendMail mailboxes (friend, NONE)
            )
        end

      fun unblockRequests () = myRequestCell := NO_REQUEST

      val YIELD_CNT = 32
      fun modY x = if x >= YIELD_CNT then x - YIELD_CNT else x

      (* old version that the waked up thread will steal from its parents *)
      (* fun requestCnt (cnt) =
        let
          val _ = myProbe()
          val victimId = 
                if cnt < (YIELD_CNT-1) then
                  randomOtherId()
                else  (* cnt >= (YIELD_CNT-1) *)
                  let
                    val tgt = myLLSleep(myId)
                    (* fun loop () = 
                      let
                        val tgt = myLLWait(myId)
                      in if tgt = ~1 then loop ()
                        else tgt
                      end
                    val _ = myLLLock(myId)
                    val tgt = loop ()
                    val _ = myLLUnlock(myId) *)
                  in
                    if tgt < 0 then randomOtherId() else tgt
                  end
          val hasWork = getStatus victimId
        in
          if not (hasWork andalso cas (requestCell victimId, NO_REQUEST, myId))
          then (verifyStatus (); 
                requestCnt (modY(cnt+1)))
          else case Mailboxes.getMail mailboxes myId of
                 NONE => (verifyStatus ();
                          requestCnt (modY(cnt+1)))
               | SOME task => task
        end *)

      fun requestCnt (cnt) =
        if cnt >= (YIELD_CNT-1) then
            let
              val tgt = myLLSleep myId
            in
              if tgt < 0 then requestCnt (0)
              else case Mailboxes.getMail mailboxes myId of
                NONE => (verifyStatus ();
                         requestCnt (0))
              | SOME task => task
            end
        else
          let
            val _ = myProbe()
            val victimId = randomOtherId()
            val hasWork = getStatus victimId
          in
            if not (hasWork andalso cas (requestCell victimId, NO_REQUEST, myId))
            then (verifyStatus (); 
                  requestCnt (modY(cnt+1)))
            else  case Mailboxes.getMail mailboxes myId of
                    NONE => (verifyStatus ();
                            requestCnt (modY(cnt+1)))
                  | SOME task => task
          end

      (* fun request () =
        let
          val _ = myProbe()
          val victimId = randomOtherId ()
          val hasWork = getStatus victimId
        in
          if not (hasWork andalso cas (requestCell victimId, NO_REQUEST, myId))
          then (verifyStatus ();
                (* myYield (); *)
                (* request ()) *)
                requestCnt (1))
          else case Mailboxes.getMail mailboxes myId of
                 NONE => (verifyStatus (); 
                          (* myYield (); *)
                          (* request ()) *)
                          requestCnt (1))
               | SOME task => task
        end *)

      (* ------------------------------------------------------------------- *)
      
      fun acquireWork () =
        ( setStatus (myId, false)
        ; blockRequests ()
        ; let 
            val task = requestCnt (0)
          in ( unblockRequests ()
             ; task ()
             )
          end
        )

      fun return (counter, cont) =
        if decrementHitsZero counter
        then (communicate (); jumpTo (!cont))
        else acquireWork ()

    in
      ( arrayUpdate "pushFuncs" (pushFuncs, myId, push)
      ; arrayUpdate "popDiscardFuncs" (popDiscardFuncs, myId, popDiscard)
      ; arrayUpdate "returnFuncs" (returnFuncs, myId, return)
      ; acquireWork
      )
    end (* init *)

  (* ----------------------------------------------------------------------- *
   * --------------------------- INITIALIZATION ---------------------------- *
   * ----------------------------------------------------------------------- *)

  fun sched () = init (myWorkerId ()) ()

  val _ = MLton.Parallel.registerProcessorFunction sched
  val _ = MLton.Parallel.initializeProcessors ()

  (* init sets up worker-local data and returns the acquireWork function. This
   * allows us to set up the acquire loop for the other workers. Since the
   * current worker is the "master" worker, we ignore the returned value (we
   * only need to make sure it executes its setup code *)
  val _ = init (myWorkerId ())

end

structure ForkJoin :> FORK_JOIN = Scheduler.ForkJoin

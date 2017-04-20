(* Author: Sam Westrick (swestric@cs.cmu.edu) *)

(* Scheduler implements a single structure.
 *   ForkJoin : FORK_JOIN
 * It is pulled out of Scheduler at the bottom of this file. *)
structure Scheduler =
struct

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
  val cas = MLton.Parallel.compareAndSwap

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
    MLton.Parallel.fetchAndAdd (x, ~1) = 1

  (*fun increment (x : int ref) : unit =
    ignore (MLton.Parallel.fetchAndAdd (x, 1))*)

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

      fun push (t, v) =
        Queue.pushBot ((t, v), myQueue)
        before communicate ()

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

      fun request () =
        let
          val victimId = randomOtherId ()
          val hasWork = getStatus victimId
        in
          if not (hasWork andalso cas (requestCell victimId, NO_REQUEST, myId))
          then (verifyStatus (); request ())
          else case Mailboxes.getMail mailboxes myId of
                 NONE => (verifyStatus (); request ())
               | SOME task => task
        end

      (* ------------------------------------------------------------------- *)

      fun acquireWork () =
        ( setStatus (myId, false)
        ; blockRequests ()
        ; let val task = request ()
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

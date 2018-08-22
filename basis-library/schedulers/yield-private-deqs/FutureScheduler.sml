(* Author: Sam Westrick (swestric@cs.cmu.edu) *)

(* FutureScheduler implements two structures.
 *   ForkJoin : FORK_JOIN
 *   Future : FUTURE
 * These are pulled out of FutureScheduler at the bottom of this file. *)
structure FutureScheduler =
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
  val vcas = MLton.Parallel.compareAndSwap
  fun cas (r, old, new) = vcas r (old, new) = old

  fun faa (r, d) = MLton.Parallel.fetchAndAdd r d

  (* TODO: Implement a faster queue? Is this necessary? *)
  (*structure Queue = SimpleQueue*)
  structure Queue = DoublyLinkedList

  structure Bag = ListBag

  type vertex = int ref * unit Thread.t ref

  datatype task =
    Thunk of (unit -> unit) * vertex
    (* Threads keep track locally of how many pushes they've done. *)
  | Thread of unit Thread.t * int

  fun dummyThunk () = die (fn _ => "Error: dummy thunk")
  val dummyThread = Thread.new dummyThunk

  fun runnable (k : unit Thread.t) = Thread.prepare (k, ())
  fun jumpTo (k : unit Thread.t) = Thread.switch (fn _ => runnable k)

  fun decrementHitsZero (x : int ref) : bool =
    faa (x, ~1) = 1

  fun increment (x : int ref) : unit =
    ignore (faa (x, 1))

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

  val mailboxes : (unit -> unit) option Mailboxes.t = Mailboxes.new NONE

  (* val push : (unit -> unit) * vertex -> unit
   * `push (t,v)` registers t as a dependency for v, and pushes t onto the task
   * stack. v should be thought of as a join vertex which has not yet been
   * assigned some computation. *)
  val pushFuncs = Array.array (P, fn _ => die (fn _ => "Error: dummy push"))
  fun push (t, v) = arraySub "pushFuncs" (pushFuncs, myWorkerId ()) (t, v)

  (* val popDiscard : unit -> bool
   * Attempts to pop a task off the current worker's task stack. It will fail
   * if the current task has previously suspended, or if the task stack is
   * empty. In the latter case, the desired task must have been served to
   * another worker. *)
  val popDiscardFuncs = Array.array (P, fn _ => (die (fn _ => "Error: dummy popDiscard"); false))
  fun popDiscard () = arraySub "popDiscardFuncs" (popDiscardFuncs, myWorkerId ()) ()

  (* val returnToSched : vertex -> unit
   * `returnToSched v` is the same as `sync v`, except it doesn't assign the
   * current continuation to v. Typically for a single v, there is one call to
   * `sync v` and one or more calls to `returnToSched v`. Whoever gets there
   * last will be the one who executes v. *)
  val returnFuncs = Array.array (P, fn _ => die (fn _ => "Error: dummy return"))
  fun returnToSched v = arraySub "returnFuncs" (returnFuncs, myWorkerId ()) v

  (* val returnFutureToSched : unit Thread.t Bag.t -> unit *)
  val returnFutureFuncs = Array.array (P, fn _ => die (fn _ => "Error: dummy future return"))
  fun returnFutureToSched b = arraySub "returnFutureFuncs" (returnFutureFuncs, myWorkerId ()) b

  (* val pushThread : unit Thread.t -> unit *)
  val pushThreadFuncs = Array.array (P, fn _ => die (fn _ => "Error: dummy push thread"))
  fun pushThread k = arraySub "pushThreadFuncs" (pushThreadFuncs, myWorkerId ()) k

  (* val suspend : unit -> unit *)
  val suspendFuncs = Array.array (P, fn _ => die (fn _ => "Error: dummy suspend"))
  fun suspend () = arraySub "suspendFuncs" (suspendFuncs, myWorkerId ()) ()

  (* val sync : vertex -> unit
   * `sync v` assigns the current continuation to v, decrements its counter,
   * and executes v if the counter hits zero. *)
  (* TODO: Can we prevent making a new thread here? *)
  (* NOTE: It is crucial that the continuation be assigned before the counter
   * is decremented; otherwise, this continuation could be switched *to* by
   * another worker before this switch completes. *)
  fun sync (counter, cont) =
    Thread.switch (fn k =>
      ( cont := k (* this must happen before decrementing the counter! *)
      ; runnable (Thread.new (fn _ => returnToSched (counter, cont)))
      ))

  (* Create a new join vertex *)
  fun new () = (ref 2, ref dummyThread)

  datatype 'a result =
    Waiting
  | Finished of 'a
  | Raised of exn

  fun writeResult fr f () =
    fr := (Finished (f ()) handle e => Raised e)

  (* ----------------------------------------------------------------------- *
   * ------------------------------ FORK-JOIN ------------------------------ *
   * ----------------------------------------------------------------------- *)

  structure ForkJoin :> FORK_JOIN =
  struct

    exception ForkJoin

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
   * ------------------------------- FUTURES ------------------------------- *
   * ----------------------------------------------------------------------- *)

  structure Future :> FUTURE =
  struct

    exception Future

    type 'a t = 'a result ref * unit Thread.t Bag.t

    fun run (fr, bag) f () =
      ( writeResult fr f ()
      ; returnFutureToSched bag
      )

    fun future f =
      let
        val fut = (ref Waiting, Bag.new ())
      in
        ( Thread.switch (fn k =>
            ( pushThread k
            ; runnable (Thread.new (run fut f))
            ))
        ; fut
        )
      end

    fun poll (result, bag) =
      if not (Bag.isDumped bag) then NONE
      else case !result of
             Finished x => SOME x
           | Raised e => raise e
           | Waiting => raise Future

    fun suspend' (bag, k) () =
      if Bag.insert (bag, k)
      then suspend ()
      else Thread.switch (fn _ => runnable k)

    fun force (fut as (result, bag)) =
      ( if Bag.isDumped bag then ()
        else Thread.switch (fn k => runnable (Thread.new (suspend' (bag, k))))
      ; case !result of
          Finished x => x
        | Raised e => raise e
        | Waiting => raise Future
      )

  end

  (* ----------------------------------------------------------------------- *
   * ------------------------- WORKER-LOCAL SETUP -------------------------- *
   * ----------------------------------------------------------------------- *)

  fun init myId =
    let
      val myQueue = Queue.new ()
      val myRand = SimpleRandom.rand myId
      val myRequestCell = requestCell myId
      val pushCounter = ref 0

      fun communicate () =
        ( let
            val friend = !myRequestCell
          in
            if friend = NO_REQUEST then ()
            else if friend = REQUEST_BLOCKED then die (fn _ => "Error: serve while blocked")
            else ( myRequestCell := NO_REQUEST
                 ; let
                     val mail =
                       case Queue.popTop myQueue of
                         NONE => NONE
                       | SOME (Thread (k, _)) => SOME (fn () => jumpTo k)
                       | SOME (Thunk (work, v)) =>
                           (* New counters start at 2 now, so we don't need to
                            * increment the counter at a steal. *)
                           SOME (fn () => (work (); returnToSched v))
                   in
                     Mailboxes.sendMail mailboxes (friend, mail)
                   end
                 )
          end
        ; setStatus (myId, not (Queue.empty myQueue))
        )

      fun push (t, v) =
        ( Queue.pushBot (Thunk (t, v), myQueue)
        ; pushCounter := !pushCounter + 1
        ; communicate ()
        )

      fun pushThread k =
        Queue.pushBot (Thread (k, !pushCounter), myQueue)
        before communicate ()

      fun popDiscard () =
        let val c = !pushCounter
        in if c = 0 then (communicate (); false)
           else (Queue.popBotDiscard myQueue before ( pushCounter := c - 1
                                                    ; communicate ()
                                                    ))
        end

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
               | SOME work => work
        end

      (* ------------------------------------------------------------------- *)

      fun acquireWork () =
        ( setStatus (myId, false)
        ; blockRequests ()
        ; let val work = request ()
          in ( unblockRequests ()
             ; pushCounter := 0
             ; work ()
             )
          end
        )

      fun suspend () =
        case Queue.popBot myQueue of
          NONE => acquireWork ()
        | SOME (Thread (k, c)) => (communicate (); pushCounter := c; jumpTo k)
        | SOME (Thunk (t, v)) =>
            (* Execute it as though it were parallel. This is fine because the
             * incounter at v was initialized to account for this thunk. Note
             * that this will probably no longer be correct if we incorporate
             * async/finish. *)
            (communicate (); pushCounter := 0; t (); returnToSched v)

      fun return (counter, cont) =
        if decrementHitsZero counter
        then (communicate (); jumpTo (!cont))
        else suspend () (* seems like it's possible to reuse this *)


      fun returnFuture bag =
        case Bag.dump bag of
          NONE => die (fn _ => "Error: returned from future but someone else dumped the bag.")

        | SOME (k :: ks) =>
            ( List.app (fn k' => Queue.pushBot (Thread (k', 0), myQueue)) ks
            ; communicate ()
            ; pushCounter := 0
            ; jumpTo k
            )

        | SOME [] =>
            case Queue.popBot myQueue of
              NONE => acquireWork ()
            | SOME (Thread (k, c)) => (communicate (); pushCounter := c; jumpTo k)
            | SOME (Thunk _) => die (fn _ => "Error: thunk on stack after return from future")

    in
      ( arrayUpdate "pushFuncs" (pushFuncs, myId, push)
      ; arrayUpdate "pushThreadFuncs" (pushThreadFuncs, myId, pushThread)
      ; arrayUpdate "popDiscardFuncs" (popDiscardFuncs, myId, popDiscard)
      ; arrayUpdate "suspendFuncs" (suspendFuncs, myId, suspend)
      ; arrayUpdate "returnFuncs" (returnFuncs, myId, return)
      ; arrayUpdate "returnFutureFuncs" (returnFutureFuncs, myId, returnFuture)
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

structure ForkJoin :> FORK_JOIN = FutureScheduler.ForkJoin
structure Future :> FUTURE = FutureScheduler.Future

(* Author: Sam Westrick (swestric@cs.cmu.edu) *)

structure Scheduler :> SCHEDULER =
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
  val dummyVert = (ref 1, ref dummyThread)

  fun decrementHitsZero (x : int ref) : bool =
    MLton.Parallel.fetchAndAdd (x, ~1) = 1

  fun increment (x : int ref) : unit =
    ignore (MLton.Parallel.fetchAndAdd (x, 1))

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

  val mailboxes : (task * vertex) option Mailboxes.t = Mailboxes.new NONE

  val pushFuncs = Array.array (P, fn _ => die (fn _ => "Error: dummy push func"))
  (*val popFuncs = Array.array (P, fn _ => (die (fn _ => "Error: dummy pop func"); NONE))*)
  val popDiscardFuncs = Array.array (P, fn _ => (die (fn _ => "Error: dummy popDiscard func"); false))
  val syncFuncs = Array.array (P, fn _ => die (fn _ => "Error: dummy yield func"))
  val returnFuncs = Array.array (P, fn _ => die (fn _ => "Error: dummy return func"))

  fun push (t, v) = arraySub "pushFuncs" (pushFuncs, myWorkerId ()) (t, v)
  (*fun pop x = arraySub "popFuncs" (popFuncs, myWorkerId ()) x*)
  fun popDiscard x = arraySub "popDiscardFuncs" (popDiscardFuncs, myWorkerId ()) x
  fun sync x = arraySub "syncFuncs" (syncFuncs, myWorkerId ()) x
  fun returnToSched x = arraySub "returnFuncs" (returnFuncs, myWorkerId ()) x

  fun new () = (ref 1, ref dummyThread)

  fun runnable (k : unit Thread.t) = Thread.prepare (k, ())
  fun jumpTo (k : unit Thread.t) = Thread.switch (fn _ => runnable k)

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
                           SOME (x as (_, (c, _))) => (increment c; SOME x)
                         | NONE => NONE
                   in Mailboxes.sendMail mailboxes (friend, mail)
                   end
                 )
          end
        ; setStatus (myId, not (Queue.empty myQueue))
        )

      fun push (t, v) =
        Queue.pushBot ((t, v), myQueue)
        before communicate ()

      (*fun pop () =
        Option.map (fn (t, _) => t) (Queue.popBot myQueue)
        before communicate ()*)

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

      fun request () =
        let
          val victimId = randomOtherId ()
          val hasWork = getStatus victimId
        in
          if not (hasWork andalso cas (requestCell victimId, NO_REQUEST, myId))
          then (verifyStatus (); request ())
          else case Mailboxes.getMail mailboxes myId of
                 NONE => (verifyStatus (); request ())
               | SOME x => (myRequestCell := NO_REQUEST; x)
        end

      (* ------------------------------------------------------------------- *)

      (* (counter, cont) is the outgoing dependency of the given task *)
      fun doWork (task, (counter, cont)) =
        ( communicate ()
        ; task ()
        (* When the task returns, we may have moved to a different worker.
         * returnToSched handles this by looking up the appropriate `return`
         * function and calling it. *)
        ; returnToSched (counter, cont)
        )

      fun acquireWork () =
        ( setStatus (myId, false)
        ; blockRequests ()
        ; doWork (request ())
        )

      fun return (counter, cont) =
        if decrementHitsZero counter
        then (communicate (); jumpTo (!cont))
        else acquireWork ()

      (* NOTE: it might be tempting to write the switch like so:
       *   Thread.switch (fn k =>
       *     ( cont := k
       *     ; if decrementHitsZero counter then runnable k
       *       else runnable (Thread.new acquireWork)
       *     )
       * However, this is incorrect. Thread.switch requires that the switch
       * complete before the argument (k, in this case) is switched to. Since
       * we have multiple workers running concurrently, this could happen! *)
      (* TODO: Can we prevent making a new thread here? *)
      fun sync (counter, cont) : unit =
        Thread.switch (fn k =>
          ( cont := k (* this must happen before decrementing the counter! *)
          ; runnable (Thread.new (fn _ => return (counter, cont)))
          ))

    in
      ( arrayUpdate "pushFuncs" (pushFuncs, myId, push)
      (*; arrayUpdate "popFuncs" (popFuncs, myId, pop)*)
      ; arrayUpdate "popDiscardFuncs" (popDiscardFuncs, myId, popDiscard)
      ; arrayUpdate "syncFuncs" (syncFuncs, myId, sync)
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
  val _ = init (myWorkerId ())

end

(* Author: Sam Westrick (swestric@cs.cmu.edu) *)

structure Scheduler :> SCHEDULER =
struct

  val _ = print ("Using private-deqs scheduler. Note that this might still be buggy.\n")

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

  fun arraySub str (a, i) = Array.sub (a, i)
  fun arrayUpdate str (a, i, x) = Array.update (a, i, x)
  fun vectorSub str (v, i) = Vector.sub (v, i)

  (*
  fun arraySub str (a, i) =
    Array.sub (a, i) handle e => (Atomic.print (fn _ => "Array.sub (" ^ str ^ ", " ^ Int.toString i ^ ")\n"); raise e)
  fun arrayUpdate str (a, i, x) =
    Array.update (a, i, x) handle e => (Atomic.print (fn _ => "Array.update (" ^ str ^ ", " ^ Int.toString i ^ ", ...)\n"); raise e)
  fun vectorSub str (v, i) =
    Vector.sub (v, i) handle e => (Atomic.print (fn _ => "Vector.sub (" ^ str ^ ", " ^ Int.toString i ^ ")\n"); raise e)
  *)

  (*
  fun arraySub x = Array.unsafeSub x
  fun arrayUpdate x = Array.unsafeUpdate x
  fun vectorSub x = Vector.unsafeSub x
  *)

  (* A request is either NO_REQUEST, REQUEST_BLOCKED, or a processor id.
   * Workers request work by writing their own id into another worker's
   * request cell. If a worker is idle, it blocks requests from other workers
   * by changing its own request cell to REQUEST_BLOCKED. *)
  val NO_REQUEST = ~1
  val REQUEST_BLOCKED = ~2
  val requestCells = Vector.tabulate (P, fn _ => ref NO_REQUEST)
  fun requestCell p = vectorSub "requestCells" (requestCells, p)

  (* Space out the statuses for cache performance (don't want unnecessary
   * invalidations for status updates). I wish there was a better way to do
   * this. TODO: does this actually make performance better? *)
  val statuses = Array.array (P*16, false)
  fun getStatus p = arraySub "statuses" (statuses, p*16)
  fun setStatus (p, s) = arrayUpdate "statuses" (statuses, p*16, s)
  val _ = setStatus (0, true)

  val MAIL_WAITING = 0
  val MAIL_RECEIVING = 1
  type mailbox = {flag : int ref, mail : (task * vertex) option ref}
  val mailboxes = Vector.tabulate (P, fn _ => {flag = ref MAIL_WAITING, mail = ref NONE})
  fun getMail p =
    let val {flag, mail} = vectorSub "getMail" (mailboxes, p)
    in if !flag = MAIL_RECEIVING
       then (flag := MAIL_WAITING; !mail)
       else getMail p
    end
  fun sendMail (p, m) =
    let val {flag, mail} = vectorSub "sendMail" (mailboxes, p)
    in ( mail := m
       ; if cas (flag, MAIL_WAITING, MAIL_RECEIVING) then ()
         else die (fn _ => "Error: send mail")
       )
    end

  val pushFuncs = Array.array (P, fn _ => fn _ => die (fn _ => "Error: dummy push func"))
  val popFuncs = Array.array (P, fn _ => (die (fn _ => "Error: dummy pop func"); NONE))
  val popDiscardFuncs = Array.array (P, fn _ => (die (fn _ => "Error: dummy popDiscard func"); false))
  val syncFuncs = Array.array (P, fn _ => die (fn _ => "Error: dummy yield func"))
  val returnFuncs = Array.array (P, fn _ => die (fn _ => "Error: dummy return func"))

  fun push j t = arraySub "pushFuncs" (pushFuncs, myWorkerId ()) j t
  fun pop x = arraySub "popFuncs" (popFuncs, myWorkerId ()) x
  fun popDiscard x = arraySub "popDiscardFuncs" (popDiscardFuncs, myWorkerId ()) x
  fun sync x = arraySub "syncFuncs" (syncFuncs, myWorkerId ()) x
  fun returnToSched x = arraySub "returnFuncs" (returnFuncs, myWorkerId ()) x

  fun new () = (ref 1, ref dummyThread)

  fun runnable (k : unit Thread.t) = Thread.prepare (k, ())
  fun jumpTo (k : unit Thread.t) = Thread.switch (fn _ => runnable k)

  (* ----------------------------------------------------------------------- *
   * ------------------------- MAIN SCHEDULER LOOP ------------------------- *
   * ----------------------------------------------------------------------- *)

  fun schedule (mainThread, myId) =
    let
      val myQueue = Queue.new ()
      val myRand = SimpleRandom.rand myId

      fun communicate () =
        ( let val friend = !(requestCell myId)
          in if friend = NO_REQUEST then ()
             else if friend = REQUEST_BLOCKED then die (fn _ => "Error: serve while blocked")
             else ( requestCell myId := NO_REQUEST
                  ; let val mail =
                          case Queue.popTop myQueue of
                            SOME (x as (_, (c, _))) => (increment c; SOME x)
                          | NONE => NONE
                    in sendMail (friend, mail)
                    end
                  )
          end
        ; let val haveWork = not (Queue.empty myQueue)
          in if getStatus myId = haveWork then ()
             else setStatus (myId, haveWork)
          end
        )

      fun reject () =
        let
          val myCell = requestCell myId
          val friend = !myCell
        in
          if friend = NO_REQUEST then
            if cas (myCell, NO_REQUEST, REQUEST_BLOCKED) then ()
            else reject () (* recurs at most once *)
          else
            ( myCell := REQUEST_BLOCKED
            ; sendMail (friend, NONE)
            )
        end

      fun push v t =
        Queue.pushBot ((t, v), myQueue)
        before communicate ()

      fun pop () =
        Option.map (fn (t, _) => t) (Queue.popBot myQueue)
        before communicate ()

      fun popDiscard () =
        Queue.popBotDiscard myQueue before communicate ()

      (* -------------------- request and receive loops -------------------- *)

      fun verifyStatus () =
        if getStatus myId = false then ()
        else die (fn _ => "Error: status not set correctly\n")

      fun randomOtherId () =
        let val other = SimpleRandom.boundedInt (0, P-1) myRand
        in if other < myId then other else other+1
        end

      fun request () =
        let
          val victimId = randomOtherId ()
        in
          if getStatus victimId andalso
            cas (requestCell victimId, NO_REQUEST, myId)
          then receiveFrom victimId
          else (verifyStatus (); request ())
        end

      and receiveFrom victimId =
        case getMail myId of
          NONE => (verifyStatus (); request ())
        | SOME x => (requestCell myId := NO_REQUEST; x)

      (* -------------------------- working loop -------------------------- *)

      (* (counter, cont) is the outgoing dependency of the given work *)
      fun doWork (task, (counter, cont)) =
        ( communicate ()
        ; task ()
        (* When work returns, we may have moved to a different worker.
         * returnToSched handles this by looking up the appropriate `return`
         * function and calling it. *)
        ; returnToSched (counter, cont)
        )

      fun acquireWork () =
        ( setStatus (myId, false)
        ; reject ()
        ; doWork (request ())
        )

      fun return (t as (counter, cont)) =
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
      ; arrayUpdate "popFuncs" (popFuncs, myId, pop)
      ; arrayUpdate "popDiscardFuncs" (popDiscardFuncs, myId, popDiscard)
      ; arrayUpdate "syncFuncs" (syncFuncs, myId, sync)
      ; arrayUpdate "returnFuncs" (returnFuncs, myId, return)
      ; case mainThread of
          NONE => acquireWork ()
        | SOME main => jumpTo main
      )
    end (* schedule *)

  (* ----------------------------------------------------------------------- *
   * -------- FINAL SETUP: INITIALIZING OTHER PROCS AND MAIN VERTEX -------- *
   * ----------------------------------------------------------------------- *)

  fun beginSched mainThread () = schedule (mainThread, myWorkerId ())

  val _ = MLton.Parallel.registerProcessorFunction (beginSched NONE)
  val _ = MLton.Parallel.initializeProcessors ()

  val _ = Thread.switch (fn main =>
    runnable (Thread.new (beginSched (SOME main))))

end

(* Author: Sam Westrick (swestric@cs.cmu.edu) *)

structure Scheduler :> SCHEDULER =
struct

  val _ = print ("Using private-deqs scheduler. Note that this might still be buggy.\n")

  exception Parallel of string

  val P = MLton.Parallel.numberOfProcessors
  val myWorkerId = MLton.Parallel.processorNumber

  (* ----------------------------------------------------------------------- *
   * ------------------------------ DEBUGGING ------------------------------ *
   * ----------------------------------------------------------------------- *)

  fun die strfn =
    ( Atomic.print (fn _ => Int.toString (myWorkerId ()) ^ ": " ^ strfn ())
    ; OS.Process.exit OS.Process.failure
    ; ()
    )

  val DEBUG = false
  val PRINT = false

  fun dbgPrint strsfn =
    if not PRINT then () else
    Atomic.print (fn _ => String.concatWith " " ((Int.toString (myWorkerId ()) ^ ":") :: (strsfn ())) ^ "\n")

  fun dbgCheck strsfn invariantfn =
    if not DEBUG orelse (invariantfn ()) then ()
    else die (fn _ => String.concatWith " " ("VIOLATED" :: (strsfn ())) ^ "\n")

  (* ----- janky command-line arguments ----- *)

  fun getBusyWorkArg ("-busy" :: str :: _) = Option.valOf (Int.fromString str)
    | getBusyWorkArg (_ :: l) = getBusyWorkArg l
    | getBusyWorkArg nil = 1000

  val BUSY_WORK = getBusyWorkArg (CommandLine.arguments ())

  (* ----------------------------------------------------------------------- *
   * ----------------------------------------------------------------------- *
   * ----------------------------------------------------------------------- *)

  structure Thread = MLton.Thread
  val cas = MLton.Parallel.compareAndSwap

  (* TODO: Implement a faster queue? Is this necessary? As long as only "big"
   * tasks are migrated, the cost of linear scanning to find the end of the
   * queue is amortized nicely. *)
  structure Queue = SimpleQueue
  (*structure Queue = DoublyLinkedList*)
  (*structure Queue = MkRingBuffer (val initialCapacity = 2048)*)
  (*structure Queue = MkChunkedDoublyLinkedList (val chunkSize = 64)*)

  type t = int ref * unit Thread.t ref
  type work = unit -> unit

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
  val statuses = Array.array (P*64, false)
  fun getStatus p = arraySub "statuses" (statuses, p*64)
  fun setStatus (p, s) = arrayUpdate "statuses" (statuses, p*64, s)

  datatype mailbox = Empty | Receiving of (t * work) option
  val mailboxes = Vector.tabulate (P, fn _ => Atomic.new Empty)
  fun getMailbox p = Atomic.read (vectorSub "mailboxes" (mailboxes, p))
  fun setMailbox (p, s) = Atomic.write (vectorSub "mailboxes" (mailboxes, p), s)

  val dummyThread = Thread.new (fn _ => die (fn _ => "Error: dummy thread"))

  val pushFuncs = Array.array (P, fn _ => die (fn _ => "Error: dummy push func"))
  val popFuncs = Array.array (P, fn _ => (die (fn _ => "Error: dummy pop func"); NONE))
  val popDiscardFuncs = Array.array (P, fn _ => (die (fn _ => "Error: dummy popDiscard func"); false))
  val syncFuncs = Array.array (P, fn _ => die (fn _ => "Error: dummy yield func"))
  val returnFuncs = Array.array (P, fn _ => die (fn _ => "Error: dummy return func"))

  fun returnToSched x = arraySub "returnFuncs" (returnFuncs, myWorkerId ()) x
  fun pop x = arraySub "popFuncs" (popFuncs, myWorkerId ()) x
  fun popDiscard x = arraySub "popDiscardFuncs" (popDiscardFuncs, myWorkerId ()) x
  fun push x = arraySub "pushFuncs" (pushFuncs, myWorkerId ()) x
  fun sync x = arraySub "syncFuncs" (syncFuncs, myWorkerId ()) x

  fun new () = (ref 1, ref dummyThread)

  fun runnable (k : unit Thread.t) = Thread.prepare (k, ())
  fun jumpTo (k : unit Thread.t) = Thread.switch (fn _ => runnable k)

  (* ----------------------------------------------------------------------- *
   * ------------------------- MAIN SCHEDULER LOOP ------------------------- *
   * ----------------------------------------------------------------------- *)

  fun schedule (mainThread, myId) =
    let
      val myQueue = Queue.new ()
      val myRand = Random.rand myId

      local val r = ref (0w0 : Word32.word) in
      fun busyWork n =
        if n = 0 then ()
        else ( let val x = !r
               in r := (x+0w2)*(x+0w3) div 0w3
               end
             ; busyWork (n-1)
             )
      end

      fun serve () =
        let val friend = !(requestCell myId)
        in if friend = NO_REQUEST then ()
           else if friend = REQUEST_BLOCKED then die (fn _ => "Error: serve while blocked")
           else ( requestCell myId := NO_REQUEST
                ; let val mail =
                        case Queue.popTop myQueue of
                          SOME (x as ((c, _), _)) => (increment c; SOME x)
                        | NONE => NONE
                  in setMailbox (friend, Receiving mail)
                  end
                )
        end

      fun communicate () =
        ( serve ()
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
            ; setMailbox (friend, Receiving NONE)
            )
        end

      fun push x = Queue.pushBot (x, myQueue)

      fun pop () =
        case Queue.popBot myQueue of
          SOME (_, work) => (communicate (); SOME work)
        | NONE => NONE

      fun popDiscard () =
        if Queue.popBotDiscard myQueue then (communicate (); true) else false

      (* -------------------- request and receive loops -------------------- *)

      fun verifyStatus () =
        if getStatus myId = false then ()
        else die (fn _ => "Error: status not set correctly\n")

      fun randomOtherId () =
        let val other = Random.boundedInt (0, P-1) myRand
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
        case getMailbox myId of
          Empty =>
            ( busyWork BUSY_WORK
            ; verifyStatus ()
            ; receiveFrom victimId
            )
        | Receiving NONE =>
            ( setMailbox (myId, Empty)
            ; verifyStatus ()
            ; request ()
            )
        | Receiving (SOME x) =>
            ( requestCell myId := NO_REQUEST
            ; setMailbox (myId, Empty)
            ; x
            )

      (* -------------------------- working loop -------------------------- *)

      (* (counter, cont) is the outgoing dependency of the given work *)
      fun doWork ((counter, cont), work) =
        ( communicate ()
        ; work ()
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
        case Queue.popBot myQueue of
          SOME (_, work) => (communicate (); work (); return t)
        | NONE => if decrementHitsZero counter
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
      fun sync (counter, cont) : unit =
        Thread.switch (fn k =>
          ( cont := k (* this must happen before decrementing the counter! *)
          ; runnable (Thread.new (fn _ => return (counter, cont)))
          ))

    in
      ( arrayUpdate "syncFuncs" (syncFuncs, myId, sync)
      ; arrayUpdate "returnFuncs" (returnFuncs, myId, return)
      ; arrayUpdate "pushFuncs" (pushFuncs, myId, push)
      ; arrayUpdate "popFuncs" (popFuncs, myId, pop)
      ; arrayUpdate "popDiscardFuncs" (popDiscardFuncs, myId, popDiscard)
      ; if myId = 0 then jumpTo mainThread else acquireWork ()
      )
    end (* schedule *)

  (* ----------------------------------------------------------------------- *
   * -------- FINAL SETUP: INITIALIZING OTHER PROCS AND MAIN VERTEX -------- *
   * ----------------------------------------------------------------------- *)

  fun beginSched mainThread () = schedule (mainThread, myWorkerId ())

  val _ = MLton.Parallel.registerProcessorFunction (beginSched dummyThread)
  val _ = MLton.Parallel.initializeProcessors ()

  val _ = Thread.switch (fn main => runnable (Thread.new (beginSched main)))

end

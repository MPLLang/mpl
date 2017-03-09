(* Author: Sam Westrick (swestric@cs.cmu.edu) *)

(* TODO: fork-join still seems to be buggy. Sometimes it crashes, sometimes it
 * hits a MLton.Thread exception, sometimes a Subscript exception. What is
 * going on? *)
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
    ( Atomic.print strfn
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
  (*structure Queue = MkRingBuffer (val initialCapacity = 2048)*)
  (*structure Queue = DoublyLinkedList*)
  (*structure Queue = MkChunkedDoublyLinkedList (val chunkSize = 64)*)

  datatype 'a result =
    Waiting
  | Finished of 'a
  | Raised of exn

  type 'a t = (int ref * 'a result ref)

  type thunk = unit -> unit

  fun decrementHitsZero (x : int ref) : bool =
    MLton.Parallel.fetchAndAdd (x, ~1) = 1

  fun increment (x : int ref) : unit =
    ignore (MLton.Parallel.fetchAndAdd (x, 1))

  (*fun arraySub x = Array.unsafeSub x
  fun arrayUpdate x = Array.unsafeUpdate x*)

  fun arraySub str (a, i) = Array.sub (a, i)
    (*Array.sub (a, i) handle e => (Atomic.print (fn _ => "Array.sub (" ^ str ^ ", " ^ Int.toString i ^ ")\n"); raise e)*)
  fun arrayUpdate str (a, i, x) = Array.update (a, i, x)
    (*Array.update (a, i, x) handle e => (Atomic.print (fn _ => "Array.update (" ^ str ^ ", " ^ Int.toString i ^ ", ...)\n"); raise e)*)

  fun vectorSub str (v, i) = Vector.sub (v, i)
    (*Vector.sub (v, i) handle e => (Atomic.print (fn _ => "Vector.sub (" ^ str ^ ", " ^ Int.toString i ^ ")\n"); raise e)*)

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

  datatype mailbox = Empty | Receiving of thunk option
  val mailboxes = Vector.tabulate (P, fn _ => Atomic.new Empty)
  fun getMailbox p = Atomic.read (vectorSub "mailboxes" (mailboxes, p))
  fun setMailbox (p, s) = Atomic.write (vectorSub "mailboxes" (mailboxes, p), s)
  (*val mailboxes = Array.array (P, Empty)
  fun getMailbox p = arraySub "mailboxes" (mailboxes, p)
  fun setMailbox (p, s) = arrayUpdate "mailboxes" (mailboxes, p, s)*)

  val pushFuncs = Array.array (P, fn _ => die (fn _ => "Error: dummy push func"))
  val popFuncs = Array.array (P, fn _ => (die (fn _ => "Error: dummy pop func"); false))
  val syncFuncs = Array.array (P, fn _ => die (fn _ => "Error: dummy yield func"))
  val returnFuncs = Array.array (P, fn _ => die (fn _ => "Error: dummy return func"))
  (*val executeQueueFuncs = Array.array (P, fn _ => die (fn _ => "Error: dummy executeQueue func"))*)

  fun runnable (k : unit Thread.t) = Thread.prepare (k, ())
  fun jumpTo (k : unit Thread.t) = Thread.switch (fn _ => runnable k)

  fun returnToSched x = arraySub "returnFuncs" (returnFuncs, myWorkerId ()) x
  fun pop () = arraySub "popFuncs" (popFuncs, myWorkerId ()) ()

  (*fun executeQueue () =
    arraySub "executeQueueFuncs" (executeQueueFuncs, myWorkerId ()) ()*)

  (*fun dummyThunk () = die (fn _ => "Error: dummy thunk")*)

  fun push (run : unit -> 'a) : 'a t =
    let
      val counter = ref 2
      val result = ref Waiting
      fun run' current () =
        ( result := (Finished (run ()) handle e => Raised e)
        ; returnToSched (counter, current)
        )
    in
      ( arraySub "pushFuncs" (pushFuncs, myWorkerId ()) run'
      ; (counter, result)
      )
    end

  fun sync (counter : int ref, result : 'a result ref) : 'a =
    ( arraySub "syncFuncs" (syncFuncs, myWorkerId ()) counter
    ; case !result of
        Finished x => x
      | Raised e => raise e
      | Waiting => raise Parallel "Result not written after sync!"
    )

  (*type async = thunk -> unit

  (* TODO: correct way is
   * fun async (counter, cont) thunk = ...
   * where `cont` is determined to be the current of where `finish` was called. *)
  fun async counter thunk =
    let
      (* when `current` is passed, the thunk activates, incrementing the
       * appropriate synchronization counter and returning a new thunk
       * which returns to the scheduler upon completing. *)
      (* TODO: passing `current` at activation is incorrect (actually passing
       * current at creation is incorrect too). We need the continuation
       * associated with the person who called `finish`, not the current
       * processor. So we will need to associate continuations with counters :(
       *)
      fun thunk' current =
        ( increment counter
        ; fn () => ( thunk ()
                   ; executeQueue ()
                   ; returnToSched (counter, current)
                   )
        )
    in
      arraySub "pushFuncs" (pushFuncs, myWorkerId ()) (thunk, thunk')
    end

  fun finish (body : async -> 'a) : 'a =
    let
      val counter = ref 1
      val result = body (async counter)
    in
      ( executeQueue ()
      ; arraySub "syncFuncs" (syncFuncs, myWorkerId ()) counter
      ; result
      )
    end*)

  (* ----------------------------------------------------------------------- *
   * ------------------------- MAIN SCHEDULER LOOP ------------------------- *
   * ----------------------------------------------------------------------- *)

  val dummyThread = Thread.new (fn _ => die (fn _ => "Error: dummy thread"))
  (*val mainCont = ref dummyThread*)

  fun schedule (mainThread, myId) =
    let
      val myQueue = Queue.new () : thunk Queue.t
      val myRand = Random.rand myId
      val myCurrent = ref (ref dummyThread)

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
           else ( requestCell myId := NO_REQUEST
                ; let val mail = Queue.popTop myQueue
                        (*case Queue.popTop myQueue of
                          NONE => NONE
                        | SOME (_, thunk) => SOME (thunk (!myCurrent)) (* activate the thunk *)*)
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

      fun push (run' : unit Thread.t ref -> thunk) =
        Queue.pushBot (run' (!myCurrent), myQueue)

      fun pop () =
        if Queue.popBotDiscard myQueue then (communicate (); true) else false

      (*fun executeQueue () =
        case Queue.popBot myQueue of
          NONE => ()
        | SOME (thunk, _) => (thunk (); communicate (); executeQueue ())*)

      (* -------------------- request and receive loops -------------------- *)

      fun verifyStatus () =
        if getStatus myId = false then ()
        else die (fn _ => "status not set correctly\n")

      fun randomOtherId () =
        let val other = Random.boundedInt (0, P-1) myRand
        in if other < myId then other else other+1
        end

      fun request () : thunk =
        let
          val victimId = randomOtherId ()
        in
          if getStatus victimId andalso
            cas (requestCell victimId, NO_REQUEST, myId)
          then receiveFrom victimId
          else (verifyStatus (); request ())
        end

      and receiveFrom victimId : thunk =
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
        | Receiving (SOME thunk) =>
            ( requestCell myId := NO_REQUEST
            ; setMailbox (myId, Empty)
            ; thunk
            )

      (* -------------------------- working loop -------------------------- *)

      fun doWork thunk =
        ( myCurrent := ref dummyThread
        ; communicate ()
        ; thunk ()
        )

      fun acquireWork () =
        ( setStatus (myId, false)
        ; reject ()
        ; doWork (request ())
        )

      fun continueWork (cont : unit Thread.t ref) =
        ( myCurrent := cont
        ; communicate ()
        ; jumpTo (!cont)
        )

      fun return (counter, cont) =
        if decrementHitsZero counter then continueWork cont else acquireWork ()

      (* NOTE: it might be tempting to just write this function like so:
       *   fun sync counter =
       *     Thread.switch (fn k =>
       *       ( !myCurrent := k
       *       ; if decrementHitsZero counter then runnable k
       *         else runnable (Thread.new acquireWork)
       *       )
       * However, this is incorrect. Thread.switch requires that the switch
       * complete before the argument (k, in this case) is switched to. Since
       * we have multiple workers running concurrently, this could happen! *)
      fun sync (counter : int ref) : unit =
        let val cont = !myCurrent
        in Thread.switch (fn k =>
             ( cont := k (* this must happen before decrementing the counter! *)
             ; runnable (Thread.new (fn _ => return (counter, cont)))
             ))
        end


    in
      ( arrayUpdate "syncFuncs" (syncFuncs, myId, sync)
      ; arrayUpdate "returnFuncs" (returnFuncs, myId, return)
      ; arrayUpdate "pushFuncs" (pushFuncs, myId, push)
      ; arrayUpdate "popFuncs" (popFuncs, myId, pop)
      (*; arrayUpdate "executeQueueFuncs" (executeQueueFuncs, myId, executeQueue)*)
      ; if myId = 0 then continueWork (ref mainThread) else acquireWork ()
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

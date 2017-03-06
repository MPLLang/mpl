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

  fun decrement (x : int ref) : bool =
    MLton.Parallel.fetchAndAdd (x, ~1) = 1

  (*fun arraySub x = Array.unsafeSub x
  fun arrayUpdate x = Array.unsafeUpdate x*)

  fun arraySub str (a, i) =
    Array.sub (a, i) handle e => (Atomic.print (fn _ => "Array.sub (" ^ str ^ ", " ^ Int.toString i ^ ")\n"); raise e)
  fun arrayUpdate str (a, i, x) =
    Array.update (a, i, x) handle e => (Atomic.print (fn _ => "Array.update (" ^ str ^ ", " ^ Int.toString i ^ ", ...)\n"); raise e)

  (* A request is either NO_REQUEST, REQUEST_BLOCKED, or a processor id.
   * Workers request work by writing their own id into another worker's
   * request cell. If a worker is idle, it blocks requests from other workers
   * by changing its own request cell to REQUEST_BLOCKED. *)
  val NO_REQUEST = ~1
  val REQUEST_BLOCKED = ~2
  val requestCells = Vector.tabulate (P, fn _ => ref NO_REQUEST)
  fun requestCell p = Vector.sub (requestCells, p)

  val statuses = Array.array (P*64, false)
  fun getStatus p = arraySub "statuses" (statuses, p*64)
  fun setStatus (p, s) = arrayUpdate "statuses" (statuses, p*64, s)

  datatype mailbox = Empty | Receiving of thunk option
  val mailboxes = Vector.tabulate (P, fn _ => Atomic.new Empty)
  fun getMailbox p = Atomic.read (Vector.sub (mailboxes, p))
  fun setMailbox (p, s) = Atomic.write (Vector.sub (mailboxes, p), s)
  (*val mailboxes = Array.array (P, Empty)
  fun getMailbox p = arraySub "mailboxes" (mailboxes, p)
  fun setMailbox (p, s) = arrayUpdate "mailboxes" (mailboxes, p, s)*)

  val pushFuncs = Array.array (P, fn _ => die (fn _ => "Error: dummy push func"))
  val popFuncs = Array.array (P, fn _ => (die (fn _ => "Error: dummy pop func"); false))
  val syncFuncs = Array.array (P, fn _ => die (fn _ => "Error: dummy yield func"))
  val returnFuncs = Array.array (P, fn _ => die (fn _ => "Error: dummy return func"))

  fun runnable (k : unit Thread.t) = Thread.prepare (k, ())
  fun jumpTo (k : unit Thread.t) = Thread.switch (fn _ => runnable k)

  fun returnToSched x = arraySub "returnFuncs" (returnFuncs, myWorkerId ()) x
  fun pop x = arraySub "popFuncs" (popFuncs, myWorkerId ()) x

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

  (* ----------------------------------------------------------------------- *
   * ------------------------- MAIN SCHEDULER LOOP ------------------------- *
   * ----------------------------------------------------------------------- *)

  val dummyThread = Thread.new (fn _ => die (fn _ => "dummy thread"))
  val mainCont = ref dummyThread

  fun schedule myId =
    let
      val myQueue = Queue.new ()
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
                ; setMailbox (friend, Receiving (Queue.popTop myQueue))
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

      fun push (run : unit Thread.t ref -> thunk) =
        Queue.pushBot (run (!myCurrent), myQueue)

      fun pop () =
        if Queue.popBotDiscard myQueue then (communicate (); true) else false

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
        if decrement counter then continueWork cont else acquireWork ()

      fun sync (counter : int ref) : unit =
        Thread.switch (fn k =>
          ( !myCurrent := k (* this must happen before decrementing the counter! *)
          ; if decrement counter then runnable k
            else runnable (Thread.new acquireWork)
          ))

    in
      ( arrayUpdate "syncFuncs" (syncFuncs, myId, sync)
      ; arrayUpdate "returnFuncs" (returnFuncs, myId, return)
      ; arrayUpdate "pushFuncs" (pushFuncs, myId, push)
      ; arrayUpdate "popFuncs" (popFuncs, myId, pop)
      ; if myId = 0 then continueWork mainCont else acquireWork ()
      )
    end (* schedule *)

  (* ----------------------------------------------------------------------- *
   * -------- FINAL SETUP: INITIALIZING OTHER PROCS AND MAIN VERTEX -------- *
   * ----------------------------------------------------------------------- *)

  fun beginSched () = schedule (myWorkerId ())

  val _ = MLton.Parallel.registerProcessorFunction beginSched
  val _ = MLton.Parallel.initializeProcessors ()

  val _ = Thread.switch (fn main =>
    ( mainCont := main
    ; runnable (Thread.new beginSched)
    ))

end

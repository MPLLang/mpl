(* NOTE/TODO: for now, we restrict edge (v1, v2) by requiring that neither
 * v1 nor v2 is concurrent with the thread that called edge. This disallows
 * futures, but still allows fork-join and async-finish. *)

(* TODO: I think Vector.sub might actually be a significant expense. Need to
 * fix that. *)

structure MLtonParallelBasic :> MLTON_PARALLEL_BASIC =
struct

  exception Parallel of string

  structure Thread = MLtonThread
  structure Atomic = MLtonParallelAtomic
  structure Queue = RingBuffer
  structure Incounter = FetchAddIncounter

  (* Might as well reuse this for now. Will need to eventually replace. *)
  structure Outset = RingBuffer

  val P = MLtonParallelInternal.numberOfProcessors
  val myWorkerId = MLtonParallelInternal.processorNumber

  val sdelayError = "Error: -sdelay x[ms|us|ns]"
  fun sdelayArg str =
    case LargeInt.fromString str of
      NONE => (print (sdelayError ^ "\n"); raise Parallel sdelayError)
    | SOME x =>
        case String.extract(str, String.size str - 2, NONE) of
          "ms" => Time.fromMilliseconds x
        | "us" => Time.fromMicroseconds x
        | "ns" => Time.fromNanoseconds x
        | _    => (print (sdelayError ^ "\n"); raise Parallel sdelayError)

  (* After being social (communicating, checking mailbox, etc.), a worker
   * must wait this long until it is allowed to be social again. *)
  fun getSocialDelayTime ("-sdelay" :: str :: _) = sdelayArg str
    | getSocialDelayTime (_ :: l) = getSocialDelayTime l
    | getSocialDelayTime nil = Time.fromMilliseconds 0

  val SOCIAL_DELAY = getSocialDelayTime (CommandLine.arguments ())

  (*
  val DEBUG = List.exists (fn str => str = "--debug" orelse str = "--debugprint") (CommandLine.arguments ())
  val PRINT = List.exists (fn str => str = "--debugprint") (CommandLine.arguments ())
  *)
  val DEBUG = false
  val PRINT = false

  fun die strfn =
    ( OS.Process.atExit (fn _ => Atomic.print strfn)
    ; OS.Process.exit OS.Process.failure
    )

  fun dbgPrint strsfn =
    if not PRINT then () else
    Atomic.print (fn _ => String.concatWith " " ((Int.toString (myWorkerId ()) ^ ":") :: (strsfn ())) ^ "\n")

  fun dbgCheck strsfn invariantfn =
    if not DEBUG orelse (invariantfn ()) then ()
    else die (fn _ => String.concatWith " " ("VIOLATED" :: (strsfn ())) ^ "\n")

  val doLog = ref false
  val INITIAL_LOG_SIZE = 1024
  fun eventNow () = Int64.fromLarge (Time.toMicroseconds (Time.now ()))
  val eventBuffers : Log.event Queue.t vector = Vector.tabulate (P, fn _ => Queue.new INITIAL_LOG_SIZE)
  fun event (l : Log.label) =
    if not (!doLog) then () else
    let val p = myWorkerId ()
        val e = (eventNow (), Int64.fromInt p, l)
    in ignore (Queue.pushTop (Vector.sub (eventBuffers, p), e))
    end
  fun esToList es =
    ( case Queue.peekTop es of
        SOME (_, p, Log.EnterWait) => ignore (Queue.pushTop (es, (eventNow (), p, Log.ExitWait)))
      | _ => ()
    ; Queue.toList es
    )
  fun writeLog filepath =
    let val es = Vector.foldr (fn (es, all) => esToList es @ all) [] eventBuffers
    in Log.writeEvents (filepath, es)
    end
  fun log (filepath, f : unit -> 'a) =
    let
      val _ = doLog := true
      val _ = event Log.EnterAlgo
      val ts = Time.now ()
      val x = f ()
      val te = Time.now ()
      val _ = event Log.ExitAlgo
      val _ = doLog := false
      val _ = writeLog filepath
    in
      (x, Time.toMilliseconds (Time.- (te, ts)))
    end

  (* ----------------------------------------------------------------------- *
   * --------------- VERTEX DEFINITIONS, SHARED/PRIVATE DATA --------------- *
   * ----------------------------------------------------------------------- *)

  val INITIAL_QUEUE_SIZE = 1024
  val INITIAL_OUTSET_SIZE = 4

  datatype work =
    Thunk of (unit -> unit)
  | Paused of unit Thread.t

  datatype t =
    Vertex of
      { cont  : work ref
      , inc   : Incounter.t
      , out   : (Incounter.h * t) Outset.t
      , hold  : Incounter.h ref (* release handle *)
      }
  type vertex = t

  fun contOf (Vertex {cont, ...}) = cont
  fun incOf (Vertex {inc, ...}) = inc
  fun outOf (Vertex {out, ...}) = out
  fun holdOf (Vertex {hold, ...}) = hold

  (* Rely on equal ref cells *)
  fun verticesEqual (v1, v2) = (contOf v1 = contOf v2)

  type seed = { run : unit -> unit, out : vertex }

  val dummyVert =
    let val inc = Incounter.new ()
    in Vertex { cont = ref (Thunk (fn _ => die (fn _ => "Unexpected run of dummy vertex\n")))
              , inc = inc
              , out = Outset.new 1
              , hold = ref (Incounter.increment inc)
              }
    end

  (* Local (private) state for each worker:
   *  - a work queue, containing ready vertices
   *  - random generator state, to pick work stealing victims
   *  - current: the vertex most recently popped from the queue
   *  - the next time at which a processor is allowed to be social (communicate
   *    or check mailbox, etc.) *)
  val perWorker = Vector.tabulate (P, fn i =>
    { queue = Queue.new INITIAL_QUEUE_SIZE : vertex Queue.t
    , seeds = Queue.new INITIAL_QUEUE_SIZE : seed Queue.t
    , rand = Random.rand i                 : Random.t
    , current = ref dummyVert              : vertex ref
    , socialtime = ref (Time.now ())       : Time.time ref
    , expense = ref Time.zeroTime          : Time.time ref
    , timer = ref Time.zeroTime            : Time.time ref
    , victims = Array.array (P, (0, 0))    : (int * int) array
    })

  (* instrument func to figure out if it is expensive *)
  fun instrument func arg =
    let val ts = Time.now ()
        val result = func arg
        val tf = Time.now ()
        val er = #expense (Vector.sub (perWorker, myWorkerId ()))
    in er := Time.+ (!er, Time.- (tf, ts));
       result
    end

  val timeNow = (*instrument*) Time.now
  fun vectorSub x = instrument Vector.sub x

  fun startExpenseTimer p =
    let val tr = #timer (vectorSub (perWorker, p))
    in tr := timeNow ()
    end

  fun stopExpenseTimer p =
    let val tf = timeNow ()
        val ref ts = #timer (vectorSub (perWorker, p))
        val er = #expense (vectorSub (perWorker, p))
    in er := Time.+ (!er, Time.- (tf, ts))
    end

  fun markVictimSuccess (p, v) =
    let val vs = #victims (vectorSub (perWorker, p))
        val (s, f) = Array.sub (vs, v)
    in Array.update (vs, v, (s + 1, f))
    end

  fun markVictimFailure (p, v) =
    let val vs = #victims (vectorSub (perWorker, p))
        val (s, f) = Array.sub (vs, v)
    in Array.update (vs, v, (s, f + 1))
    end

  fun readVictims p =
    Array.foldl (fn ((scount, fcount), str) => str ^ Int.toString scount ^ "," ^ Int.toString fcount ^ "\t") ""
      (#victims (vectorSub (perWorker, p)))

  fun report () =
    ( List.app
        (fn (p, t) => print (Int.toString p ^ ": " ^ LargeInt.toString
          (Time.toMilliseconds t) ^ " ms;\t" ^ readVictims p ^ "\n"))
        (List.tabulate (P, fn p => (p, !(#expense (Vector.sub (perWorker, p))))))
    )

  fun getQueue p = #queue (vectorSub (perWorker, p))
  fun myQueue () = getQueue (myWorkerId ())

  fun getSeeds p = #seeds (vectorSub (perWorker, p))
  fun mySeeds () = getSeeds (myWorkerId ())

  fun getCurrent p = !(#current (vectorSub (perWorker, p)))
  fun myCurrent () = getCurrent (myWorkerId ())

  (*fun getSchedCont p = !(#cont (vectorSub (perWorker, p)))
  fun mySchedCont () = getSchedCont (myWorkerId ())*)

  fun getRand p = #rand (vectorSub (perWorker, p))
  fun myRand () = getRand (myWorkerId ())

  fun getSocialTime p = !(#socialtime (vectorSub (perWorker, p)))
  fun mySocialTime () = getSocialTime (myWorkerId ())

  fun setCurrent (p, v) = #current (vectorSub (perWorker, p)) := v
  fun setMyCurrent v = setCurrent (myWorkerId (), v)

  (*fun setSchedCont (p, k) = #cont (vectorSub (perWorker, p)) := k
  fun setMySchedCont k = setSchedCont (myWorkerId (), k)*)

  fun setSocialTime p =
    let val t = Time.+ (timeNow (), SOCIAL_DELAY)
    in #socialtime (vectorSub (perWorker, p)) := t
    end
  fun setMySocialTime () = setSocialTime (myWorkerId ())

  fun isAllowedSocial p = Time.>= (timeNow (), getSocialTime p)
  fun amAllowedSocial () = isAllowedSocial (myWorkerId ())

  fun waitUntilIsAllowedSocial p =
    if isAllowedSocial p then () else waitUntilIsAllowedSocial p
  fun waitUntilAmAllowedSocial () = waitUntilIsAllowedSocial (myWorkerId ())

  (* Pick a uniformly random member of ({0 ... P-1} \ p). Error if P = 1.
   * Note that the random state is updated imperatively. *)
  fun randomOtherId p =
    let val other = Random.boundedInt (0, P-1) (getRand p)
    in if other < p then other else other+1
    end

  (* A request is either NO_REQUEST, REQUEST_BLOCKED, or a processor id.
   * Workers request work by writing their own id into another worker's
   * request cell. If a worker is idle, it blocks requests from other workers
   * by changing its own request cell to REQUEST_BLOCKED. *)
  val NO_REQUEST = ~1
  val REQUEST_BLOCKED = ~2
  val requestCells = Vector.tabulate (P, fn _ => ref NO_REQUEST)
  fun requestCell p = vectorSub (requestCells, p)

  (* Mailboxes are used to respond to requests. If a worker has an incoming
   * request from an idle worker, it writes a response into the other worker's
   * mailbox. A response is a collection of ready vertices, taken from the
   * non-idle worker's queue. For now, this is just given by a vertex list,
   * but in the future we could do an entire queue of possibly many vertices *)
  datatype mail = Empty | Reject | Package of vertex
  val mailboxes : mail array = Array.array (P, Empty)
  fun sendMail (p, m) = Array.update (mailboxes, p, m)
  fun checkMail p = Array.sub (mailboxes, p)
  fun emptyMailbox p = Array.update (mailboxes, p, Empty)

  (* Status cells allow processors to remotely determine if a victim has work
   * to share without waiting for a reply. *)
  val statuses = Array.array (P, false)
  fun hasWork p = Array.sub (statuses, p)
  fun updateStatus p =
    let val s = not (Queue.empty (getQueue p) andalso Queue.empty (getSeeds p))
    in if s = hasWork p then () else Array.update (statuses, p, s)
    end

  fun runnable (k : unit Thread.t) = Thread.prepare (k, ())
  fun jumpTo (k : unit Thread.t) = Thread.switch (fn _ => runnable k)

  fun decrementPush p (h, v) =
    if not (Incounter.decrement h) then ()
    else Queue.pushBot (v, getQueue p)

  fun release (v : vertex) =
    decrementPush (myWorkerId ()) (!(holdOf v), v)

  (* TODO: make this thread-safe for when v1 and/or v2 are concurrent with the
   * calling thread. Will have to deal with v1 having already finished (i.e.
   * adding to its outset would fail, because its outneighbors have already
   * been notified) in which case we roll back on the v2 in edge creation. *)
  fun edge (v1 : vertex, v2 : vertex) =
    ignore (Outset.pushBot ((Incounter.increment (incOf v2), v2), outOf v1))

  fun self () = myCurrent ()

  (* ----------------------------------------------------------------------- *
   * ------------------------- MAIN SCHEDULER LOOP ------------------------- *
   * ----------------------------------------------------------------------- *)

  fun hold (v : vertex) =
    (holdOf v) := Incounter.increment (incOf v)

  fun reject p =
    let val r = requestCell p
    in if !r = NO_REQUEST then
         if Atomic.compareAndSwap (r, NO_REQUEST, REQUEST_BLOCKED)
         then () else reject p (* recurs at most once *)
       else
         ( sendMail (!r, Reject)
         ; r := REQUEST_BLOCKED
         )
    end

  fun accept p = requestCell p := NO_REQUEST

  fun verifyBlocked p =
    if !(requestCell p) = REQUEST_BLOCKED then ()
    else Atomic.print (fn _ => "Uh oh, " ^ Int.toString p ^ " not blocked!\n")

  fun verifyNoWorkStatus p =
    if not (hasWork p) then ()
    else Atomic.print (fn _ => "Uh oh, " ^ Int.toString p ^ " status is broken!\n")

  fun schedule p =
    case Queue.popBot (getQueue p) of
      NONE => (updateStatus p; reject p; request p)
    | SOME v =>
        ( setCurrent (p, v)
        ; hold v
        ; procCommunicate p
        ; execute (p, v)
        )

  and procCommunicate p =
    if not (isAllowedSocial p) then () else
    let
      fun assembleMail () =
        case Queue.popTop (getQueue p) of
          SOME v => Package v
        | NONE => case Queue.popTop (getSeeds p) of
                    NONE => Reject
                  | SOME {run, out=outv} =>
                      let fun run' () = (run (); returnTo (myWorkerId ()))
                          val v = Vertex { cont = ref (Thunk run')
                                         , out = Outset.new INITIAL_OUTSET_SIZE
                                         , inc = Incounter.new ()
                                         , hold = ref (!(holdOf dummyVert))
                                         }
                      in edge (v, outv); Package v
                      end
      val r = !(requestCell p)
    in
      ( setSocialTime p
      ; if r = NO_REQUEST then ()
        else ( sendMail (r, assembleMail ())
             ; requestCell p := NO_REQUEST
             )
      ; updateStatus p
      )
    end

  and execute (p, v) =
    case !(contOf v) of
      Thunk f   => f ()
    | Paused vt => jumpTo vt

  and yieldTo p =
    ( release (getCurrent p)
    ; schedule p
    )

  and returnTo p =
    ( Outset.app (decrementPush p) (outOf (getCurrent p))
    ; schedule p
    )

  and request me =
    let
      val _ = verifyBlocked me
      val _ = verifyNoWorkStatus me
      val victim = randomOtherId me
      val success = (hasWork victim) andalso
                    Atomic.compareAndSwap (requestCell victim, NO_REQUEST, me)
    in
      if not success then request me
      else receive (me, victim)
    end

  and receive (p, victim) =
    case checkMail p of
      Empty =>
        ( verifyBlocked p
        ; verifyNoWorkStatus p
        ; receive (p, victim)
        )

    | Reject =>
        ( markVictimFailure (p, victim)
        ; emptyMailbox p
        ; request p
        )

    | Package v =>
        ( markVictimSuccess (p, victim)
        ; emptyMailbox p
        ; Queue.pushBot (v, getQueue p)
        ; accept p
        ; schedule p
        )

  (* Creating a new vertex from a thunk *)
  fun new (run : unit -> unit) : vertex =
    let
      val inc = Incounter.new ()
      fun execute () =
        ( run ()
        ; returnTo (myWorkerId ())
        )
    in
      Vertex { cont = ref (Thunk execute)
             , inc = inc
             , out = Outset.new INITIAL_OUTSET_SIZE
             , hold = ref (Incounter.increment inc)
             }
    end

  fun push run =
    let val p = myWorkerId ()
    in Queue.pushBot ({run = run, out = getCurrent p}, getSeeds p)
    end

  fun pop () =
    Option.isSome (Queue.popBot (mySeeds ()))

  fun communicate () =
    procCommunicate (myWorkerId ())

  fun yield () =
    let val p = myWorkerId ()
    in Thread.switch (fn k =>
        ( contOf (getCurrent p) := Paused k
        ; runnable (Thread.new (fn _ => yieldTo p))
        ))
    end

  fun beginSched () = schedule (myWorkerId ())

  val () = (_export "Parallel_run": (unit -> unit) -> unit;) beginSched
  (* init MUST come after scheduler loop has been exported *)
  val init = (_import "Parallel_init" runtime private: unit -> unit;)
  val () = init ()

  val _ = Thread.switch (fn main =>
    let
      val inc = Incounter.new ()
      val mainVertex =
        Vertex { cont = ref (Paused main)
               , inc = inc
               , out = Outset.new INITIAL_OUTSET_SIZE
               , hold = ref (Incounter.increment inc)
               }
      val _ = release mainVertex
    in
      runnable (Thread.new beginSched)
    end)

end

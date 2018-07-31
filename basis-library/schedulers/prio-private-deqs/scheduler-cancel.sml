(*structure Tm =
struct
  open Time
  fun fromTime t = t
  fun toTime t = t
end*)
(*structure Tm = BetterTime*)
structure Tm = TSCTime
structure A = Array
structure V = Vector
structure P = Priority
structure I = Interrupt
structure T = MLton.Thread
structure R = UsefulRandom
structure MT = MersenneTwister

val numberOfProcessors = MLton.Parallel.numberOfProcessors
val processorNumber = MLton.Parallel.processorNumber

fun lockMutex m =
    if !m <> 0 then
        lockMutex m
    else
        if MLton.Parallel.compareAndSwap m (0, 1) = 0 then
            ()
        else
            lockMutex m
fun unlockMutex m = m := 0

datatype cancellable =
         C of { cancelled : bool ref,
                f         : (unit -> unit) ref,
                children  : cancellable list ref,
                mutex     : int ref }

val cancels : cancellable option A.array =
    A.array (numberOfProcessors, NONE)

fun new_cancel () =
    let val newc =
            C { cancelled = ref false,
                f = ref (fn _ => ()),
                children = ref [],
                mutex = ref 0}
    in
        (case A.sub (cancels, processorNumber ()) of
             NONE => ()
           | SOME (C {children, mutex, ...}) =>
             (lockMutex mutex;
              children := newc::(!children);
              unlockMutex mutex));
        newc
    end

structure Task =
struct
datatype work =
         Empty
         | Thunk of unit -> unit
         | Thread of T.Runnable.t
type t = work * int * cancellable
val default = (Empty, 0, new_cancel ())
fun depth (_, w, _) = w
fun cancellable (_, _, c) = c
end


structure M = Mailbox
(*structure Q = BinaryHeapQueue(Task)*)
(* structure Q = ListQueue(Task) *)
structure Q = HeavyInsertDLLQueue(Task)

structure Cancellable =
struct

exception Cancel of string

type t = cancellable

fun new () = new_cancel ()

fun cancel (C { cancelled, f, children, ... }) =
    if !cancelled then ()
    else
        let in
            cancelled := true;
            (!f) ();
            List.app cancel (!children)
        end

fun pop (C { f, ...}) =
    f := (fn _ => ())

fun push (C { f, ...}) qh =
    f := (fn _ => (* ignore (Q.tryRemove qh)) *)
             if Q.tryRemove qh then
                 ()
             else
                 (print "not in list\n";
                  raise (Cancel "not in list")))

fun isCancelled (C {cancelled, ...}) =
    !cancelled

fun workOn c =
    A.update (cancels, processorNumber (), SOME c)

fun workingOn () =
    let val p = processorNumber ()
        val co = A.sub (cancels, p)
    in
        A.update (cancels, p, NONE);
        co
            (*
        case co of
            SOME c => c
          | NONE => (print "no cancellable in array\n";
                     raise (Cancel "no cancellable in array"))
*)
    end
end

structure C = Cancellable

exception ShouldntGetHere

type priorities =
     { primary: P.t,
       secondary: P.t,
       send: P.t }

structure IOQ =
struct
type t = ((unit -> bool) * Task.t * P.t) list
fun new () = []
fun process (resume: Task.t * P.t -> unit) (l: t) : t =
    List.foldl (fn ((f, t, r), l) =>
                   if f () then
                       (resume (t, r);
                        l)
                   else
                       (f, t, r)::l)
               []
               l
end

val printMutex = ref 0


fun log l f =
    if l < 0 then
        (lockMutex printMutex;
         print (f ());
         TextIO.flushOut TextIO.stdOut;
         unlockMutex printMutex)
    else
        ()

(*** Constants ***)
val switchInterval = Tm.fromMicroseconds 10000
(*val dealInterval = Tm.fromMicroseconds 100*)
val dIf = 300.0
val interruptInterval = Tm.fromMicroseconds 5000


(*** Per-processor state ***)
val nextSwitch = A.array (numberOfProcessors, Tm.zeroTime)
val nextDeal = A.array (numberOfProcessors, Tm.zeroTime)
val depth = A.array (numberOfProcessors, 0)
val curprios = A.array (numberOfProcessors, P.bot)
val prios = A.array (numberOfProcessors, {primary = P.bot,
                                          secondary = P.bot,
                                          send = P.bot})
val ioqueues = A.tabulate (numberOfProcessors, fn _ => IOQ.new ())
val reqCells = V.tabulate (numberOfProcessors, fn _ => ref ~1)
(*val rands = A.tabulate (numberOfProcessors, fn i => MT.init32
                                                        (Word.fromInt i))
 *)
(* These are initialized after the priorities. *)
val mailboxes = ref (A.fromList [])
val queues = ref (A.fromList [])
val topprios = V.tabulate (numberOfProcessors, fn _ => ref ~1)

                 (*
structure R =
struct
fun randInt (a, b) =
    let val p = processorNumber ()
        val mt = A.sub (rands, p)
    in
        (MT.random_nat mt (b + 1)) + a
    end

fun rand01 () =
    let val n = Real.fromInt (randInt (0, 1000000))
    in
        n / 1000000.0
    end

fun rand01ex () =
    let val n = Real.fromInt ((randInt (0, 999998)) + 1)
    in
        n / 1000000.0
    end
end
*)

(* instrumentation ========================================================= *)

val doInstrument = false

fun timerNow () =
  if doInstrument then Tm.now () else Tm.zeroTime

val idleTimer = Array.array (numberOfProcessors, Tm.zeroTime)
val dealTimer = Array.array (numberOfProcessors, Tm.zeroTime)
val handleResumedTimer = Array.array (numberOfProcessors, Tm.zeroTime)

val dealCounter = Array.array (numberOfProcessors, 0)
val failedDealCounter = Array.array (numberOfProcessors, 0)
val newThreadCounter = Array.array (numberOfProcessors, 0)
val switchCounter = Array.array (numberOfProcessors, 0)

fun incrementCounter (cs, p) =
  if not doInstrument then ()
  else Array.update (cs, p, 1 + Array.sub (cs, p))

fun updateTimer (ts, p) (t1, t2) =
  if not doInstrument then () else
  let
    val delta = Tm.- (t2, t1)
  in
    Array.update (ts, p, Tm.+ (delta, Array.sub (ts, p)))
  end

fun sumTimer data =
  let
    val us = Tm.toMicroseconds (Array.foldl Tm.+ Tm.zeroTime data)
  in
    Real.fromLargeInt us / 1000000.0
  end

fun timerString (name, data) =
  name ^ "\t" ^ Real.fmt (StringCvt.FIX (SOME 3)) (sumTimer data) ^ "s"

fun counterString (name, data) =
  name ^ "\t" ^ Int.toString (Array.foldl op+ 0 data)

fun timerStrings () =
  String.concatWith "\n"
    [ timerString ("idle", idleTimer)
    , timerString ("deal", dealTimer)
    , counterString ("deals", dealCounter)
    , counterString ("~deals", failedDealCounter)
    , counterString ("new thd", newThreadCounter)
    , counterString ("switch", switchCounter)
    ]
    
val _ = OS.Process.atExit (fn _ =>
  if not doInstrument then ()
  else print (timerStrings () ^ "\n"))

(* end instrumentation ===================================================== *)

fun queueIndex (p, r) =
  p * (P.count ()) + (P.toInt r) - 1

fun mb (p, r) =
  A.sub (!mailboxes, queueIndex (p, r))
fun queue (p, r) =
  A.sub (!queues, queueIndex (p, r))

fun curPrio p =
    A.sub (curprios, p)
fun setPrio (p, r) =
    A.update (curprios, p, r)

fun getDepth p =
    A.sub (depth, p)
fun setDepth (p, d) =
    A.update (depth, p, d)

fun incTopPrio p newPrio =
    let val pto = P.toInt newPrio
        val tpref = V.sub (topprios, p)
        fun try () =
            let val ctprio = !tpref
            in
                if ctprio >= pto then
                    (* Priority is currently higher. Do nothing. *)
                    ()
                else
                    if MLton.Parallel.compareAndSwap
                           tpref (ctprio, pto) = ctprio
                    then
                        (* Success *)
                        ()
                    else (* Try again *)
                        try ()
            end
    in
        try ()
    end

fun workOnTask p (w, d, _) =
    (A.update (depth, p, d);
     case w of
         Task.Thunk f => (f ();
                     raise ShouldntGetHere)
       | Task.Thread t =>
           ( incrementCounter (switchCounter, p)
           ; T.switch (fn _ => t)))

fun newNextSwitch () =
    Tm.+ (Tm.now (), switchInterval)

fun newNextDeal () = (* Tm.+ (Tm.now (), dealInterval) *)
    let (* val df = Real.fromLargeInt (Time.toMicroseconds dealInterval) *)
        val iv = dIf * Real.~ (Math.ln (R.rand01ex ()))
        val iiv = Real.round iv
    in
        Tm.+ (Tm.now (), Tm.fromMicroseconds (IntInf.fromInt iiv))
    end

fun switchPrios p =
    (A.update (prios, p, { primary = P.chooseFromDist (R.rand01 ()),
                          secondary = P.top (),
                          send = P.chooseFromDist (R.rand01 ()) }))
    (* handle Subscript => (log (~1) (fn _ => "switchPrios\n"); raise Subscript)) *)
(*;
     log 6 (fn _ => "switched to " ^ (P.toString (#primary (A.sub (prios, p)))) ^ "\n"))*)
     (*;
     log ~1 (fn _ => "blah"))(*
     log ~1 (fn _ => "switched to " ^ (P.toString (#primary (A.sub (prios, p)))) ^ "\n"))*)*)

fun maybeSwitchPrios p =
    let val ns = A.sub (nextSwitch, p)
    in
        if Tm.> (Tm.now (), ns) then
            (switchPrios p;
             A.update (nextSwitch, p, newNextSwitch ()))
        else
            ()
    end

fun PROfRequest n =
    (n mod numberOfProcessors, P.fromInt (n div numberOfProcessors))

fun requestOfPR (p, r) =
    (P.toInt r) * numberOfProcessors + p

fun dealAttempt (p, r) =
    let val t1 = timerNow () 
        val req = !(V.sub (reqCells, p))
        fun randP () =
            let val p' = R.randInt (0, numberOfProcessors - 2)
            in if p' >= p then p' + 1 else p'
            end
        val (p', r) =
            if req > 0 then
                let val (p', r') = PROfRequest req
                    val q = queue (p, r')
                    val _ = V.sub (reqCells, p) := ~1
                in
                    if Q.isEmpty q then
                        (randP (), r)
                    else
                        (p', r')
                end
            else
                (randP (), r)
        val m = mb (p', r)
        val q = queue (p, r)
        val _ = log 2 (fn _ =>"deal attempt on " ^ (Int.toString p) ^
                       " at " ^ (P.toString r) ^ "\n")
(*        fun length q =
            let val (_, _, es) = !q
            in
                List.length es
            end*)
        val _ = case (Q.size q <= 1, M.status m) of
            (true, _) => (log 2 (fn _ => "nothing to send\n");
                          incrementCounter (failedDealCounter, p);
                          () (* Nothing to send; don't bother. *))
          | (_, M.Waiting) =>
            (if M.tryClaim m p then
                 (* We've claimed this mailbox. Send half. *)
                 (log 2 (fn _ =>"deal " ^ (Int.toString p) ^ " -> " ^
                         (Int.toString p') ^ "\n");
                 case Q.split q of
                     SOME ts => (M.sendMail m ts;
                                 incrementCounter (dealCounter, p);
                                 log 2 (fn _ =>"sent " ^ (Int.toString (Q.numts ts))
                                        ^ "; " ^ (Int.toString (Q.size q)) ^
                                        " left\n");
                                incTopPrio p' r)
                   | NONE => log 2 (fn _ => "something weird\n"))
             else
             (* We failed to claim the mailbox. Give up. *)
                 (log 2 (fn _ => "failed to claim\n");
                  incrementCounter (failedDealCounter, p);
                  ()))
          | _ => (* Mailbox is claimed or not waiting. *)
            (log 2 (fn _ => "mb claimed/not waiting\n");
             incrementCounter (failedDealCounter, p);
             ())
      val _ = updateTimer (dealTimer, p) (t1, timerNow ())
    in
      ()
    end

fun maybeDeal p (*(p, prios)*) =
    if numberOfProcessors <= 1 then () else
    let val _ = log 4 (fn _ => "maybeDeal " ^ (Int.toString p) ^ "\n")
        val nd = A.sub (nextDeal, p)
    in
        if Tm.> (Tm.now (), nd) then
            (log 3 (fn _ => "attempting " ^ (Int.toString p) ^ "\n");
             (* dealAttempt (p, #send prios); *)
             dealAttempt (p, curPrio p);
             log 3 (fn _ => "attempted " ^ (Int.toString p) ^ "\n");
             A.update (nextDeal, p, newNextDeal ());
             log 3 (fn _ => "updated " ^ (Int.toString p) ^ "\n")
            )
        else
            log 4 (fn _ => "not time " ^ (Int.toString p) ^ "\n")
    end

fun makeRequest (p, r) =
    if numberOfProcessors = 1 then ()
    else
        let val p' = R.randInt (0, numberOfProcessors - 2)
            val p' = if p' >= p then p' + 1 else p'
            val c = V.sub (reqCells, p')
            val req = requestOfPR (p, r)
        in
            (* if P.pe (r, P.bot) then *)
            if !c = ~1 then
                ignore (MLton.Parallel.compareAndSwap c (~1, req))
            else
                ()
                    (* else () *)
        end

fun pushAll ((p, r), (ts: Q.task_set)) =
    let val q = Q.fromSet ts
    in
        Q.app (fn ((_, _, c), h) => C.push c (q, h)) q;
        A.update (!queues, queueIndex (p, r), q)
    end

fun tryClearFlag (m, p, r) =
    case M.status m of
        M.Waiting =>
        (case M.tryClear m of
             SOME ts => pushAll ((p, r), ts)
           | NONE => ())
      | _ => ()

                 (*
fun pushOrInsert insched f (r, t) =
    let val p = processorNumber ()
        val _ = I.block p
        val m = mb (p, r)
        val q = queue (p, r)
        (* val _ = log 7 (fn _ => "pushing work at " ^ (P.toString r) ^ "\n") *)
    in
        tryClearFlag (m, p, r);
        f (q, t);
        incTopPrio p r;
        I.unblock p
    end
                 *)
fun insertInt insched (r, t) =
    let val p = processorNumber ()
        val insched = true
        val _ = if insched then () else I.block p
        val m = mb (p, r)
                (* handle Subscript => (log (~1) (fn _ => "306\n"); raise Match) *)
        val q = queue (p, r)
                (* handle Subscript => (log (~1) (fn _ => "308\n"); raise Bind) *)
        (* val _ = log 7 (fn _ => "pushing work at " ^ (P.toString r) ^ "\n") *)
        val _ = tryClearFlag (m, p, r)
        val h = Q.insert (q, t)
    in
        C.push (Task.cancellable t) (q, h);
        incTopPrio p r;
        if insched then () else I.unblock p;
        h
    end

val insert = insertInt false

fun pushInt (r, t) =
    let val p = processorNumber ()
        val q = queue (p, r)
                      (* val _ = log 7 (fn _ => "pushing work at " ^ (P.toString r) ^ "\n") *)
        val h = Q.push (q, t)
    in
        C.push (Task.cancellable t) (q, h);
        h
    end

fun push (r, t) =
    let val p = processorNumber ()
        val _ = I.block p
        val q = queue (p, r)
        (* val _ = log 7 (fn _ => "pushing work at " ^ (P.toString r) ^ "\n") *)
        val h = Q.push (q, t)
    in
        C.push (Task.cancellable t) (q, h);
        maybeDeal p;
        I.unblock p;
        h
    end

fun tryRemove (r, h) =
    let val p = processorNumber ()
        val _ = I.block p
        val q = queue (p, r)
        val removed = Q.tryRemove (q, h)
    in
        (if removed then
             C.pop (Task.cancellable (Q.taskOfHand h))
         else ());
        maybeDeal p;
        I.unblock p;
        removed
    end

        (*
fun tryRemoveThunk (r, h) =
    let val p = processorNumber ()
        val _ = I.block p
        val t = Q.taskOfHand h
    in
        (case t of
             (Task.thunk f, _, _) =>
             if Q.tryRemove (queue (p, r), h) then
                 (C.pop (Task.cancellable (Q.taskOfHand h));
                  SOME f)
             else
                 NONE
           | _ => NONE)
        before
        (maybeDeal p;
         I.unblock p)
    end
*)

fun logPrios n {primary, secondary, send} =
    log n (fn _ => "{" ^ (P.toString primary) ^ ", " ^ (P.toString secondary) ^ ", " ^
           (P.toString send) ^ "}\n")

fun advancePrios p =
    let val {primary, secondary, send} = A.sub (prios, p)
    in
        (* logPrios 4 (A.sub (prios, p)); *)
        A.update (prios, p, {primary = primary,
                             secondary = P.next secondary,
                             send = send}) (*;
        logPrios 4 (A.sub (prios, p)) *)
    end

fun newTask (w : Task.work) : Task.t =
    let val p = processorNumber ()
        val d = A.sub (depth, p)
        val _ = A.update (depth, p, d + 1)
    in
        (w, d + 1, C.new ())
    end

fun handleResumed p =
    let val ioq = A.sub (ioqueues, p)
        fun resume (t, r) =
            ((* print ("resume at " ^ (Priority.toString r) ^ "\n"); *)
             ignore (insertInt true (r, t)))
        val ioq' = IOQ.process resume ioq
    in
        A.update (ioqueues, p, ioq')
    end

fun pushCC (r', c, g) =
    let val p = processorNumber ()
        val _ = I.block p
        val d = A.sub (depth, p)
        val r = curPrio p
        val _ = incrementCounter (newThreadCounter, p)
        fun f k =
            let val kt = case C.workingOn () of
                             SOME c => (Task.Thread (T.prepare (k, ())), d + 1, c)
                           | NONE => raise ShouldntGetHere
                val h = pushInt (r, kt)
            in
                T.prepare (T.new g, (r, h))
            end
    in
        setPrio (p, r');
        (* A.update (curprios, p, r'); *)
        setDepth (p, d + 1);
        C.workOn c;
        I.unblock p;
        T.switch f
    end

fun switchTo h =
    let val (w, d, c) = Q.taskOfHand h
    in
        C.workOn c;
        setDepth (processorNumber (), d);
        case w of
            Task.Thread t => T.switch (fn _ => t)
          | Task.Thunk f => f ()
          | Task.Empty => raise ShouldntGetHere
    end

fun schedule p kt =
    let (* val _ = log 5 (fn _ => "schedule " ^ (Int.toString p) ^ "\n") *)
        val t1 = timerNow ()
        val _ = maybeSwitchPrios p
        val prio_rec = A.sub (prios, p)
        val _ = handleResumed p
        val _ = maybeDeal p (*(p, prio_rec)*)
        val _ = case kt of
                    SOME kt => ignore (pushInt (curPrio p, kt))
                  | NONE => ()
        fun getWorkAt r =
            let val _ = log 6 (fn _ => "getting work at " ^ (P.toString r) ^ "\n")
                val m = mb (p, r)
                val q = queue (p, r)
            in
                case Q.choose q of
                    NONE => (M.setWaiting m
                              (*;
                             if (P.pe (r, P.bot)) then
                                 makeRequest (p, r)
                             else ()*);
                             case M.getMail m of
                                 NONE => NONE
                               | SOME tasks => (pushAll ((p, r), tasks);
                                                Q.choose (queue (p, r))))
                  | SOME x => (if Cancellable.isCancelled (Task.cancellable x)
                               then (log ~1 (fn _ => "cancelled but not removed\n");
                                     getWorkAt r)
                               else SOME x)
            end
        fun iterGetWork r =
            case getWorkAt r of
                SOME x => (r, SOME x)
              | NONE => (if P.pe (r, P.bot) then
                             (r, NONE)
                         else
                             iterGetWork (P.next r))
        (* First try getting work at the primary priority *)
        (* val _ = logPrios 4 prio_rec *)
        val (prio, t) = case getWorkAt (#primary prio_rec) of
                            SOME t => (#primary prio_rec, SOME t)
                          | NONE =>
        (* If there's no work there, try the stored top priority. *)
                            (makeRequest (p, #primary prio_rec);
                             let val top =
                                     P.fromInt (!(V.sub (topprios, p)))
                             in
                                 case getWorkAt top of
                                     SOME t => (top, SOME t)
                                   | NONE =>
                                     (let val (r, t) = iterGetWork (P.top ())
                                      in
                                      (* Reset the stored top priority. There's
                                       * a race condition here, but that OK
                                       * since this is just a heuristic anyway *)
                                          V.sub (topprios, p) := P.toInt r;
                                          (r, t)
                                      end)
                             end)
        val t2 = timerNow ()
        val _ = updateTimer (idleTimer, p) (t1, t2)
    in
        case t of
            SOME t =>
            (* Do the work. It shouldn't return. *)
            (C.pop (Task.cancellable t);
             C.workOn (Task.cancellable t);
             setPrio (p, prio);
             (* A.update (curprios, p, prio); *)
             (* log 6 (fn _ => "found work at " ^ (P.toString prio) ^ "\n"); *)
             I.unblock p;
             workOnTask p t;
             raise ShouldntGetHere)
          | NONE =>
            (* Continue scanning through secondary priorities *)
            ((* advancePrios p; *)
             schedule p NONE)
    end

fun suspend (f: P.t * Task.t -> unit) : unit =
    T.switch (fn k =>
                 let val p = processorNumber ()
                     val _ = I.block p
                     val r = curPrio p
                     val d = A.sub (depth, p)
                     val c = case C.workingOn () of
                                 SOME c => c
                               | NONE => raise ShouldntGetHere
                     val t = (Task.Thread (T.prepare (k, ())), d, c)
                     val _ = f (r, t)
                     val _ = incrementCounter (newThreadCounter, p)
                     val _ = incrementCounter (switchCounter, p)
                 in
                   T.prepare (T.new (schedule p), NONE)
                 end)

fun suspendIO (f: unit -> bool) =
    suspend (fn (r, t) => let val p = processorNumber ()
                              val q = A.sub (ioqueues, p)
                          in
                              A.update (ioqueues, p, (f, t, r)::q)
                          end)

fun returnToSched () =
    let val p = processorNumber ()
    in
        I.block p;
        schedule p NONE
    end

fun initPriorities () = P.init ()
fun finalizePriorities () = P.check ()

fun interruptHandler (p, k) =
    let (* val _ = log 4 (fn _ => "interrupt on " ^ (Int.toString p) ^ "\n") *)
        val d = A.sub (depth, p)
        val _ = incrementCounter (newThreadCounter, p)
        val kt = case C.workingOn () of
                     SOME c => if C.isCancelled c then
                                   (log ~1 (fn _ => "thread was cancelled\n");
                                    NONE)
                               else
                                   SOME (Task.Thread k, d, c)
                   | NONE => NONE
    in
        T.prepare (T.new (schedule p), kt)
    end

fun prun () =
    let val p = processorNumber ()
    in
        I.init interruptHandler (Tm.toTime interruptInterval);
        log 1 (fn _ => "initialized " ^ (Int.toString p) ^ "\n");
        I.block p;
        schedule p NONE
    end

fun init () =
    let val p = processorNumber ()
    in
        mailboxes := A.tabulate (numberOfProcessors * (P.count ()),
                                 fn _ => M.new ());
        queues := A.tabulate (numberOfProcessors * (P.count ()),
                              fn _ => Q.empty ());
        V.app (fn r => r := P.toInt P.bot) topprios;
        ignore (M.tryClear (mb (0, P.bot)));
        MLton.Parallel.registerProcessorFunction prun;
        MLton.Parallel.initializeProcessors ();
        C.workOn (C.new ());
        I.init interruptHandler (Tm.toTime interruptInterval);
        log 1 (fn _ => "initialized")
    end

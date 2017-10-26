structure Tm = Time
structure A = Array
structure V = Vector
structure P = Priority
structure I = Interrupt
structure T = MLton.Thread
structure R = UsefulRandom
structure MT = MersenneTwister

structure Task =
struct
datatype work =
         Empty
         | Thunk of unit -> unit
         | Thread of T.Runnable.t
type t = work * int
val default = (Empty, 0)
fun depth (_, w) = w
end

structure M = Mailbox
structure Q = BinaryHeapQueue(Task)

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

val numberOfProcessors = MLton.Parallel.numberOfProcessors
val processorNumber = MLton.Parallel.processorNumber

val printMutex = ref 0
fun lockMutex m =
    if !m <> 0 then
        lockMutex m
    else
        if MLton.Parallel.compareAndSwap m (0, 1) = 0 then
            ()
        else
            lockMutex m
fun unlockMutex m = m := 0

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
val dealInterval = Tm.fromMicroseconds 200
val dIf = 200.0
val interruptInterval = Tm.fromMicroseconds 500


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

fun mb (p, r) =
    A.sub (!mailboxes, p * (P.count ()) + (P.toInt r) - 1)
fun queueIndex (p, r) = p * (P.count ()) + (P.toInt r) - 1
fun queue (p, r) =
    A.sub (!queues, queueIndex (p, r))

fun curPrio p =
    A.sub (curprios, p)

fun workOnTask p (w, d) =
    (A.update (depth, p, d);
     case w of
         Task.Thunk f => (f ();
                     raise ShouldntGetHere)
       | Task.Thread t => T.switch (fn _ => t))

fun newNextSwitch () =
    Tm.+ (Tm.now (), switchInterval)

fun newNextDeal () = (* Tm.+ (Tm.now (), dealInterval) *)
    let (* val df = Real.fromLargeInt (Time.toMicroseconds dealInterval) *)
        val iv = dIf * Math.ln (R.rand01ex ())
        val iiv = Real.round iv
    in
        Tm.- (Time.now (), Tm.fromMicroseconds (IntInf.fromInt iiv))
    end

fun switchPrios p =
    A.update (prios, p, { primary = P.chooseFromDist (R.rand01 ()),
                          secondary = P.top,
                          send = P.chooseFromDist (R.rand01 ()) })

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
    let val req = !(V.sub (reqCells, p))
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
    in
        case (Q.size q <= 1, M.status m) of
            (true, _) => (log 2 (fn _ => "nothing to send\n");
                          () (* Nothing to send; don't bother. *))
          | (_, M.Waiting) =>
            (if M.tryClaim m p then
                 (* We've claimed this mailbox. Send half. *)
                 (log 2 (fn _ =>"deal " ^ (Int.toString p) ^ " -> " ^
                         (Int.toString p') ^ "\n");
                 case Q.split q of
                     SOME ts => (M.sendMail m ts;
                                 log 2 (fn _ =>"sent " ^ (Int.toString (Q.numts ts))
                                        ^ "; " ^ (Int.toString (Q.size q)) ^
                                        " left\n"))
                   | NONE => log 2 (fn _ => "something weird\n"))
             else
             (* We failed to claim the mailbox. Give up. *)
                 (log 2 (fn _ => "failed to claim\n");
                  ()))
          | _ => (* Mailbox is claimed or not waiting. *)
            (log 2 (fn _ => "mb claimed/not waiting\n");
             ())
    end

fun maybeDeal (p, prios) =
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
        let val p' = Int.div (p, 2) (*  R.randInt (0, numberOfProcessors - 2) *)
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
    A.update (!queues, queueIndex (p, r),
              Q.fromSet ts)

fun tryClearFlag (m, p, r) =
    case M.status m of
        M.Waiting =>
        (case M.tryClear m of
             SOME ts => pushAll ((p, r), ts)
           | NONE => ())
      | _ => ()

fun pushOrInsert f (r, t) =
    let val p = processorNumber ()
        val _ = I.block p
        val m = mb (p, r)
        val q = queue (p, r)
        (* val _ = log 7 (fn _ => "pushing work at " ^ (P.toString r) ^ "\n") *)
    in
        f (q, t);
        tryClearFlag (m, p, r);
        I.unblock p
    end
val push = pushOrInsert Q.push
val insert = pushOrInsert Q.insert

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
        (w, d + 1)
    end

fun handleResumed p =
    let val ioq = A.sub (ioqueues, p)
        fun resume (t, r) =
            let val q = queue (p, r)
            in
                Q.insert (q, t)
            end
        val ioq' = IOQ.process resume ioq
    in
        A.update (ioqueues, p, ioq')
    end

fun schedule p kt =
    let (* val _ = log 5 (fn _ => "schedule " ^ (Int.toString p) ^ "\n") *)
        val _ = maybeSwitchPrios p
        val prio_rec = A.sub (prios, p)
        val _ = handleResumed p
        val _ = if numberOfProcessors > 1 then maybeDeal (p, prio_rec)
                else ()
        val _ = case kt of
                    SOME kt => push (curPrio p, kt)
                  | NONE => ()
        fun getWorkAt r =
            let (* val _ = log 6 (fn _ => "getting work at " ^ (P.toString r) ^ "\n") *)
                val m = mb (p, r)
                val q = queue (p, r)
            in
                case Q.choose q of
                    NONE => (M.setWaiting m;
                             if (P.pe (r, P.bot)) then
                                 makeRequest (p, r)
                             else ();
                             case M.getMail m of
                                 NONE => NONE
                               | SOME tasks => (pushAll ((p, r), tasks);
                                                Q.choose (queue (p, r))))
                  | SOME x => SOME x
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
                          | NONE => iterGetWork P.top
    in
        case t of
            SOME t =>
            (* Do the work. It shouldn't return. *)
            (A.update (curprios, p, prio);
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
                     val t = (Task.Thread (T.prepare (k, ())), d)
                     val _ = f (r, t)
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

fun finalizePriorities () = P.init ()

fun interruptHandler (p, k) =
    let (* val _ = log 4 (fn _ => "interrupt on " ^ (Int.toString p) ^ "\n") *)
        val d = A.sub (depth, p) in
        T.prepare (T.new (schedule p), SOME (Task.Thread k, d))
    end

fun prun () =
    let val p = processorNumber ()
    in
        I.init interruptHandler interruptInterval;
        log 1 (fn _ => "initialized " ^ (Int.toString p) ^ "\n");
        schedule p NONE
    end

fun init () =
    let val p = processorNumber ()
    in
        P.check ();
        mailboxes := A.tabulate (numberOfProcessors * (P.count ()),
                                 fn _ => M.new ());
        queues := A.tabulate (numberOfProcessors * (P.count ()),
                              fn _ => Q.empty ());
        ignore (M.tryClear (mb (0, P.bot)));
        MLton.Parallel.registerProcessorFunction prun;
        MLton.Parallel.initializeProcessors ();
        I.init interruptHandler interruptInterval;
        log 1 (fn _ => "initialized")
    end

structure Tm = Time
structure A = Array
structure P = Priority
structure I = Interrupt
structure T = MLton.Thread

structure Task =
struct
datatype work =
         Thunk of unit -> 'a
         | Thread of T.Runnable.t
type t = work * int
fun weight (_, w) = w
end

structure M = Mailbox
structure Q = Queue(Task)

exception ShouldntGetHere

type priorities =
     { primary: P.t,
       secondary: P.t,
       send: P.t }

structure IOQ =
struct
type t = ((unit -> bool) * Task.t * P.t) list
fun new () = []
fun process (resume: Task.t * P.t -> unit) : t =
    List.foldl (fn ((f, t, r), l) =>
                   if f () then
                       (resume (t, r);
                        l)
                   else
                       (f, t)::l)
               []
end

val numberOfProcessors = MLton.Parallel.numberOfProcessors
val processorNumber = MLton.Parallel.processorNumber

(*** Constants ***)
val switchInterval = Tm.fromMicroseconds 10000
val dealInterval = Tm.fromMicroseconds 200
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

(* These are initialized after the priorities. *)
val mailboxes = ref (A.fromList [])
val queues = ref (A.fromList [])

fun mb (p, r) =
    A.sub (mailboxes, p * numberOfPriorities + r)
fun queueIndex (p, r) = p * numberOfPriorities + r
fun queue (p, r) =
    A.sub (queues, queueIndex (p, r))

fun curPrio p =
    A.sub (curprios, p)

fun workOnTask p (w, d) =
    (A.update (depth, p, d);
     case w of
         Thunk f => f (); raise ShouldntGetHere
       | Thread t => T.switch (fn _ => t))

fun newNextSwitch old =
    Tm.+ (old, switchInterval)

fun newNextDeal old =
    let val df = Real.fromInt (Time.toMicroseconds dealInterval)
        val iv = df * Math.ln (UsefulRandom.rand01ex ())
        val iiv = Real.round iv
    in
        Tm.- (old, Tm.fromMicroseconds iv)
    end

fun switchPrios p =
    A.update (prios, p, { primary = P.chooseFromDist (),
                          secondary = P.top,
                          send = P.chooseFromDist () })

fun maybeSwitchPrios p =
    let val ns = A.sub (nextSwitch, p)
    in
        if Tm.> (Tm.now (), ns) then
            (switchPrios p;
             A.update (nextSwitch, p, newNextSwitch ns))
        else
            ()
    end

fun dealAttempt (p, r) =
    let val p' = R.randInt (0, numberOfProcessors - 2)
        val p' = if p' >= p then p' + 1 else p'
        val m = mb (p', r)
        val q = queue (p, r)
    in
        case (Q.isEmpty q, M.status m) of
            (true, _) => () (* Nothing to send; don't bother. *)
          | (_, Waiting) =>
            (if M.tryClaim m p then
                 (* We've claimed this mailbox. Send half. *)
                 case Q.split q of
                     SOME ts => M.sendMail m ts
                   | NONE => ()
             else
             (* We failed to claim the mailbox. Give up. *)
                 ())
          | _ => (* Mailbox is claimed or not waiting. *)
            ()
    end

fun maybeDeal (p, prios) =
    let val nd = A.sub (nextDeal, p)
    in
        if Tm.> (Tm.now (), nd) then
            (dealAttempt (p, #send prios);
             A.update (nextDeal, p, newNextDeal ns))
        else
            ()
    end

fun tryClearFlag (m, q) (p, r) =
    in
        case M.status m of
            Waiting =>
            (case M.tryClear m of
                 SOME t => Q.push (q, t)
               | NONE => ())
          | _ => ()
    end

fun pushOrInsert f (r, t) =
    let val p = processorNumber ()
        val m = mb (p, r)
        val q = q (p, r)
    in
        f (q, t);
        tryClearFlag (m, q) (p, r)
    end
val push = pushOrInsert Q.push
val insert = pushOrInsert Q.insert

fun advancePrios p =
    let val {primary, secondary, send} = A.sub (prios, p)
    in
        A.update (prios, p, {primary = primary,
                             secondary = P.next secondary,
                             send = send})
    end

fun newTask (w : Task.work) : Task.t =
    let val p = processorNumber
        val d = A.sub (depth, p)
        val _ = A.update (depth, p, d + 1)
    in
        (w, d + 1)
    end

fun suspend (f: P.t * Task.t -> unit) -> unit =
    T.switch (fn k =>
                 let val p = processorNumber ()
                     val r = curPrio p
                     val d = A.sub (depth, p)
                     val t = (Thread (T.prepare (k, ())), d)
                     val _ = f (r, t)
                 in
                     T.new (schedule p NONE)
                 end)

fun suspendIO (f: unit -> bool) =
    suspend (fn (r, t) => let val p = processorNumber
                         val q = A.sub (ioqueues, p)
                     in
                         A.update (ioqueues, p, (f, t, r)::q)
                     end)

fun handleResumed p =
    let val ioq = A.sub (ioqueues, p)
        fun resume (t, r) =
            let val q = queue (p, r)
            in
                Q.push (q, t)
            end
        val ioq' = IOQ.process resume ioq
    in
        A.update (ioqueues, p, ioq')
    end



fun schedule p kt =
    let val _ = maybeSwitchPrios p
        val prio_rec = A.sub (prios, p)
        val _ = handleResumed p
        val _ = maybeDeal p
        val _ = case kt of
                    SOME kt => push (p, curPrio p, kt)
                  | NONE => ()
        fun getWorkAt r =
            let val m = mb (p, pr)
                val q = queue (p, r)
                val _ = case M.getMail m of
                            NONE => ()
                          | SOME tasks =>
                            A.update (queues, queueIndex (p, r),
                                      Q.fromSet tasks)
            in
                Q.choose q
            end
        (* First try getting work at the primary priority *)
        val prio = #primary prio_rec
        val t = getWorkAt prio
        (* If there's no work there, try the secondary. *)
        val (prio, t) = case t of
                    SOME t => (prio, SOME t)
                  | NONE => (#secondary prios, getWorkAt (#secondary prio_rec))
    in
        case t of
            SOME t =>
            (* Do the work. It shouldn't return. *)
            (A.update (curprios, p, prio);
             I.unblock p;
             workOnTask t;
             raise ShouldntGetHere)
          | NONE =>
            (* Continue scanning through secondary priorities *)
            (advancePrios p;
             schedule p NONE)
    end
end

fun returnToSched () =
    let val p = processorNumber ()
    in
        schedule p NONE
    end

fun finalizePriorities () = P.init ()

fun interruptHandler (p, k) =
    T.switch (schedule p (SOME k))

fun prun () =
    let val p = processorNumber ()
    in
        I.init interruptHandler;
        schedule p NONE
    end

fun init () =
    let val p = processorNumber ()
    in
        P.check ();
        mailboxes := A.tabulate (numberOfProcessors * numberOfPriorities,
                                 fn _ => M.new ());
        queues := A.tabulate (numberOfProcessors * numberOfPriorities,
                              fn _ => Q.empty ());
        MLton.Parallel.registerProcessorFunction prun;
        I.init interruptHandler;
        schedule p NONE
    end

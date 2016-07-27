functor WorkQueue (structure W : sig type work
                                     val numberOfProcessors : unit -> int
                                     val procio : int -> bool
                                     val block_sigs : unit -> unit
                                     val unblock_sigs : unit -> unit
                                 end) =
struct

val delta = 100 (* microseconds *)

type proc = int
type work = W.work

exception WorkQueue
exception QueueSize

structure A = Array
structure V = Vector
structure T = Time
val numberOfProcessors = W.numberOfProcessors ()
val P = numberOfProcessors

val yield = _import "Parallel_yield" runtime private: unit -> unit;
val compareAndSwap = _import "Parallel_compareAndSwap" runtime private: int ref * int * int -> bool;

val WORK_ARRAY_SIZE = 1024
(* private state *)
datatype token = Token of int * queue option ref
and entry = Empty | Work of (token * W.work (* * int *))
and queue = Queue of
            {
              top : int ref,
              bottom : int ref,
              work : entry A.array ref,
              lat : bool
            }

fun newQueue lat =
    Queue {
      top = ref 0,
      bottom = ref 0,
      work = ref (A.array (WORK_ARRAY_SIZE, Empty)),
      lat = lat
    }

val fqueues = A.array (P, newQueue true)
val bqueues = A.array (P, newQueue false)

val successfulSteals = ref 0
fun resetSteals () = successfulSteals := 0
fun incSteals () = successfulSteals := !successfulSteals + 1
fun reportSuccessfulSteals () = !successfulSteals
fun reportFailedSteals () = 0

val waiting = ~1
val notwaiting = ~2
type cellval = (bool * W.work) option
val commcells = V.tabulate (P, fn _ => ref notwaiting)
val taskcells = A.array (P, NONE)
val nextdeal = A.array (P, T.zeroTime)

type susp = queue option

val nextToken = ref 0

fun newWork p = Token (!nextToken, ref NONE)
                before (nextToken := !nextToken + 1)

fun stringOfToken (Token (i, _)) = Int.toString i

fun resize (Queue {top, bottom, work, ... }) =
    let
        val i = !top
        val j = !bottom
        val w = !work
        val l = A.length w
    in
        if i - j < l div 2 then
            let
                val () = print "copying!"
                fun erase k = if k = i then ()
                              else (A.update (w, k, Empty);
                                    erase (k + 1))
                fun copy k = if k = i then erase j
                             else (A.update (w, k - j, A.sub (w, k));
                                   copy (k + 1))
            in
                copy j;
                top := i - j;
                bottom := 0
            end
        else
            let
                val () = print "resizing!"
                val w = A.tabulate (l * 2,
                                    fn k => if k < (i - j) then A.sub (w, k + j)
                                            else Empty)
            in
                work := w;
                top := i - j;
                bottom := 0
            end
    end

fun add (q as Queue {top, bottom, work, ... }) (tw as (t, w)) =
    let
        val i = !top
        val () = if i = A.length (!work) then resize q else ()
        val i = !top (* in case of resize *)
        val Token (_, qr) = t
    in
        A.update (!work, i, Work tw);
        qr := SOME q;
        top := i + 1
    end

fun popTop (q as Queue {top, bottom, work, ... }) =
    let val i = !top
        val j = !bottom
        val w = !work
    in
        if i = j then raise WorkQueue (* empty *)
        else
            (top := i - 1;
             case A.sub (w, i - 1) of
                 Empty => raise WorkQueue
               | Work (t, w) => w)
    end

fun dealAttempt p =
    let val (fq as Queue {top, bottom, work, ...}) = A.sub (fqueues, p)
        val fi = !top
        val fj = !bottom
        val fw = !work
        val (bq as Queue {top, bottom, work, ...}) = A.sub (bqueues, p)
        val bi = !top
        val bj = !bottom
        val bw = !work
    in
        if fi = fj andalso bi = bj then (* both empty *)
            false
        else
            let val p' = Word.toIntX (MLtonRandom.rand ()) mod P
                val c = V.sub (commcells, p')
            in
                if p' = p then false
                else
                    if !c = waiting then
                        case A.sub (taskcells, p) of
                            NONE =>
                            if compareAndSwap (c, waiting, p) then
                                let val (isLat, q) =
                                        if fi = fj then (false, bq)
                                        else (true, fq)
                                in
                                    A.update (taskcells, p,
                                              SOME (isLat, popTop q));
                                    true
                                end
                            else
                                false
                          | SOME _ => false
                    else false
            end
    end

fun communicate p =
    if Time.> (Time.now (), A.sub (nextdeal, p)) then
        (dealAttempt p;
         let val now = Time.now ()
             val mi = case Int.maxInt of SOME i => i | NONE => 1000000
             val rnd = Real.abs
                           (Real./
                            (Real.fromInt (Word.toIntX (MLtonRandom.rand ())),
                             Real.fromInt mi))
                       handle Overflow => (print "here 171\n"; raise Overflow)
             val mult = Math.ln rnd
             val us = LargeInt.fromInt
                          (Real.floor (Real.* (Real.fromInt delta, mult)))
         in
             A.update (nextdeal, p, Time.- (Time.now (),
                                            Time.fromMicroseconds us))
         end)
    else
        ()


fun addWork (lat, p, tws) =
    let val q = if lat then A.sub (fqueues, p) else A.sub (bqueues, p)
    in
        ignore (List.map (add q) tws)
    end

fun maybeMove {top, bottom, work} =
    let val i = !top
        val j = !bottom
    in
        if i = j then
            (top := 0;
             bottom := 0)
        else
            ()
    end

fun getWorkLat lat p =
    let val q = if lat then A.sub (fqueues, p) else A.sub (bqueues, p)
        val Queue {top, bottom, work, ...} = q
        val i = !top
        val j = !bottom
        val w = !work
        val c = V.sub (commcells, p)
        fun waitForTask' p' =
            case A.sub (taskcells, p') of
                NONE => waitForTask' p'
              | SOME bw => bw
        fun waitForTask () =
            let val p' = !c
            in
                if p' = waiting then
                    (W.procio p;
                     let val i = !top (* array may have changed *)
                         val j = !bottom
                         val w = !work
                     in
                         if i = j then (* Still no task *)
                             (yield ();
                              communicate p;
                              waitForTask ())
                         else
                             (* We have a task now; try to signal notwaiting *)
                             if compareAndSwap (c, waiting, notwaiting) then
                                 (* Succeeded; give back the bottom task *)
                                 case A.sub (w, j) of
                                     Empty => raise WorkQueue
                                   | Work (_, wrk) =>
                                     (bottom := j + 1;
                                      (lat, wrk))
                             else
                                 (* Got a new task in the meantime *)
                                 waitForTask' (!c)
                     end)
                else
                    waitForTask' p'
            end
    in
        if i = j then (* empty *)
            let val _ = c := waiting
                val _ = (top := 0; bottom := 0)
                val _ = W.block_sigs ()
                val (isLat, wrk) = waitForTask ()
            in
                W.unblock_sigs ();
                SOME (true, isLat, wrk)
            end
        else (* pop bottom task *)
            case A.sub (w, j) of
                Empty => raise WorkQueue
              | Work (_, wrk) =>
                (bottom := j + 1;
                 SOME (false, lat, wrk))
    end

fun getWork p = getWorkLat true p

fun startWork p = ()

fun finishWork p = ()

fun suspendWork p = NONE

fun resumeWork (lat, p, (t, w)) = addWork (lat, p, [(t, w)])

fun removeWork (p, Token (ti, qr)) =
    case !qr of
        SOME (Queue {top, bottom, work, ...}) =>
        let val tp = !top
            val bot = !bottom
            val w = !work
            fun unloop i =
                if i >= tp - 2 then (top := tp - 1; ())
                else
                    (A.update (w, i, A.sub (w, i + 1));
                     unloop (i + 1))
            fun loop i =
                if i >= tp then false
                else
                    case A.sub (w, i) of
                        Empty => raise WorkQueue
                      | Work (Token (t, _), w) =>
                        if t = ti then
                            (unloop i; true)
                        else
                            loop (i + 1)
        in
            loop bot
        end
      | NONE => raise WorkQueue

fun removeWorkLat (_, p, t) = removeWork (p, t)

fun shouldYield _ = false

val policyName = "Prompt private deques"

end

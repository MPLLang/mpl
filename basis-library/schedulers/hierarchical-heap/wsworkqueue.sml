functor WorkStealing (structure W :
                                sig
                                  type work
                                  val numberOfProcessors: unit -> int
                                end) =
struct

  type proc = int
  type work = W.work
  type unlocker = unit -> unit

  val dbgmsg = fn _ => ()

  val successfulSteals = ref 0
  val failedSteals = ref 0
  fun incr r = r := !r + 1

  val yield = MLton.Parallel.Deprecated.yield

  val lockInit = MLton.Parallel.Deprecated.lockInit
  val takeLock = MLton.Parallel.Deprecated.takeLock
  val releaseLock = MLton.Parallel.Deprecated.releaseLock

  local
    exception Impossible
    open TextIO
  in
  fun die m = (output (stdErr,
                       "WSWorkQueue: died reason:  " ^ m ^ "\n");
               flushOut stdErr;
               (* XX releaseLock (); *)
               raise Impossible)
  end

  exception WorkQueue
  exception QueueSize

  structure A = Array
  structure V = Vector

  val numberOfProcessors = W.numberOfProcessors ()

  val WORK_ARRAY_SIZE = 1024
  (* private state *)
  datatype entry = Empty | Work of token * W.work | Marker
  and queue = Queue of {top : int ref,
                        bottom : int ref,
                        index : int ref,
                        work : entry A.array ref}
  and token = Token of queue option ref
  type susp = queue option

  fun newQueue index =
      Queue {
              top = ref 0,
              bottom = ref 0,
              index = ref index,
              work = ref (A.array (WORK_ARRAY_SIZE, Empty))
            }

  datatype lock = Lock of { thiefLock : Word32.word ref }

  (* protects access to those queues owned by processors *)
  val locks = A.tabulate (numberOfProcessors,
                          fn _ => Lock { thiefLock = ref (Word32.fromInt ~1) })
  val () = A.appi (fn (p, Lock {thiefLock, ...}) =>
                      lockInit thiefLock)
                  locks
  val () = A.appi (fn (p, Lock {thiefLock, ...}) =>
                      MLton.HM.registerQueueLock (Word32.fromInt p, thiefLock))
                  locks

  val QUEUE_ARRAY_SIZE = 8192
  val queues = A.tabulate (QUEUE_ARRAY_SIZE,
                           fn i => if i < numberOfProcessors then
                                     SOME (newQueue i)
                                   else NONE)
  val () = A.appi (fn (p, q) => case q
                                 of SOME (Queue {work = ref q, ...}) =>
                                    MLton.HM.registerQueue (Word32.fromInt p, q)
                                  | _ => ())
                  queues

  fun assertToken location (Token t) p =
      let
          val pString = Int.toString p
      in
          case A.sub (queues, p)
           of NONE => die ("assertToken(" ^ location ^"): Processor " ^
                           pString ^ " does not have queue!")
           | SOME pq => case !t
                         of NONE => die ("assertToken(" ^ location ^
                                         "): t is NONE!")
                          | SOME tq => if tq <> pq
                                       then die ("assertToken(" ^ location ^
                                                 "): Token's queue does not" ^
                                                 "match Processor " ^ pString ^
                                                 " queue!")
                                       else ()
      end

  fun newWork () = Token (ref NONE)

  (* must already hold the lock! *)
  fun resize p (Queue { top, bottom, work, ... }) =
      let
        val i = !top
        val j = !bottom
        val w = !work
        val l = A.length w
      in
        if i - j < l div 2 then
          let
            val () = dbgmsg "copying!"
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
            val () = dbgmsg "resizing!"
            val w = A.tabulate (l * 2,
                                fn k => if k < (i - j) then A.sub (w, k + j)
                                        else Empty)
          in
            work := w;
            top := i - j;
            bottom := 0;
            MLton.HM.registerQueue (Word32.fromInt p, w)
          end
      end

  fun addWork (p, tws) =
    let
      val Lock { thiefLock, ... } = A.sub (locks, p)
      val () = dbgmsg "tyring lock 1\n"
      val () = takeLock thiefLock; (* XTRA *)
      val () = dbgmsg "got lock 1\n"
      val q as Queue { top, work, ... } = case A.sub (queues, p)
                                      of SOME q => q | NONE => (dbgmsg "add\n"; raise WorkQueue)
      fun add (tw as (t as Token r, w)) =
          let
            val i = !top
            val () = if i = A.length (!work) then resize p q else ()
            val i = !top (* in case of resize *)
          in
            r := SOME q;
            A.update (!work, i, Work (t, w));
            top := i + 1;
            assertToken "add" t p
          end
    in
      app add tws;
      releaseLock thiefLock (* XTRA *)
    end

  local
    fun victim p = Word.toIntX (MLton.Random.rand ()) mod numberOfProcessors
  in
  fun getWork p =
    let
      val Lock { thiefLock, ... } = A.sub (locks, p)
      val () = dbgmsg "tyring lock 2\n"
      val () = takeLock thiefLock (* XTRA *)
      val () = dbgmsg "got lock 2\n"

      fun steal () =
          let
            (* Who to steal from? *)
            val p' = victim p
            val unsync =
                let
                  (* Release our own lock *)
                  val () = releaseLock thiefLock (* XTRA *)
                  (* Take the victim's lock *)
                  val Lock { thiefLock, ... } = A.sub (locks, p')
                  val () = dbgmsg "tyring lock 3\n"
                  val () = takeLock thiefLock
                  val () = dbgmsg "got lock 3\n"
                in
                  fn () => (releaseLock thiefLock)
                end
          in
            case A.sub (queues, p')
             of NONE => (incr failedSteals; unsync (); yield (); NONE)
              | SOME (q as Queue { bottom, top, work, index, ... }) =>
                let
                  val j = !bottom
                  val i = !top
                in
                  if i <> j
                  then (* not empty *)
                    let (* Just steal the oldest task *)
                      val w = A.sub (!work, j)
                      val () = A.update (!work, j, Empty)
                    in
                      bottom := j + 1;
                      case w
                       of Empty => raise WorkQueue
                        | Marker => (unsync ();
                                     NONE)
                        | Work tw =>
                          (assertToken "steal" (#1 tw) p';
                           incr successfulSteals;
                           SOME (true, unsync, #2 tw))
                    end
                  else (* empty queue *)
                    (incr failedSteals;
                     unsync (); yield (); NONE)

                end
          end
    in
      case A.sub (queues, p)
           of NONE => steal ()
            | SOME (Queue { top, bottom, work, ... }) =>
              let
                val i = !top
                val j = !bottom
              in
                if i = j then
                  (if i <> 0 then (top := 0; bottom := 0) else (); (* PERF unconditional? *)
                   (* release lock in steal *)
                   steal ())
                else (top := i - 1;
                      case A.sub (!work, i - 1)
                       of Empty => raise WorkQueue
                        | Marker => (A.update (!work, i - 1, Empty);
                                     releaseLock thiefLock;
                                     case getWork p
                                      of NONE => NONE
                                       | SOME (_, unlocker, w) => SOME (true, unlocker, w))
                        | Work tw =>
                          let
                            val () = assertToken "normal" (#1 tw) p
                          in
                            SOME (false, fn () => releaseLock thiefLock, #2 tw)
                          end
                          before (A.update (!work, i - 1, Empty)))
              end
    end
  end

(* what if a job J is added, then that queue is suspended, then resumed by a
  different processor.  then the token (the proc number at the time J was
  added) doesn't reflect which lock should be take nor which queue *)
  fun removeWork (p', Token r) =
      let
        val Queue { index, ... } = case !r of SOME q => q | NONE => (dbgmsg "remove\n"; raise WorkQueue)
(* SPOONHOWER_NOTE: this is a litte SUSP -- what if the queue is moved while we are trying to lock it? *)
        val p = !index
        val unsync =
            if p < numberOfProcessors then
              let
                val Lock { thiefLock, ... } = A.sub (locks, p)
                val () = dbgmsg "tyring lock 6\n"
                val () = if p = p' then
                           takeLock thiefLock (* XTRA *)
                         else
                           takeLock thiefLock
                val () = dbgmsg "got lock 6\n"
              in
                fn _ => if p = p' then
                          releaseLock thiefLock (* XTRA *)
                        else
                          releaseLock thiefLock
              end
            else
                die "Tried to resume work from unowned queue"

        val Queue { top, bottom, work, ... } = case !r of SOME q => q | NONE => (dbgmsg "remove2\n"; raise WorkQueue)
        val last = !top - 1
        val j = !bottom

        fun unloop i =
            if i = last
            then (top := last;
                  true)
            else (A.update (!work, i, A.sub (!work, i + 1));
                  unloop (i + 1))

        fun loop i =
            if i < j then false
            else
              case A.sub (!work, i)
               of Empty => raise WorkQueue
                | Marker => loop (i - 1)
                | Work w =>
                  let
                    val Token r' = #1 w
                  in
                    if r = r' then (A.update (!work, i, Empty);
                                    unloop i)
                    else loop (i - 1)
                  end

        val found = loop last
      in
        found
        before unsync ()
      end

  fun shouldYield _ = false

  fun reportSuccessfulSteals () = !successfulSteals
  fun reportFailedSteals () = !failedSteals
  fun resetSteals () = (successfulSteals := 0;
                        failedSteals := 0)

  (* Unused function *)
  val startWork : proc -> unit = fn _ => die "startWork called"
  val finishWork : proc -> unit = fn _ => die "finishWork called"
  val suspendWork : proc -> susp = fn _ => die "suspendWork called"
  val resumeWork : proc * susp * (token * work) -> unit =
   fn _ => die "resumeWork called"
end

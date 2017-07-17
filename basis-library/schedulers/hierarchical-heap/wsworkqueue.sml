functor WorkStealing (structure W :
                                sig
                                  type work
                                  val numberOfProcessors: unit -> int
                                end) =
struct

  type proc = int
  type work = W.work
  type unlocker = unit -> unit

  val dbgmsg: (unit -> string) -> unit =
      if false
      then fn m => MLtonParallelInternal.dbgmsg ("wsworkqueue: " ^ (m ()))
      else fn _ => ()

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
  and token = Token of int * int
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

  val queues = A.tabulate (numberOfProcessors, fn i => newQueue i)
  val () = A.appi (fn (p, q) =>
                      let
                          val Queue {work = ref q, ...} = q
                      in
                          MLton.HM.registerQueue (Word32.fromInt p, q)
                      end)
                  queues

  val tokens = A.tabulate (numberOfProcessors, fn _ => 0)
  fun newToken p =
      let
          val t = A.sub (tokens, p)
      in
          Token (p, t) before A.update (tokens, p, t + 1)
      end

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
            val () = dbgmsg (fn () => "copying!")
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
            val () = dbgmsg (fn () => "resizing!")
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

  fun addWork (p, w) =
    let
      val Lock { thiefLock, ... } = A.sub (locks, p)
      val () = takeLock thiefLock; (* XTRA *)
      val q as Queue { top, work, ... } = A.sub (queues, p)
      fun add w =
          let
            val i = !top
            val () = if i = A.length (!work) then resize p q else ()
            val i = !top (* in case of resize *)
            val t = newToken p
          in
            A.update (!work, i, Work (t, w));
            top := i + 1;
            t
          end
    in
      add w before releaseLock thiefLock (* XTRA *)
    end

  local
    fun victim p = Word.toIntX (MLton.Random.rand ()) mod numberOfProcessors
  in
  fun getWork p =
    let
      val Lock { thiefLock, ... } = A.sub (locks, p)
      val () = takeLock thiefLock (* XTRA *)

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
                  val () = takeLock thiefLock
                in
                  fn () => (releaseLock thiefLock)
                end
          in
            case A.sub (queues, p')
             of Queue { bottom, top, work, index, ... } =>
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
                          (incr successfulSteals;
                           SOME (true, unsync, #2 tw))
                    end
                  else (* empty queue *)
                    (incr failedSteals;
                     unsync (); yield (); NONE)

                end
          end
    in
      case A.sub (queues, p)
           of Queue { top, bottom, work, ... } =>
              let
                val i = !top
                val j = !bottom
              in
                if i = j then
                  (if i <> 0 then (top := 0; bottom := 0) else (); (* PERF unconditional? *)
                   (* release lock in steal *)
                   steal ())
                else (top := i - 1;
                      (case A.sub (!work, i - 1)
                        of Empty => raise WorkQueue
                         | Marker => (A.update (!work, i - 1, Empty);
                                      releaseLock thiefLock;
                                      case getWork p
                                      of NONE => NONE
                                       | SOME (_, unlocker, w) => SOME (true, unlocker, w))
                         | Work tw => SOME (false, fn () => releaseLock thiefLock, #2 tw))
                          before (A.update (!work, i - 1, Empty)))
              end
    end
  end

  fun removeWork (p', Token t) =
      let
        val Queue {top, bottom, index, work, ...} = A.sub (queues, p')
        val p = !index
        val unsync =
            if p < numberOfProcessors then
              let
                val Lock { thiefLock, ... } = A.sub (locks, p)
                val () = if p = p' then
                           takeLock thiefLock (* XTRA *)
                         else
                           takeLock thiefLock
              in
                fn _ => if p = p' then
                          releaseLock thiefLock (* XTRA *)
                        else
                          releaseLock thiefLock
              end
            else
                die "Tried to resume work from unowned queue"

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
                    val Token t' = #1 w
                  in
                    if t = t' then (A.update (!work, i, Empty);
                                    unloop i)
                    else loop (i - 1)
                  end

        val found = loop last
      in
        found
        before unsync ()
      end

  fun reportSuccessfulSteals () = !successfulSteals
  fun reportFailedSteals () = !failedSteals
  fun resetSteals () = (successfulSteals := 0;
                        failedSteals := 0)
end

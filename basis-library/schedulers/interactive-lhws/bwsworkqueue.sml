signature ARGS =
sig

    val QUEUE_ARRAY_SIZE : int

    val blocked : int Array.array
    val activeQueue : int Array.array
    val readyQueues : int list Array.array
    val emptyQueues : int list Array.array
end

functor BWorkStealing (structure W : sig type work val numberOfProcessors : unit -> int end
                       structure Args : ARGS) =
struct

  open Args

  type proc = int
  type work = W.work

  val successfulSteals = ref 0
  val failedSteals = ref 0
  fun incr r = r := !r + 1

  val yield = _import "Parallel_yield" runtime private: unit -> unit;

  val takeLock = _import "Parallel_lockTake" runtime private: int ref -> unit;
  val releaseLock = _import "Parallel_lockRelease" runtime private: int ref -> unit;

  local
    val takeDekker = _import "Parallel_dekkerTake" runtime private: bool * bool ref * bool ref * bool ref -> unit;
    val releaseDekker = _import "Parallel_dekkerRelease" runtime private: bool * bool ref * bool ref * bool ref -> unit;
  in
  type dekker = bool ref * bool ref * bool ref
  fun dekkerInit () = (ref false, ref false, ref true)
  fun dekkerLock (amLeft, (left, right, leftsTurn)) = takeDekker (amLeft, left, right, leftsTurn)
(*
      let
        val (mine, other) = if amLeft
                            then (left, right)
                            else (right, left)
      in
        mine := true;
        while !other
        do if amLeft <> !leftsTurn then
             let in
               mine := false;
               while amLeft <> !leftsTurn do yield ();
               mine := true
             end
           else ()
      end
*)
  fun dekkerUnlock (amLeft, (left, right, leftsTurn)) = releaseDekker (amLeft, left, right, leftsTurn)
(*
      let in
        leftsTurn := (not amLeft);
        (if amLeft then left else right) := false
      end
*)
  end

(*
  local
    exception Impossible
    open TextIO
  in
  fun die n = (output (stdErr,
                       "WSWorkQueue: die at " ^ (Int.toString n) ^ "\n");
               flushOut stdErr;
               (* XX releaseLock (); *)
               raise Impossible)
  end
*)
  exception WorkQueue
  exception QueueSize

  structure A = Array
  structure V = Vector
  val numberOfProcessors = W.numberOfProcessors ()

  val WORK_ARRAY_SIZE = 1024
  (* private state *)
  datatype entry = Empty | Work of (token * work (* * int *)) | Marker
  and queue = Queue of {
                              top : int ref,
                              bottom : int ref,
                              index : int ref,
                              work : entry (* (token * W.work (* * int *) ) option *) A.array ref
                            }
  and token = Token of queue option ref
  type susp = queue option

  fun newQueue index =
      Queue {
              top = ref 0,
              bottom = ref 0,
              index = ref index,
              work = ref (A.array (WORK_ARRAY_SIZE, Empty))
            }

  datatype lock = Lock of {
                            thiefLock : int ref,
                            ownerLock : dekker
                          }
  (* protects the array of queues -- specifically those queues not owned by
    any processor -- and the total number of active queues *)
  val masterLock = ref ~1
  val activeQueues = ref numberOfProcessors
  val totalQueues = ref numberOfProcessors

  val singleLock = Lock { thiefLock = masterLock, ownerLock = dekkerInit () }

  (* protects access to those queues owned by processors *)
  val locks = A.tabulate (numberOfProcessors,
                          (* fn _ => singleLock) *)
                          fn _ => Lock { thiefLock = ref ~1, ownerLock = dekkerInit () })
(*  val suspending = A.array (numberOfProcessors,
                            false) *)

  val queues = A.tabulate (QUEUE_ARRAY_SIZE,
                           fn i => if i < numberOfProcessors then
                                     SOME (newQueue i)
                                   else NONE)

(*
  local
    val r = ref 100
  in
  fun next () = !r before r := !r + 1
  end
*)

(*
  local
    structure I = Primitive.Int32
    open I
    val precision': Int.int = Primitive.Int32.zextdToInt sizeInBits
    val maxNumDigits = Int.+ (precision', 1)
    val fromInt = I.schckFromInt
    val toInt = I.schckToInt

    val bufs = A.array (W.numberOfProcessors (), CharArray.array (maxNumDigits, #"\000"))
  in
  fun intToString p n =
      let
        val buf = A.sub (bufs, p)
        val radix = fromInt (StringCvt.radixToInt StringCvt.DEC)
        fun loop (q, i: Int.int) =
            let
              val _ =
                  CharArray.update
                      (buf, i, StringCvt.digitToChar (toInt (~? (rem (q, radix)))))
              val q = quot (q, radix)
            in
              if q = zero
              then
                let
                  val start =
                      if n < zero
                      then
                        let
                          val i = Int.- (i, 1)
                          val () = CharArray.update (buf, i, #"~")
                        in
                          i
                        end
                      else i
                in
                  CharArraySlice.vector
                      (CharArraySlice.slice (buf, start, NONE))
                end
              else loop (q, Int.- (i, 1))
            end
      in
        loop (if n < zero then n else ~? n, Int.- (maxNumDigits, 1))
      end
  end
*)

  (* XX more compositional way of writing these assertions *)
  val lastCount = ref 0
  fun count p s delta = ()
(*
      let
        val s = Int.toString p ^ " " ^ s ^ " FAIL: "
        val error = ref false
        val b = !totalQueues
        fun outer (p, x) : int =
            if p = QUEUE_ARRAY_SIZE then x
            else if p >= b then
              case A.sub (queues, p)
               of NONE => outer (p + 1, x)
                | SOME _ => (print (concat [s, "found SOME queue at ",
                                            Int.toString p, " with total = ",
                                            Int.toString b, "\n"]);
                             error := true;
                             outer (p + 1, x))
            else
              case A.sub (queues, p)
               of NONE => outer (p + 1, x)
                | SOME (Queue { top, bottom, work, index, ... }) =>
                  let
                    val () = if !index <> p then
                               (print (concat [s, "index (", Int.toString (!index),
                                               ") does not match location (",
                                               Int.toString p, ")\n"]);
                                error := true)
                             else ()
                    val i = !top
                    val j = !bottom
                    val work = !work
                    fun inner (k, x) : int =
                        if k = WORK_ARRAY_SIZE then outer (p + 1, x)
                        else if k < j orelse k >= i then
                          case A.sub (work, k)
                           of NONE => inner (k + 1, x)
(*
                            | Marker => (print (concat [s, "found marker at ",
                                                        Int.toString k, " with bottom = ",
                                                        Int.toString j, " and top = ",
                                                        Int.toString i, "\n"]);
                                         error := true;
                                         inner (k + 1, x))
*)
                            | SOME _ => (print (concat [s, "found work at ",
                                                        Int.toString k, " with bottom = ",
                                                        Int.toString j, " and top = ",
                                                        Int.toString i, "\n"]);
                                         error := true;
                                         inner (k + 1, x))
                        else case A.sub (work, k)
                              of NONE => (print (concat [s, "found empty at ",
                                                          Int.toString k, " with bottom = ",
                                                          Int.toString j, " and top = ",
                                                          Int.toString i, "\n"]);
                                           error := true;
                                           inner (k + 1, x))
(*
                               | Marker => inner (k + 1, x)
*)
                               | SOME _ => inner (k + 1, x + 1)
                  in
                    inner (0, x)
                  end
        val newCount = outer (0, 0)
        val () = if !lastCount + delta = newCount
                 then lastCount := newCount
                 else (print (concat [s, "new count (", Int.toString newCount,
                                      ") does not match last count (", Int.toString (!lastCount),
                                      ") plus delta (", Int.toString delta, ")\n"]);
                       error := true)
      in
        if !error then raise WorkQueue
        else error := false
      end
*)

  fun pr p s = ()
(*
      let
        fun (* each ~1 = ()
          | *) each p' =
            case A.sub (queues, p') of
              SOME (Queue { top, bottom, work, ... }) =>
              let
(*
                fun loop i = if i < !bottom then ""
                             else (intToString p ((#3 (valOf (A.sub (!work, i)))))
                                   ^ " "
                                   ^ (loop (i - 1)))
*)
              in
                concat [" ", intToString p p',
                        " size = ", intToString p (!top - !bottom),
                        " active-queues = ", intToString p (!activeQueues),
                        " total-queues = ", intToString p (!totalQueues),
(*
                        "; queue = ",
                        (loop (!top - 1)),
*)
                        " "]
                (* each (p' - 1) *)
              end
            | NONE => (* each (p' - 1) *) ""
      in
        (* each (numberOfProcessors - 1); *)
        print (intToString p p ^ " " ^ s ^ each p ^ "\n");
        TextIO.flushOut TextIO.stdOut
      end
*)

  fun newWork p = Token (ref NONE)

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

  fun addWork (_, p, tws) =
    let
      val qi = A.sub (activeQueue, p)
                     handle Subscript => (print "331\n"; raise Subscript)
      (* val () = pr p "before-add" *)
      val Lock { ownerLock = lock, thiefLock, ... } = A.sub (locks, p)
      val () = takeLock thiefLock; (* XTRA *)
      (* val () = dekkerLock (true, lock); *)

      val q as Queue { top, work, ... } = case A.sub (queues, qi)
                                                     handle Subscript =>
                                                            (print "339\n";
                                                             raise Subscript)
                                      of SOME q => q | NONE => (print "add\n"; raise WorkQueue)
      fun add (tw as (t as Token r, w)) =
          let
            val i = !top
            val () = if i = A.length (!work) then resize qi q else ()
            val i = !top (* in case of resize *)
          in
            r := SOME q;
            A.update (!work, i, Work (* tw *) (t, w (*, next () *)));
            top := i + 1
          end
    in
      app add tws;
      pr p "add:";
      count p "add" (length tws);
      (* dekkerUnlock (true, lock); *)
      releaseLock thiefLock (* XTRA *)
    end

(* move queue at a into p', assumes the master lock is held *)
(*
  fun moveQueue (a, p') =
      if a = p' then ()
      else
        let
          val q as Queue { index, ... } =
              case A.sub (queues, a) of SOME q => q
                                      | NONE => (print "move\n"; raise WorkQueue)
        in
          index := p';
          A.update (queues, p', SOME q)
        end
*)

  local
    fun victim p =
        let
            val p' = Word.toIntX (MLton.Random.rand ()) mod numberOfProcessors
            val qs = A.sub (readyQueues, p')
                           handle Subscript => (print "377\n"; raise Subscript)
            val lqi = Word.toIntX (MLton.Random.rand ()) mod ((List.length qs) + 1)
        in
            (p',
            if lqi = 0 then A.sub (activeQueue, p')
                                  handle Subscript => (print "384\n"; raise Subscript)
            else
                List.nth (qs, lqi - 1)
                handle Subscript => (print "387\n"; raise Subscript)
            )
        end
  in
  fun getWorkA p allowedToSteal =
    let
      (* val () = pr p "before-get" *)
        val qi = A.sub (activeQueue, p)
                       handle Subscript => (print "393\n"; raise Subscript)
      val Lock { ownerLock = lock, thiefLock, ... } = A.sub (locks, p)
                                                            handle Subscript =>
                                                                   (print "396\n";
                                                                    raise Subscript)
      val () = takeLock thiefLock (* XTRA *)
      (* val () = dekkerLock (true, lock) *)

      fun newDeque () =
          let val nd =
                  case A.sub (emptyQueues, p)
                             handle Subscript => (print "404\n"; raise Subscript)
                   of
                      [] => (takeLock masterLock;
                             print "adding a new deque\n";
                             let val tq = !totalQueues in
                                 totalQueues := tq + 1;
                                 (if tq + 1 > QUEUE_ARRAY_SIZE then
                                      print "exceeded max number of deques\n"
                                  else ());
                                 A.update (queues, tq, SOME (newQueue tq));
                                 tq
                                 before releaseLock masterLock
                             end)
                    | qi::t =>
                      ((* print "switching to an empty deque\n"; *)
                       A.update (emptyQueues, p, t);
                       qi)
          in
              A.update (activeQueue, p, nd)
          end

      fun steal () =
          let
            (* Who to steal from? *)
            val (p', qi') = victim p
            val unsync =
                if p' < numberOfProcessors then
                  let
                    (* Release our own lock *)
                    (* val () = dekkerUnlock (true, lock) *)
                    val () = releaseLock thiefLock (* XTRA *)
                    (* Take the victim's lock *)
                    val Lock { thiefLock, ownerLock, ... } =
                        A.sub (locks, p')
                        handle Subscript => (print "432\n"; raise Subscript)
                    val () = takeLock thiefLock
                    (* val () = dekkerLock (false, ownerLock) *)
                  in
                    fn () => ((* dekkerUnlock (false, ownerLock); *)
                              releaseLock thiefLock)
                  end
                else
                  let
                    (* Here we hold on to our own lock, since we might modify
                      our own queue. *)
                    val () = takeLock masterLock
                  in
                    fn () => (releaseLock masterLock;
                              (* dekkerUnlock (true, lock); *)
                              releaseLock thiefLock (* XTRA *))
                  end
(*
            fun maybeCleanup () =
                if p' < numberOfProcessors then false (* can't remove owned queues *)
                else if p' >= !activeQueues then false (* can't remove inactive queues *)
                else
                  let
                    (* assumes we hold masterLock and there is nothing in p' *)
                    val a = !activeQueues - 1
                    val b = !totalQueues - 1
                  in
                    (* If this is the last queue then we need to clear it explicitly *)
                    A.update (queues, p', NONE);
                    moveQueue (a, p');
                    activeQueues := a;
                    moveQueue (b, a);
                    totalQueues := b;
                    true
                  end
*)
          in
            case A.sub (queues, qi')
                 handle Subscript => (print "470\n"; raise Subscript)
             of NONE => (incr failedSteals; unsync (); yield (); NONE)
              | SOME (q as Queue { bottom, top, work, index, ... }) =>
                let
                  val j = !bottom
                  val i = !top
                in
                  if i <> j then (* not empty *)
                      let (* Just steal the oldest task *)
                        val w = A.sub (!work, j)
                                handle Subscript => (print "480\n"; raise Subscript)
                        val () = A.update (!work, j, Empty)
                      in
                        (* if i = j + 1 andalso maybeCleanup () then ()
                        else (); *)
                        bottom := j + 1;
                        case w
                         of Empty => raise WorkQueue
                          | Marker => NONE
                          | Work tw =>
                            (pr p "work-steal:";
                             newDeque ();
                             incr successfulSteals;
                             SOME (true, #2 tw))
                      end
                        before (count p "work-steal" ~1;
                                unsync ())
                  else (* empty queue *)
                    ((* ignore (maybeCleanup ()); *)
                     count p "failed-steal" 0;
                     incr failedSteals;
                     unsync (); yield (); NONE)

                end
          end
      val qi = A.sub (activeQueue, p)
                     handle Subscript => (print "492\n"; raise Subscript)
    in
      case A.sub (queues, qi)
           handle Subscript => (print "509\n"; raise Subscript)
           of NONE => steal ()
            | SOME (Queue { top, bottom, work, ... }) =>
              let
                val i = !top
                val j = !bottom
              in
                if i = j then
                  (if i <> 0 then (top := 0; bottom := 0) else (); (* PERF unconditional? *)
                   (if A.sub (blocked, p) = 0
                       handle Subscript => (print "519\n"; raise Subscript)
                    then
                        let val ed = A.sub (emptyQueues, p)
                                     handle Subscript => (print "522\n"; raise Subscript)
                        in
                            A.update (emptyQueues, p, qi::ed)
                        end
                    else ());
                   (case A.sub (readyQueues, p)
                         handle Subscript => (print "528\n"; raise Subscript)
                     of
                        nqi::t => (A.update (readyQueues, p, t);
                                   A.update (activeQueue, p, nqi);
                                   (* print "switching to a ready deque\n"; *)
                                   releaseLock thiefLock;
                                   getWorkA p false)
                      | [] =>
                        (* release lock in steal *)
                        if allowedToSteal then steal ()
                        else
                            (releaseLock thiefLock;
                             NONE)
                  ))
                else (* Take the youngest from our queue *)
                  (top := i - 1;
                   pr p "get:";
                   case A.sub (!work, i - 1)
                        handle Subscript => (print "545\n"; raise Subscript)
                    of Empty => raise WorkQueue
                     | Marker => (A.update (!work, i - 1, Empty);
                                  releaseLock thiefLock;
                                  case getWorkA p allowedToSteal
                                   of NONE => NONE
                                    | SOME (_, w) => SOME (true, w))
                     | Work tw => SOME (false, #2 tw)
                                  before (A.update (!work, i - 1, Empty);
                                          count p "get" ~1;
                                          (* dekkerUnlock (true, lock); *)
                                          releaseLock thiefLock  ) (* XTRA *))
              end
    end
  end

  fun getWork p = getWorkA p true

  fun startWork p =
      let
          val qi = A.sub (activeQueue, p)
                   handle Subscript => (print "566\n"; raise Subscript)
        val Lock { ownerLock, thiefLock, ... } = A.sub (locks, p)
                                                 handle Subscript => (print "568\n"; raise Subscript)
        (* val () = dekkerLock (true, ownerLock) *)
        val () = takeLock thiefLock (* XTRA *)
      in
        (* Initialize a queue for this processor if none exists *)
        case A.sub (queues, qi)
             handle Subscript => (print "575\n"; raise Subscript)
         of SOME _ => ()
          | NONE => A.update (queues, qi, SOME (newQueue p));
        count p "start" 0;
        releaseLock thiefLock (* XTRA *)
        (* dekkerLock *)
      end

  fun finishWork p = ()

  fun suspendWork p = NONE
(*
      let
        (* val () = pr p "before-suspend" *)
        val Lock { ownerLock, thiefLock, ... } = A.sub (locks, p)
                                                 handle Subscript => (print "589\n"; raise Subscript)
        (* val () = dekkerLock (true, ownerLock) *)
        val () = takeLock thiefLock (* XTRA *)
      in

        (A.update (suspending, p, true);
         NONE)
        before ((* dekkerUnlock (true, ownerLock); *)
                releaseLock thiefLock) (* XTRA *)
      end
*)

  fun resumeWork (_, p, NONE, tw as (t as Token r, w)) =
      let
          (* val () = pr p "before-resume-local" *)
          val Lock { ownerLock = lock, thiefLock, ... } = A.sub (locks, p)
              handle Subscript => (print "603\n"; raise Subscript)
          val q as Queue { top, work, ... } = case A.sub (queues, p)
                                                   handle Subscript => (print "605\n"; raise Subscript)
                                               of SOME q => q | NONE => (print "resume-add\n"; raise WorkQueue)
          fun add w =
              let
                val i = !top
                val () = if i = A.length (!work) then resize p q else ()
                val i = !top (* in case of resize *)
              in
                A.update (!work, i, w);
                top := i + 1
              end
          val () = r := SOME q;
        in
          takeLock thiefLock; (* XTRA *)
          (* dekkerLock (true, lock); *)
          add Marker;
          add (Work tw);
          add Marker;
          pr p "local-resume:";
          (* dekkerUnlock (true, lock); *)
          releaseLock thiefLock (* XTRA *)
        end

    | resumeWork (_, p, q as SOME (Queue { work, top, index, ... }), tw as (t, w)) =
      let
        (* val () = pr p "before-resume-original" *)
        val () = takeLock masterLock
        val i = !top
        val a = !activeQueues
        val p' = !index
        val () = if i = WORK_ARRAY_SIZE then resize p (valOf q) else ()
        val i = !top (* in case of resize *)
      in
        A.update (!work, i, Work (t, w (*, next () *)));
        top := i + 1;
        (* Swap this queue in as the last active one *)
(*
        moveQueue (a, p');
        index := a;
        A.update (queues, a, q);
        activeQueues := a + 1;
*)
        pr p "resume:";
        count p "resume" 1;
        releaseLock masterLock
      end

(* what if a job J is added, then that queue is suspended, then resumed by a
  different processor.  then the token (the proc number at the time J was
  added) doesn't reflect which lock should be take nor which queue *)
  fun removeWork (p', Token r) =
      let
        (* val () = pr p' "before-remove" *)
        val Queue { index, ... } = case !r of SOME q => q | NONE => (print "remove\n"; raise WorkQueue)
(* this is a litte SUSP -- what if the queue is moved while we are trying to lock it? *)
        val p = !index
        val unsync =
            if p < numberOfProcessors then
              let
                val Lock { thiefLock, ownerLock, ... } = A.sub (locks, p)
                                                         handle Subscript => (print "665\n"; raise Subscript)
                val () = if p = p' then
                           (takeLock thiefLock; (* XTRA *)
                            (* dekkerLock (true, ownerLock) *) ())
                         else
                           (takeLock thiefLock;
                            (* dekkerLock (false, ownerLock) *) ())
              in
                fn _ => if p = p' then
                          ((* dekkerUnlock (true, ownerLock); *)
                           releaseLock thiefLock) (* XTRA *)
                        else
                          ((* dekkerUnlock (false, ownerLock); *)
                           releaseLock thiefLock)
              end
            else
              let
                val () = takeLock masterLock
              in
                fn _ => releaseLock masterLock
              end

        val Queue { top, bottom, work, ... } = case !r of SOME q => q | NONE => (print "remove2\n"; raise WorkQueue)
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
                   handle Subscript => (print "702\n"; raise Subscript)
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
        val () = pr p "remove:"
        val () = count p "remove" (if found then ~1 else 0)
      in
        found
        before unsync ()
      end

  fun removeWorkLat (_, p, t) = removeWork (p, t)

  fun shouldYield _ = false

  val policyName = "blocking"

  fun stringOfToken _ = ""

  fun reportSuccessfulSteals () = !successfulSteals
  fun reportFailedSteals () = !failedSteals
  fun resetSteals () = (successfulSteals := 0;
                        failedSteals := 0)
end

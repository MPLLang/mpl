signature POLICY =
sig
  val suspendEntireQueues : bool
  (* If we just suspended and there is work on our queue, should we take from
    the oldest end instead of the youngest end?  (This only makes sense if we
    are not suspendEntireQueues.) *)
  val stealOldestFromSelf : bool
  (* When resuming work, should it be added to our local queue? *)
  val resumeWorkLocally : bool
  (* When stealing should we consider queues whose top is suspended?  (This
    only matters if we suspendEntireQueues.) *)
  val stealFromSuspendedQueues : bool
  (* If a queue is not suspended and not running on any processor, can we just
   steal the whole thing? (This only matters if we suspendEntireQueues.) *)
  val stealEntireQueues : bool
  (* workonLatency n proc returns whether proc should run only latency-bound
     tasks when there are n processors *)
  val workOnLatency : int -> int -> bool

  val policyName : string
end

functor WorkStealingLC (structure W : sig type work val numberOfProcessors : unit -> int end
                      structure P : POLICY) =
struct

  infix 3 /
  fun x / y = Int.div (x, y)

  type proc = int
  type work = W.work

  val successfulSteals = ref 0
  val failedSteals = ref 0
  fun incr r = r := !r + 1

  val yield = _import "Parallel_yield" runtime private: unit -> unit;
  val pthread_yield = _import "pthread_yield" : unit -> unit;

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
  val realNumberOfProcessors = W.numberOfProcessors ()
  val numberOfProcessors = realNumberOfProcessors / 2

  val WORK_ARRAY_SIZE = 1024
  (* private state *)

  (* type age = Word *)

  type q = MLtonPointer.t

  datatype entry = Empty | Work of (token * W.work (* * int *)) | Marker
(*
  and q = Q of { age : Word32.word,
                 bot : int,
                 deq : entry A.array }
*)
  and queue = Queue of {
                              work : q ref,
                              lat : bool
                            }
  and token = Token of queue option ref
  type susp = queue option

(*
  val cas = _import "Parallel_cas" runtime private:
            Word32.word ref -> Word32.word -> Word32.word;
*)

  val getRef = _import "getRef" runtime private: MLtonPointer.t -> entry ref;

  val newABP = _import "ABP_newDeque" runtime private: unit -> q;

  val pushBottomC = _import "ABP_pushBottom" runtime private:
                    q * entry ref -> int;

  fun pushBottom (q: q) (e: entry) : bool =
      pushBottomC (q, ref e) = 0

  val popTopC = _import "ABP_popTop" runtime private: q -> MLtonPointer.t;

  fun popTop (q: q) : entry option =
      let val retval = popTopC q
      in
          case MLtonPointer.compare (retval, MLtonPointer.null) of
              EQ => NONE
            | _ => SOME (!(getRef retval))
      end

  val popBottomC = _import "ABP_popBottom" runtime private: q -> MLtonPointer.t;

  fun popBottom (q: q) : entry option =
      let val retval = popBottomC q
      in
          case MLtonPointer.compare (retval, MLtonPointer.null) of
              EQ => NONE
            | _ => SOME (!(getRef retval))
      end

  fun newQueue lat index =
      Queue {
              work = ref (newABP ()),
              lat = lat
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
  val suspending = A.array (numberOfProcessors,
                            false)
  val QUEUE_ARRAY_SIZE = 8192
  val cqueues = A.tabulate (QUEUE_ARRAY_SIZE,
                           fn i => if i < (numberOfProcessors) then
                                     SOME (newQueue false i)
                                   else NONE)

  val lqueues = A.tabulate (QUEUE_ARRAY_SIZE,
                           fn i => if i < (numberOfProcessors) then
                                     SOME (newQueue true i)
                                   else NONE)

(*
  local
    val r = ref 100
  in
  fun next () = !r before r := !r + 1
  end
*)

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
  fun resize p (Queue q) =
      raise WorkQueue
(*
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
*)

  fun addWork (lat, p, tws) =
    let
      (* val () = pr p "before-add" *)
      val p = p / 2

      val q as Queue { work, ... } =
          if lat then
              ((* print "adding latency task\n"; *)
               case A.sub (lqueues, p)
                of SOME q => q | NONE => (print "add\n"; raise WorkQueue))
          else
              case A.sub (cqueues, p)
               of SOME q => q | NONE => (print "add\n"; raise WorkQueue)
      fun add (tw as (t as Token r, w)) =
          (r := SOME q;
           if (pushBottom (!work) (Work (t, w)))
           then ()
           else (* exceeded queue size *) raise WorkQueue)
    in
      app add tws;
      pr p "add:";
      count p "add" (length tws)
    end

(* move queue at a into p', assumes the master lock is held *)
  fun moveQueue lat (a, p') =
      ()
(*
      if a = p' then ()
      else
        let
          val q as Queue { index, ... } =
              if lat then
                  case A.sub (lqueues, a) of
                      SOME q => q
                    | NONE => (print "move\n"; raise WorkQueue)
              else
                  case A.sub (cqueues, a) of
                      SOME q => q
                    | NONE => (print "move\n"; raise WorkQueue)
        in
          index := p';
          A.update (if lat then lqueues else cqueues, p', SOME q)
        end
*)

  local
    fun victim p =
        let
          (* XXX this is not greedy *)

          (* NB this is unprotected access to totalQueues and activeQueues.
            However, if one is being concurrently updated we will still given
            sequentializable behavior. *)
          val n = if P.stealFromSuspendedQueues then
                    !totalQueues
                  else
                    !activeQueues
        in
          Word.toIntX (MLtonRandom.rand ()) mod n
        end
  in
  fun getWork p =
    let
      (* val () = pr p "before-get" *)
      val lat = (p mod 2 = 0)
      val p = p / 2
      val queues = if lat then ((* print ("Got work on " ^ (Int.toString p) ^ "\n"); *) lqueues) else cqueues

      fun steal () =
          let
              val _ =
                (* If there's no work, yield to the throughput thread *)
                if lat andalso !failedSteals > numberOfProcessors then
                    (failedSteals := 0;
                     pthread_yield ())
                else ()
            (* Who to steal from? *)
            val p' = victim p
            fun maybeCleanup () =
                (if p' < numberOfProcessors then false (* can't remove owned queues *)
                else if p' >= !activeQueues then false (* can't remove inactive queues *)
                else
                  let
                    (* assumes we hold masterLock and there is nothing in p' *)
                    val a = !activeQueues - 1
                    val b = !totalQueues - 1
                  in
                    (* If this is the last queue then we need to clear it explicitly *)
                    A.update (queues, p', NONE);
                    moveQueue lat (a, p');
                    activeQueues := a;
                    moveQueue lat (b, a);
                    totalQueues := b;
                    true
                  end)
                    handle Overflow => (print "446\n";
                                        raise WorkQueue)
          in
            case A.sub (queues, p')
             of NONE => ((incr failedSteals; yield (); NONE)
                         handle Overflow => (print "449!\n";
                                             raise WorkQueue))
              | SOME (q as Queue { work,  ... }) =>
                (case popTop (!work) of
                     SOME w =>
                     (case w of
                          Empty => raise WorkQueue
                        | Marker => NONE
                        | Work tw =>
                          ((pr p "work-steal:";
                            incr successfulSteals;
                            SOME (true, #2 tw))
                           handle Overflow => (print "507\n";
                                               raise WorkQueue))
                     )
                     before (count p "work-steal" ~1)
                   | NONE => (* empty queue *)
                     ((ignore (maybeCleanup ());
                       count p "failed-steal" 0;
                       incr failedSteals;
                       yield (); NONE)
                      handle Overflow => (print "517!\n";
                                          raise WorkQueue)))
          end
    in
      case A.sub (queues, p)
           of NONE => steal ()
            | SOME (Queue { work, ... }) =>
              (case popBottom (!work) of
                   NONE => steal ()
                 | SOME Empty => raise WorkQueue
                 | SOME Marker => ((* XXX A.update (!work, i - 1, Empty); *)
                                   case getWork p
                                   of NONE => NONE
                                    | SOME (_, w) => SOME (true, w))
                 | SOME (Work tw) => SOME (false, #2 tw)
                                  (* XXX before (A.update (!work, i - 1, Empty);
                                          count p "get" ~1) *)
              )
    end
  end

  fun startWork p =
      let
        val p = p / 2
      in
        A.update (suspending, p, false);
        (* Initialize a queue for this processor if none exists *)
        (case A.sub (cqueues, p)
         of SOME _ => ()
          | NONE => A.update (cqueues, p, SOME (newQueue false p)));
        (case A.sub (lqueues, p)
         of SOME _ => ()
          | NONE => A.update (lqueues, p, SOME (newQueue true p)));
        count p "start" 0
      end

  fun finishWork p = ()

  fun suspendWork p =
      let
        (* val () = pr p "before-suspend" *)
        val p = p / 2
        val lat = P.workOnLatency numberOfProcessors p
        val queues = if lat then lqueues else cqueues
      in
        (if P.suspendEntireQueues then
             raise WorkQueue
(*
           let
             val () = takeLock masterLock
             val q as Queue { index, ... } = case A.sub (queues, p)
                                              of SOME q => q | NONE => (print "suspend\n"; raise WorkQueue)
             val a = !totalQueues
             val () = if a >= QUEUE_ARRAY_SIZE then (print "queues1\n"; raise QueueSize) else ()
           in
             index := a;
             A.update (queues, a, SOME q);
             totalQueues := a + 1;
             pr p "suspend:";
             (* XX this is because we currently add delayed tasks (in
              schedule) after a suspend *)
             A.update (queues, p, (* XXX PERF? NONE *) SOME (newQueue lat p));
             count p "suspend" 0;
             releaseLock masterLock;
             SOME q
           end
*)
         else
           let in
             A.update (suspending, p, true);
             NONE
           end)
      end

  fun resumeWork (lat, p, NONE, tw as (t as Token r, w)) =
      let
          (* val lat = P.workOnLatency numberOfProcessors p *)
          val p = p / 2
          val queues = if lat then lqueues else cqueues
      in
      if P.resumeWorkLocally then
        let
          (* val () = pr p "before-resume-local" *)
          val q as Queue { work, ... } = case A.sub (queues, p)
                                               of SOME q => q | NONE => (print "resume-add\n"; raise WorkQueue)
          fun add w =
              if pushBottom (!work) w then ()
              else raise WorkQueue
          val () = r := SOME q;
        in
          add Marker;
          add (Work tw);
          add Marker;
          pr p "local-resume:"
        end

      else (* make a new queue *)
        let
          (* val () = pr p "before-resume-non-local" *)
          val () = takeLock masterLock
          val a = !activeQueues
          val b = !totalQueues
          val () = if b >= QUEUE_ARRAY_SIZE then (print "queues2\n"; raise QueueSize) else ()
          val q as Queue { work, ... } = newQueue lat a
        in
          pushBottom (!work) (Work (t, w));
          totalQueues := b + 1;
          moveQueue lat (a, b);
          activeQueues := a + 1;
          A.update (queues, a, SOME q);
          pr p "non-local-resume:";
          count p "non-local-resume" 1;
          releaseLock masterLock
        end
      end
    | resumeWork (lat, p, q as SOME (Queue { work, ... }), tw as (t, w)) =
      (print "don't know how to do this\n";
       raise WorkQueue)
(*
      let
        val p = p / 2
        (* val () = pr p "before-resume-original" *)
        val () = takeLock masterLock
        val i = !top
        val a = !activeQueues
        val p' = !index
        val () = if i = WORK_ARRAY_SIZE then resize p (valOf q) else ()
        val i = !top (* in case of resize *)
        val queues = if lat then lqueues else cqueues
      in
        A.update (!work, i, Work (t, w (*, next () *)));
        top := i + 1;
        (* Swap this queue in as the last active one *)
        moveQueue lat (a, p');
        index := a;
        A.update (queues, a, q);
        activeQueues := a + 1;
        pr p "resume:";
        count p "resume" 1;
        releaseLock masterLock
      end
*)

(* what if a job J is added, then that queue is suspended, then resumed by a
  different processor.  then the token (the proc number at the time J was
  added) doesn't reflect which lock should be take nor which queue *)
  fun removeWork (p, Token r) =
      (* XXX this isn't general *)
      let val lat = (p mod 2 = 0)
          val p = p / 2
          val queues = if lat then ((* print ("Got work on " ^ (Int.toString p) ^ "\n"); *) lqueues) else cqueues
      in
      case A.sub (queues, p)
           of NONE => false
            | SOME (Queue { work, ... }) =>
              (case popBottom (!work) of
                   NONE => false
                 | SOME Empty => false
                 | SOME Marker => false
                 | SOME (Work (t, w)) =>
                   if t = Token r then true
                   else
                       (pushBottom (!work) (Work (t, w));
                        false)
              )
    end
(*
      let
        (* val () = pr p' "before-remove" *)
        val p' = p' / 2
        val Queue { index, ... } = case !r of SOME q => q | NONE => (print "remove\n"; raise WorkQueue)
(* this is a litte SUSP -- what if the queue is moved while we are trying to lock it? *)
        val p = !index
        val unsync =
            if p < numberOfProcessors then
              let
                val Lock { thiefLock, ownerLock, ... } = A.sub (locks, p)
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
*)

  fun shouldYield _ = false

  val policyName = P.policyName

  fun reportSuccessfulSteals () = !successfulSteals
  fun reportFailedSteals () = !failedSteals
  fun resetSteals () = (successfulSteals := 0;
                        failedSteals := 0)
end

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

functor WorkStealingLC (structure W : sig type work val numberOfProcessors : unit -> int end) =
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

  val takeLock = (* fn _ => () *)
    _import "Parallel_lockTake" runtime private: int ref -> unit;
  val releaseLock = (* fn _ => () *)
    _import "Parallel_lockRelease" runtime private: int ref -> unit;

  exception WorkQueue
  exception QueueSize

  structure A = Array
  structure V = Vector
  val realNumberOfProcessors = W.numberOfProcessors ()
  val numberOfProcessors = realNumberOfProcessors / 2

  (* private state *)

  type age = Word32.word

  val maxsize = 65536
  fun tag age = Word32.toInt (Word32.>> (age, 0w16))
  fun top age = Word32.toInt (Word32.>> (Word32.<< (age, 0w16), 0w16))
  fun packAge (tag, top) =
      Word32.+ (Word32.<< (Word32.fromInt tag, 0w16), Word32.fromInt top)

  type token = int
  datatype entry = Empty | Work of (token * W.work (* * int *)) | Marker
  type q = { age : age ref,
             bot : int ref,
             deq : entry A.array,
             lock : int ref,
             onBot : bool ref }
  type queue = { work : q ref,
                 lat : bool
               }

  type susp = queue option

  val cas = _import "Parallel_compareAndSwap":
             Word32.word ref * Word32.word * Word32.word -> bool;

  val stringOfToken = Int.toString

  fun pushBottom ({age, bot, deq, lock, onBot}: q) (e: entry) : unit =
      let (* val _ = takeLock lock *)
          val _ = if !onBot then print "already in\n" else ()
          val _ = onBot := true
          val localBot = !bot
      in
          if localBot >= maxsize then (print "exceeded size\n"; raise WorkQueue)
          else
              (A.update (deq, localBot, e);
               bot := localBot + 1;
               onBot := false (*;
               releaseLock lock *))
      end

  fun popTop ({age, bot, deq, lock, onBot}: q) : entry option =
      let (* val _ = takeLock lock *)
          val oldAge = !age
          val localBot = !bot
      in
          if localBot <= top oldAge then NONE
          else
              let val node = A.sub (deq, top oldAge)
                  (* val _ = print ((Int.toString ((top oldAge) + 1)) ^ "\n") *)
                  val newAge = packAge (tag oldAge, (top oldAge) + 1)
                  (* val _ = print ("new top should be: " ^ (Int.toString (top newAge)) ^ "\n") *)
              in
                  if cas (age, oldAge, newAge) then
                      let val age' = !age
                          (* val _ = print ("new top: " ^ (Int.toString (top age')) ^ "\n") *)
                      in
                          SOME node
                      end
                  else
                      NONE
              end
      end
      (* before releaseLock lock *)

  fun popBottom ({age, bot, deq, lock, onBot}: q) : entry option =
      let (* val _ = takeLock lock *)
          val _ = if !onBot then print "already\n" else ()
          val _ = onBot := true
          val localBot = !bot
      in
          if localBot = 0 then NONE else
          let val localBot = localBot - 1
              val _ = bot := localBot
              val node = A.sub (deq, localBot)
              val oldAge = !age
          in
              if localBot > top oldAge then
                  SOME node
              else
                  (bot := 0;
                   let val newAge = packAge ((tag oldAge) + 1, 0)
                   in
                       if (localBot = top oldAge) andalso
                          (cas (age, oldAge, newAge))
                       then
                           SOME node
                       else
                           (age := newAge;
                            NONE)
                   end)
          end
      end
      before (onBot := false (*; releaseLock lock *))

  fun newQueue lat _ =
      { work = ref {age = ref 0w0, bot = ref 0,
                    deq = A.array (maxsize, Empty),
                    lock = ref ~1, onBot = ref false},
        lat = lat
      }

  (* protects the array of queues -- specifically those queues not owned by
    any processor -- and the total number of active queues *)
  val masterLock = ref ~1
  val activeQueues = ref numberOfProcessors
  val totalQueues = ref numberOfProcessors

  val suspending = A.array (numberOfProcessors,
                            false)

  val cqueues = A.tabulate (numberOfProcessors, newQueue false)

  val lqueues = A.tabulate (numberOfProcessors, newQueue true)

  val nexttoken = A.tabulate (realNumberOfProcessors, fn _ => 0)

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

  fun newWork p =
      let val t = A.sub (nexttoken, p)
      in
          A.update (nexttoken, p, t + 1);
          t * realNumberOfProcessors + p
      end

  (* must already hold the lock! *)
  fun resize p q =
      (print "resize not supported\n";
       raise WorkQueue)
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

      val q as { work, ... } =
          if lat then A.sub (lqueues, p)
          else A.sub (cqueues, p)

      fun add tw = pushBottom (!work) (Work tw)
    in
      app add tws;
      pr p "add:";
      count p "add" (length tws)
    end

  local
    fun victim p =
        Word.toIntX (MLton.Random.rand ()) mod numberOfProcessors
  in
  fun getWork p =
    let
      (* val () = pr p "before-get" *)
      val lat = (p mod 2 = 1)
      (* val _ = if not lat then print "getWork\n" else () *)
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
            val (q as {work, ...}) = A.sub (queues, p')
          in
            case popTop (!work) of
                SOME w =>
                (case w of
                     Empty => (print "steal\n"; raise WorkQueue)
                   | Marker => NONE
                   | Work (tw as (t, w)) =>
                     ((pr p "work-steal:";
(*
                       (* (if not lat then *)
                            print ("processor " ^ (Int.toString p) ^ " stole " ^ (Int.toString t) ^ "\n") (* else ()) *);
*)
                       incr successfulSteals;
                       SOME (true, #2 tw))
                      handle Overflow => (print "507\n";
                                          raise WorkQueue))
                )
                before (count p "work-steal" ~1)
              | NONE => (* empty queue *)
                ((count p "failed-steal" 0;
                  incr failedSteals;
                  yield (); NONE)
                 handle Overflow => (print "517!\n";
                                     raise WorkQueue))
          end
      val { work, ...} = A.sub (queues, p)
    in
        if lat then (yield (); NONE) else
      case popBottom (!work) of
          NONE => steal ()
        | SOME Empty => (print "getWork\n"; raise WorkQueue)
        | SOME Marker => ((* XXX A.update (!work, i - 1, Empty); *)
            case getWork p
             of NONE => NONE
              | SOME (_, w) => SOME (true, w))
        | SOME (Work tw) => SOME (false, #2 tw)
      (* XXX before (A.update (!work, i - 1, Empty);
                                          count p "get" ~1) *)
    end
  end

  fun startWork p =
      let
        val p = p / 2
      in
        A.update (suspending, p, false);
        count p "start" 0
      end

  fun finishWork p = ()

  fun suspendWork p =
      let
        (* val () = pr p "before-suspend" *)
        val p = p / 2
      in
          A.update (suspending, p, true);
          NONE
      end

  fun resumeWork (lat, p, NONE, tw as (t, w)) =
      let
          (* val lat = P.workOnLatency numberOfProcessors p *)
          val p = p / 2
          val queues = if lat then lqueues else cqueues
          (* val () = pr p "before-resume-local" *)
          val q as { work, ... } = A.sub (queues, p)
          fun add w =
              pushBottom (!work) w
        in
          (* add Marker; XXX *)
          add (Work tw);
          (* add Marker; XXX *)
          pr p "local-resume:"
        end
    | resumeWork _ =
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
  fun removeWorkLat (lat, p, t') =
      (* XXX this isn't general *)
      let val p = p / 2
          val queues = if lat then ((* print ("Got work on " ^ (Int.toString p) ^ "\n"); *) lqueues) else cqueues
          val { work, ... } = A.sub (queues, p)
      in
          case popBottom (!work) of
              NONE => false
            | SOME Empty => false
            | SOME Marker => false
            | SOME (Work (t, w)) =>
              if t = t' then true
              else
                  (pushBottom (!work) (Work (t, w));
                   false)
      end

  fun removeWork (p, t) = removeWorkLat (false, p, t)

  val policyName = "ws6policy-tslc"

  fun shouldYield _ = false

  fun reportSuccessfulSteals () = !successfulSteals
  fun reportFailedSteals () = !failedSteals
  fun resetSteals () = (successfulSteals := 0;
                        failedSteals := 0)
end

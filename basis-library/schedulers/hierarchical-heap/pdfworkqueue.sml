functor WorkQueue (W : sig type work val numberOfProcessors : unit -> int end) : PARALLEL_WORKQUEUE =
struct

  type proc = int
  type work = W.work

  val takeLock = _import "Parallel_lockTake" runtime private: int ref -> unit;
  val releaseLock = _import "Parallel_lockRelease" runtime private: int ref -> unit;

  fun takeLock _ = ()
  fun releaseLock _ = ()

  val compareAndSwap = _import "Parallel_compareAndSwap" runtime private: int ref * int * int -> bool;

  structure A = Array

  (* Must be at least 2! *)
  val NUMBER_CHILDREN = 6

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
(*
  XXX PERF
  better to have a single lock per node (and use it to protect owner?)
*)
  (*
    "next" tells us the next free entry in the children array
    next is also the lock -- if it is negative, then the lock is held
      (all reads/writes from/to next must be done atomically)

   updates to owner:
     ~1 --> X    by CAS
     X --> ~1    only by owner

   updates to next:
     by CAS
       only the owner will ever increase next

   updates to children[i] for i < next:
     Empty --> Node or Element        only by owner
     Node --> Empty                   only by owner of sub node
     Element --> Node or Empty        only by lock-holder

   reads / updates to children[i] for i >= next:
     only by owner

   reads / updates parentNode and parentOffset
     only by owner (and only changed when a node is a "spare")

   tokens


   *)
  datatype 'a queue = Node of {
                               (* id : int, (* check *) *)
                                owner : int ref,
                                children : 'a queue A.array,
                                next : int ref, (* next free cell, also the lock *)
                                parentNode : 'a queue ref, (* upwards pointer *)
                                parentOffset : int ref (* entry in parent.children that points to us *)
                              }
                    | Element of 'a token * 'a
                    | Empty

  and 'a token = Token of ('a queue * int) ref

  type token = work token

  local
    val yield = _import "Parallel_yield" runtime private: unit -> unit;
  in
  (* Waits until the lock is released *)
  fun readNext next =
      let
        fun loop () =
            let
              val n = !next
            in
              if n >= 0
              then n
              else
                let in
                  yield ();
                  loop ()
                end
            end
      in
        loop ()
      end

  fun writeNext (next, i) =
      while let
              val n = !next
            in
              if n < 0 then (yield (); true)
              else not (compareAndSwap (next, n, i))
            end do ()
  end

  (* Global state *)
  val lock = ref ~1
  val head = Node {
                   (* id = ~1, *)
                    owner = ref 0,
                    children = A.array (NUMBER_CHILDREN, Empty),
                    next = ref 0,
                    parentNode = ref Empty,
                    parentOffset = ref 0
                  }

  (* Private state -- pointer to a node and an index of where new work should
    be inserted.  Each entry is read and written only by the corresponding
    processor. *)
  val runningStateNode = A.tabulate (W.numberOfProcessors (), fn 0 => head
                                                               | _ => Empty)
  val runningStateOffset = A.array (W.numberOfProcessors (), 0)
  val pendingStateNode = A.array (W.numberOfProcessors (), Empty)
  val pendingStateOffset = A.array (W.numberOfProcessors (), 0)

  fun toString p s q = ""
(*
      let
        fun loop prefix Empty = concat [prefix, "_\n"]
          | loop prefix (Element _) = concat [prefix, "E\n"]
          | loop prefix (q as Node { children, next, owner, id, parentOffset, ... }) =
            let
              val s = A.foldl (fn (q, s) => concat [s, loop (prefix ^ "  ") q])
                              ""
                              children
              (* XXX code dup *)
              fun isPending (p, Node { owner, ... }) =
                  (case A.sub (pendingStateNode, p)
                    of Node { owner = owner', ... } => owner = owner'
                     | _ => false)
                | isPending (p, _) = false
              val p' = !owner
            in
              concat ([prefix, "[", "id: ", intToString p id,
                      " owner: ",
                      intToString p p'] @
                      A.foldli (fn (p, Node { owner = owner', ...}, l)
                                   => if owner = owner' then ("^" ^ intToString p p)::l else l
                                 | (_, _, l) => l) nil runningStateNode @
                      (if p' >= 0 andalso isPending (p', q) then ["*"] else []) @
                     [" next: ",
                      intToString p (!next),
                      " parentOffset: ",
                      intToString p (!parentOffset),
                      "]\n",
                      s])
            end
      in
        concat [s,
                " [", intToString p p, "]",
                A.foldli (fn (p, Empty, s) =>
                             concat [s, " {p=", intToString p p,
                                     ", running=empty}"]
                           | (p, Node { id, ... }, s) =>
                             concat [s, " {p=", intToString p p,
                                     ", running=", intToString p id,
                                     ".", intToString p
                                            (A.sub (runningStateOffset, p)), "}"]
                           | _ => s ^ " ???")
                         " running:"
                         runningStateNode,
                A.foldli (fn (p, Empty, s) =>
                           concat [s, " {p=", intToString p p,
                                   ", pending=empty}"]
                           | (p, Node { id, ... }, s) =>
                             concat [s, " {p=", intToString p p,
                                     ", pending=", intToString p id,
                                     ".", intToString p
                                            (A.sub (pendingStateOffset, p)), "}"]
                           | _ => s ^ " ???")
                         " pending:"
                         pendingStateNode,
               "\n",
               loop "" q]
      end
*)
  local
    exception Impossible
    open TextIO
  in
  fun die p n = (output (stdErr,
                         "PDFWorkQueue: die at " ^ (intToString p n) ^ "\n");
                 flushOut stdErr;
                 output (stdErr, toString p "die" head);
                 flushOut stdErr;
                 raise Impossible)
  end

(* XXX does isPending need to take the node? *)
  fun isPending (p, Node { owner, ... }) =
      (case A.sub (pendingStateNode, p)
        of Node { owner = owner', ... } => owner = owner'
         | Empty => false
         | _ => die p 37)
    | isPending (p, _) = die p 36
  fun setPending (p, (q, i)) = (A.update (pendingStateNode, p, q);
                                A.update (pendingStateOffset, p, i))
  fun clearPending p = A.update (pendingStateNode, p, Empty)

  local
    val spare = A.array (W.numberOfProcessors (), NONE)
    val nextid = ref 0
    fun getid () = !nextid before nextid := !nextid + 1 (* XX compareAndSwap *)
  in
  fun newNode (p, parentNode, parentOffset) =
      case A.sub (spare, p)
       of NONE =>
          Node {
                 (* id = getid (), *)
                 owner = ref p,
                 children = A.array (NUMBER_CHILDREN, Empty),
                 next = ref 0,
                 parentNode = ref parentNode,
                 parentOffset = ref parentOffset
               }
        | SOME (q as Node { owner, next, children,
                            parentNode = node,
                            parentOffset = offset, ... }) =>
          let (* check *)
(*
            val _ = p = !owner orelse die p 40
            val _ = 0 = !next orelse die p 41
            val _ = (A.all (fn Empty => true | _ => false) children) orelse die p 42
            val _ = case parentNode of Node _ => () | _ => die p 44
*)
          in
            A.update (spare, p, NONE);
            node := parentNode;
            offset := parentOffset;
            q
          end
        | SOME _ => die p 39

(* XX DOC describe race between reader of q and p *)
  fun saveSpare (p, q as Node { parentNode, ... }) =
      let in
        parentNode := Empty;
        A.update (spare, p, SOME q)
      end
    | saveSpare (p, _) = die p 43
  end

  val PARANOID = false
  val CHECK_SIZE = false
  fun size () = 0
(*
      let
        fun outer (Node { children, next, ... }) =
            let
              fun inner i =
                  if i < 0 then 0
                  else
                    case A.sub (children, i)
                     of q as Node _ => (outer q) + (inner (i - 1))
                      | Empty => inner (i - 1)
                      | Element tw => 1 + (inner (i - 1))
            in
              inner (!next - 1)
            end
          | outer (Element _) = 1
          | outer Empty = 0
      in
        outer head
      end
*)
  val PRINT = false
  fun print (s: string) = (TextIO.output (TextIO.stdErr, s); TextIO.flushOut TextIO.stdErr)

  fun pr p s = print (toString p s head)
  fun pr' p s = print (toString p s head)

  fun check p s = ()
(*
      let
        fun checkOne (q as Node { children, next, parentNode,
                                  parentOffset, owner, ... }) recur =
            let
              val () = case !parentNode
                        of (q' as Node { children = parentChildren, next, ... }) =>
                           let
                             val next = readNext next
                             val parentOffset = !parentOffset
                             val () = if parentOffset >= next then
                                        (print (toString p ("parentOffset (" ^ intToString p parentOffset
                                                            ^ ") >= parent->next ("
                                                            ^ intToString p next ^ ") @ " ^ s) q');
                                         print (toString p "me: " q))
                                      else ()
                             val owner' = case A.sub (parentChildren, parentOffset)
                                           of Node { owner = owner', ... } => owner'
                                            | _ => ref ~1
                             val () = if owner <> owner' then
                                        (* Without a global lock it's possible
                                          that this node was concurrently
                                          pruned.  In that case the node
                                          should still be owned (by someone
                                          else) and have no children.  NB that
                                          this is not necessarily true if we
                                          reuse Nodes. *)
                                        if !owner >= 0 andalso !owner <> p
                                           andalso A.all (fn Empty => true | _ => false) children
                                           andalso case A.sub (parentChildren, parentOffset) of Empty => true | _ => false
                                        then
                                          ()
                                        else
                                          (pr p ("parentOffset is wrong @ " ^ s);
                                           print (toString p "parent's child:" (A.sub (parentChildren, parentOffset)));
                                           print (toString p "me:" q))
                                      else ()
                           in
                             ()
                           end
                         | _ => ()

              val ownerval = !owner
              val next = readNext next
              val () = if ownerval < 0 orelse ownerval = p then
                         A.appi (fn (_, Empty) => ()
                                  | (i, _) => if i >= next then
                                                print (toString p ("non-empty entry at >= next ("
                                                                   ^ intToString p next ^ ") @ " ^ s) q)
                                              else ())
                                children
                       else ()

              fun loop children i = if i < 0 then ()
                                    else ((case A.sub (children, i)
                                            of q as Node _ => checkOne q recur
                                             | Element (Token t, _) =>
                                               let in
                                                 case !t
                                                  of SOME (q' as Node { owner = owner', ... }, i') =>
                                                     let in
                                                       if owner <> owner' then
                                                         (print (toString p ("found a token with a different parent! at offset " ^ intToString p i ^ " @ " ^ s) q);
                                                          print (toString p "token node: " q'))
                                                       else ();
                                                       if i <> i' then
                                                         (print (toString p ("found a token with a different offset! node offset " ^ intToString p i
                                                                             ^ ", token offset " ^ intToString p i ^ " @ " ^ s) q);
                                                          print (toString p "token node: " q'))
                                                       else ()
                                                     end
                                                   | SOME _ => print (toString p ("found a token with a non-node at offset " ^ intToString p i ^ " @ " ^ s) q)
                                                   | NONE => print (toString p ("found a token with no pointer at offset " ^ intToString p i ^ " @ " ^ s) q)
                                               end
                                             | _ => ());
                                          loop children (i - 1))

            in
              if recur then
                loop children (NUMBER_CHILDREN - 1)
              else ()
            end
          | checkOne _ _ = ()

        val () = A.appi (fn (_, (Empty, 0)) => ()
                          | (p', (Node { owner, ... }, i)) =>
                            let in
                              if p' <> !owner then
                                pr p ("wrong owner in pending[" ^ intToString p p' ^ "] @ " ^ s)
                              else ()
                            end
                          | (p', _) => pr p ("non-node in pending[" ^ intToString p p' ^ "] @ " ^ s))
                 pendingState
      in
        checkOne head true;
        A.app (fn (q, i) => checkOne q false) runningState
      end
*)

  (* Returns the first non-Empty child starting from i. *)
  fun lastEmpty (children, i) =
      let
        fun loop i =
            if i > 0 then
              case A.sub (children, i - 1)
               of Empty => loop (i - 1)
                | _ => i
            else i
      in
        loop i
      end

  fun newWork _ = Token (ref (Empty, 0))

  fun checkNextOrDie (p, owner, children, n, d) =
      if (!owner <> p andalso !owner >= 0)
         orelse A.foldli (fn (_, Empty, b) => b
                           | (i, _, b) => i < n andalso b) true children
      then ()
      else (pr p ("invalid update to next (" ^ intToString p n ^ ")");
            die p d)

  fun addWork (p, tws) =
    let
      fun add ((q as Node { children, next, owner,
                            parentNode, parentOffset, ... }, i, n), tws as _::_) =
          if i = NUMBER_CHILDREN - 1 orelse i < n then
            let (* Need to expand the tree *)
              val () = if PARANOID then case A.sub (children, i) of Empty => () | _ => die p 11 else ()
              val () = if PARANOID andalso i = NUMBER_CHILDREN - 1 andalso n <> i then die p 10 else ()
              val q' = newNode (p, q, i)
              (* Add the link *)
              val () = A.update (children, i, q')
              (* No more elements will be added until someone else takes
                ownership, so we lose nothing by setting next to the maximum
                value.  This is required for correctness, since someone else
                who is removing an element from this node (either in prune or
                removeWork) might not see our update at children[i].  If they
                DO miss our update, however, they hold the lock, and the
                following write won't succeed until after it is released. *)
              val () = if PARANOID then checkNextOrDie (p, owner, children, NUMBER_CHILDREN, 50) else ()
              val () = writeNext (next, NUMBER_CHILDREN)
              (* Update pending status / ownership *)
              val i = case A.sub (pendingStateNode, p)
                       of Empty => (owner := ~1; 0)
                        | Node { owner = owner', ... } =>
                          if owner = owner' then
                            let
                              val i' = A.sub (pendingStateOffset, p)
                            in
                              if i = i' then (* We're overwriting the pending slot -- give that task a new slot *)
                                (A.update (pendingStateNode, p, q');
                                 A.update (pendingStateOffset, p, 0);
                                 1)
                              else (* Different slot but same node -- leave ownership alone *)
                                0
                            end
                          else (* Different node, we can drop ownership of the parent *)
                            (owner := ~1; 0)
                        | _ => die p 70
            in
              (* Try to add on the child *)
              add ((q', i, i), tws)
            end
          else (* There is room in the current node *)
            let
              val () = if PARANOID andalso i <> n then die p 12 else ()
              fun doOne (_, i, nil) = (i, nil)
                | doOne (0, i, tws) = (i, tws) (* XXX PERF could skip right to expand here *)
                | doOne (n, i, (tw as (Token t, _))::tws) =
                  let in
                    (* Update the token *)
                    if PARANOID andalso case !t of (Empty, 0) => false | _ => true then die p 63 else ();
                    t := (q, i);
                    if PARANOID then case A.sub (children, i) of Empty => () | _ => die p 6 else ();
                    A.update (children, i, Element tw);
                    doOne (n - 1, i + 1, tws)
                  end
              fun doMany () =
                  let
                    val (i, tws) = doOne ((NUMBER_CHILDREN - i) - 1, i, tws)
                  in
                    if PARANOID then checkNextOrDie (p, owner, children, i, 51) else ();
                    writeNext (next, i);
                    add ((q, i, i), tws)
                  end
              fun here () =
                  case A.sub (pendingStateNode, p)
                   of Empty => doMany ()
                    | Node { owner = owner', ... } =>
                      if owner = owner' then
                        let
                          val i' = A.sub (pendingStateOffset, p)
                        in
                          if i = i' then
                            (* Can't overwrite the pending slot *)
                            add ((q, i + 1, i + 1), tws)
                          else doMany ()
                        end
                      else doMany ()
                    | _ => die p 74
            in
              (* Check to see if we should collapse *)
              if i > 0 orelse isPending (p, q) then here ()
              else
                case !parentNode
                 of q' as Node { owner, next, children, ... } =>
                    let
                      (* val _ = not PARANOID orelse not (isAnyPending (p, q)) orelse die p 18 *)
                      val parentOffset = !parentOffset
                    in
                      if !owner = ~1 (* parent is unowned *)
                         (* the following will also fail if parent is locked *)
                         andalso parentOffset = !next - 1 (* current is youngest *)
                         andalso (parentOffset < NUMBER_CHILDREN - 1 (* there is room *)
                                  orelse case A.sub (children, parentOffset - 1)
                                          of Empty => true | _ => false)
                         (* we can take ownership *)
                         andalso compareAndSwap (owner, ~1, p)
                         (* andalso (not PARANOID orelse not (isAnyPending (p, q')) orelse die p 49) *)
                      then
                        let
                          val () = A.update (children, parentOffset, Empty)
                          (* read this here since saveSpare will clear it *)
                          val parentNode = !parentNode
                          val () = saveSpare (p, q)
                          (* make as much room as possible... once we are the owner,
                            no one else can change an Empty entry, so we are safe to
                            count Empty ones without the lock *)
                          val i = lastEmpty (children, NUMBER_CHILDREN)  (* XXX PERF could be NUM - 1 since i < NUM (?) *)
                          val () = if PARANOID then checkNextOrDie (p, owner, children, i, 52) else ()
                          val () = writeNext (next, i)
                        in
                          add ((parentNode, i, i), tws)
                        end
                      else here ()
                    end
                  | _ => here () (* we are at the root *)
            end
        | add ((q, i, _), nil) = (q, i)
        | add _ = die p 1

      val () = takeLock lock
      val () = if PRINT then pr p (concat ["before add: (", intToString p (length tws), ")"]) else ()
      val startSize = size ()

      val (q, next, i) = case (A.sub (runningStateNode, p), A.sub (runningStateOffset, p))
                          of (q as Node { next, ... },  i) => (q, next, i)
                           | _ => die p 23
      (* Check to see if work was removed and next updated by another proc *)
      val n = readNext next
      val i = if i > n then n else i
      (* Don't bother to update i now since it will change in add *)
      val (q, i) = add ((q, i, n), rev tws)

      val () = if PARANOID andalso case (A.sub (pendingStateNode, p),
                                         A.sub (pendingStateOffset, p))
                                    of (Empty, 0) => false
                                     | (Node { children, ... }, i) =>
                                       (case A.sub (children, i) of Empty => false
                                                                  | _ => true)
                                     | _ => true then die p 76 else ()

      val endSize = size ()
      val () = if CHECK_SIZE andalso
                  endSize <> startSize + (length tws) then
                 pr p "lost an element on add!!"
               else ()

    in
      (* Now update our locat state *)
      A.update (runningStateNode, p, q);
      A.update (runningStateOffset, p, i);

      check p "after add";
      if PRINT then pr p "after add:" else ();
      releaseLock lock
    end

  fun getWork p =
    let
      exception Found of work

      (* Look for a unowned queue, or an element *)
      fun get Empty = get head (* the node was overwritten => start again *)
        | get (Element _) = get head (* same thing here *)
        | get (q as Node { children, owner, next, ... }) =
          (* PERF move loop outside of owner check? *)
          let
            val p' = !owner
          in
            if p' >= 0 then
              let
                fun loop i =
                    if i < 0 then ()
                    else
                      case A.sub (children, i)
                       of q as Node _ => (get q; loop (i - 1))
                        | Empty => loop (i - 1)
                        | Element _ =>
                          (* if we are the owner and this is the youngest, try
                             a quick bump.  this will fail if locked *)
                          if p = p' andalso i + 1 = !next
                             (* This check against (i + 1) is because we know
                             there is an element at i (that will be deleted) *)
                             andalso ((not PARANOID orelse (checkNextOrDie (p, owner, children, i + 1, 60); true))
                                      andalso compareAndSwap (next, i + 1, i))
                          then
                            (* Need to re-read just in case the element was replaced with something else *)
                            case A.sub (children, i)
                             of q as Node _ => (writeNext (next, i + 1); get q; loop (i - 1))
                              | Empty => loop (i - 1)
                              | Element (Token t, w) =>
                                let
                                  (* val () = if PARANOID andalso isAnyPending (p, q) then die p 15 else () *)
                                  val () = if PARANOID andalso case (A.sub (pendingStateNode, p), A.sub (pendingStateOffset, p))
                                                                of (Empty, 0) => false | _ => true then die p 69 else ()

                                  val () = A.update (children, i, Empty)
                                  val () = if PARANOID andalso case !t of (Node { owner = owner', ... }, i') =>
                                                                          i <> i' orelse owner <> owner
                                                                        | _ => true then die p 64 else ()
                                  val () = t := (Empty, 0)
                                  (* Make some extra room if possible *)
                                  val i' = lastEmpty (children, i)
                                  val i = if i <> i'
                                             andalso ((not PARANOID orelse (checkNextOrDie (p, owner, children, i', 61); true))
                                                      andalso compareAndSwap (next, i, i'))
                                          then
                                            i'
                                          else
                                            i (* always safe to use i *)
                                in
                                  setPending (p, (q, i));
                                  raise Found w
                                end

                          else (* otherwise we must try to take the lock *)
                            let
                              val n = !next
                            in
                              if n = 0 then () (* someone concurrently removed the last job *)
                              else if n < 0 orelse not (compareAndSwap (next, n, ~n)) then
                                (* someone else has the lock *)
                                loop (i - 1) (* XXX loop i or loop (i - 1) or get q ? *)
                              else (* we have the lock: re-compare to n and re-read from children *)
                                if i < n then
                                  case A.sub (children, i)
                                   of q as Node _ => (if PARANOID then checkNextOrDie (p, owner, children, n, 53) else ();
                                                      next := n; get q; loop (i - 1))
                                    | Empty => (if PARANOID then checkNextOrDie (p, owner, children, n, 59) else ();
                                                next := n; loop (i - 1))
                                    | Element (Token t, w) =>
                                      (* We don't own this node or there is something
                                        on top of q[i], so expand with a new node below *)
                                      let
                                        val q' = newNode (p, q, i)
                                        val _ = not PARANOID orelse case (A.sub (pendingStateNode, p), A.sub (pendingStateOffset, p))
                                                                     of (Empty, 0) => true
                                                                      | _ => die p 48
                                        val () = setPending (p, (q', 0))
                                      in
                                        (* Update the token *)
                                        t := (Empty, 0);
                                        (* replace element with a node *)
                                        A.update (children, i, q');
                                        if PARANOID andalso i >= n then die p 9 else ();
                                        (* release the lock *)
                                        if PARANOID then checkNextOrDie (p, owner, children, n, 54) else ();
                                        next := n;
                                        raise Found w
                                      end
                                else
                                  let in
                                    if PARANOID then checkNextOrDie (p, owner, children, n, 62) else ();
                                    next := n;
                                    loop (n - 1)
                                  end
                            end
              in
                loop ((readNext next) - 1)
              end

            else (* not owned *)
              let
                fun release () = if not (isPending (p, q)) andalso !owner = p then owner := ~1 else ()
                fun loop i =
                    if i < 0 then () (* PERF XX consider collapsing here *)
                    else
                      case A.sub (children, i)
                       of q as Node _ => (get q; loop (i - 1))
                        | Empty => loop (i - 1)
                        | Element tw =>
                          let in
                            (* try to take ownership *)
                            ignore (compareAndSwap (owner, ~1, p)
                                    (* andalso not PARANOID orelse not (isAnyPending (p, q)) orelse die p 70 *) );
                            (* start again regardless of result *)
                            get q;
                            release ()
                          end
                            handle e as Found _ => (release (); raise e)
              in
                loop ((readNext next) - 1)
              end
          end

      val () = takeLock lock
      val () = if PRINT then pr p "before get:" else ()
      val startSize = size ()
      val s = if CHECK_SIZE then toString p "from before:" head else ""

      (* Always start from the head *)
      val w = (get head; NONE)
          handle Found w => SOME w

      val endSize = size ()
      val () = if CHECK_SIZE then
                 if isSome w then
                   if endSize <> startSize - 1  then
                     (print s;
                      pr' p "lost an element on successful get!!")
                   else ()
                 else
                   if endSize <> startSize then
                     (print s;
                      pr' p "lost an element on failed get!!")
                   else ()
               else ()
    in
      check p "after get";
      if PRINT then pr p "after get:" else ();
      releaseLock lock;
      w
    end

  (* You must be the owner to call prune!  This will either remove the given
    node from the tree or release ownership of the it. *)
  fun prune p (q' as Node { owner, next, children,
                            parentNode, parentOffset, ... }) =
      let
        val _ = not PARANOID orelse not (isPending (p, q')) orelse die p 14
        val _ = not PARANOID orelse p = !owner orelse die p 31
      in
        if !next = 0 then
          let in
            case !parentNode
             of q as Node { children, next, owner, ... } =>
                let
                  val parentOffset = !parentOffset
                  (* Safe to remove reference to current node since we own it *)
                  val () = A.update (children, parentOffset, Empty)
                  val () = saveSpare (p, q')

                  (* Update next in the parent returning the new value *)
                  fun loop () =
                      let
                        val n = !next
                      in
                        if n = 0 then 0
                        else if not (n > 0 andalso compareAndSwap (next, n, ~n)) then
                          (* wait for the lock *)
                          loop ()
                        else (* we now have the lock *)
                          let
                            val i = lastEmpty (children, n)
                          in
                            (* Unlock *)
                            if PARANOID then checkNextOrDie (p, owner, children, i, 55) else ();
                            next := i;
                            i
                          end
                      end
                in
                  if parentOffset + 1 = !next andalso loop () = 0
                     andalso !owner = ~1
                     andalso compareAndSwap (owner, ~1, p)
                     (* andalso (not PARANOID orelse not (isAnyPending (p, q)) orelse die p 71 *)
                  then prune p q
                  else () (* failed to take parent ownership or still has children *)
                end
              | _ => (if PARANOID andalso isPending (p, q') then die p 30 else ();
                      owner := ~1) (* don't delete the root node *)
          end
        else (if PARANOID andalso isPending (p, q') then die p 32 else ();
              owner := ~1) (* still has children *)
      end
    | prune p _ = die p 13

  fun startWork p =
      let
        val () = takeLock lock
        val () = if PRINT then pr p "before start:" else ()
        val startSize = size ()

        val _ = not PARANOID orelse case A.sub (runningStateNode, p) of Empty => true | _ => false orelse die p 67

        val () = case (A.sub (pendingStateNode, p), A.sub (pendingStateOffset, p))
                  of (q' as Node { owner, children, ... }, i) =>
                     let
                       val _ = not PARANOID orelse !owner = p orelse die p 2
                       val _ = not PARANOID orelse isPending (p, q') orelse die p 17
                       val () = clearPending p

                       val _ = not PARANOID orelse case A.sub (children, i) of Empty => true | _ => false orelse die p 68
                     in
                       (* PERF XX maybe collapse here instead of at add? *)
                       (* Update our state to point to the new one *)
                       A.update (runningStateNode, p, q');
                       A.update (runningStateOffset, p, i)
                     end
                   | _ => die p 3

        val () = if PRINT then pr p "after start:" else ()

        val endSize = size ()
        val () = if CHECK_SIZE andalso
                    endSize <> startSize then
                   pr' p "lost an element on start!!"
                 else ()

      in
        check p "after start";
        releaseLock lock
      end

  fun finishWork p =
      let
        val () = takeLock lock
        val () = if PRINT then pr p "before finish:" else ()
        val startSize = size ()

        val () =
            case A.sub (runningStateNode, p)
             of q as Node { owner, next, ... } =>
                let
                  val _ = not PARANOID orelse p = !owner orelse die p 35
                in
                  if not (isPending (p, q)) then prune p q
                  else () (* XX could check pending state *)
                end
              | _ => ignore (die p 8)
        val () = A.update (runningStateNode, p, Empty);

        val () = if PRINT then pr p "after finish:" else ()

        val endSize = size ()
        val () = if CHECK_SIZE andalso
                    endSize <> startSize then
                   pr p "lost an element on finish!!"
                 else ()
      in
        check p "after finish";
        releaseLock lock
      end

  (* XX PERF combine shouldYield with getWork? *)
  fun shouldYield p =
      let
        val () = takeLock lock
        val startSize = size ()

        exception Found of bool
        fun search children i =
            if i < 0 then ()
            else
              (searchOne (A.sub (children, i));
               search children (i - 1))

        and searchOne (Node { children, owner, next, ... }) =
            if !owner = p then raise Found false
            else search children ((readNext next) - 1)
          | searchOne (Element _) = raise Found true
          | searchOne Empty = ()

        val b = (searchOne head; false)
            handle Found b => b

        val endSize = size ()
        val () = if CHECK_SIZE andalso
                    endSize <> startSize then
                   pr' p "lost an element on shouldYield!!"
                 else ()
      in
        check p "after shouldYield";
        releaseLock lock;
        b
      end

  fun removeWork (p, Token t) =
      let
        val () = takeLock lock
        val () = if PRINT then pr p "before remove:" else ()
        val startSize = size ()

        val b =
            case !t
             of (Empty, _) => false
              | (q as Node { children, next, owner, ... }, i) =>
                let
                  fun loop () =
                      let
                        val n = !next
                      in
                        if i >= n then false (* it already finished *)
                        else if not (n > 0 andalso compareAndSwap (next, n, ~n)) then
                          (* wait for the lock *) (* XXX yield? *)
                          loop ()
                        else (* we now have the lock *)
                          case A.sub (children, i)
                           of Element (Token t', _) =>
                              if t = t' then
                                let
                                  (* PERF XX could lock here then re-read *)
                                  val () = A.update (children, i, Empty)
                                  val i = lastEmpty (children, n)
                                in
                                  (* Unlock *)
                                  if PARANOID then checkNextOrDie (p, owner, children, i, 56) else ();
                                  next := i;
                                  (* Prune if unowned *)
                                  if i = 0 andalso !owner = ~1
                                     andalso compareAndSwap (owner, ~1, p)
                                     (* andalso (not PARANOID orelse not (isAnyPending (p, q)) orelse die p 72) *)
                                  then
                                    prune p q
                                  else ();
                                  true
                                end
                              else (if PARANOID then checkNextOrDie (p, owner, children, n, 57) else ();
                                    next := n; false)
                            | _ => (if PARANOID then checkNextOrDie (p, owner, children, n, 58) else ();
                                    next := n; false)
                      end
                in
                  loop ()
                end
              | _ => die p 22

        val () = if PRINT then pr p "after remove:" else ()

        val endSize = size ()
        val () = if CHECK_SIZE then
                   if b then
                     if endSize <> startSize - 1  then
                       pr p "lost an element on successful remove!!"
                     else ()
                   else
                     if endSize <> startSize then
                       pr p "lost an element on failed remove!!"
                     else ()
                 else ()

      in
        check p "after remove";
        releaseLock lock;
        b
      end

  val policyName = "pdf"

end

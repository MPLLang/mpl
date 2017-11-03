functor BinaryHeapQueue
  (Elem : sig
            type t
            val default : t
            val depth : t -> int
          end)
  : QUEUE where type task = Elem.t =
struct

  structure P = Potential

  fun nth a i = Array.sub (a, i)
  fun upd a (i, x) = Array.update (a, i, x)

  type task = Elem.t

  type t =
    { back : Elem.t list ref
    , data : Elem.t array
    , len : int ref       (* size of relevant prefix of `data` *)
    , weight : P.t ref    (* total potential of all elements in the queue *)
    , maxd : int ref      (* largest depth seen so far (of any element that
                           * has ever been in the queue) *)
    }

  type task_set = Elem.t list

  val maxSize = 1024
  exception Full

  fun empty () =
    { data = Array.array (maxSize, Elem.default)
    , len = ref 0
    , weight = ref P.zero
    , maxd = ref 0
    , back = ref []
    }

  fun isEmpty ({len = ref 0, back = ref [], ...} : t) = true
    | isEmpty _ = false

  val numts = List.length

  fun size ({len, back, ...} : t) =
    !len + List.length (!back)

  (* fun weight ({weight, ...} : t) = weight
  fun capacity ({data, ...} : t) = Array.length data *)

  (* left, right, and parent indices *)
  fun left i = 2 * i + 1
  fun right i = 2 * (i + 1)
  fun parent i = (i + 1) div 2 - 1

  (* Assume for now that we don't need to resize.
   * Requires: depth(e) >= depth(e') for any e' in the queue already *)
  fun push ({data, len, weight, maxd, back}, e) =
    back := e :: !back
    

  fun fromSet es =
    let val q = empty ()
    in List.app (fn e => push (q, e)) es;
       q
    end

  fun totalWeight md dataslice =
    let
      fun addWeight (e, w) =
        P.p (w, P.fromDepth md (Elem.depth e))
    in
      ArraySlice.foldl addWeight P.zero dataslice
    end

  fun populateBack {data, len, weight, maxd, back} =
    let
      val n = !len
      val numToMove = Int.min (n, 10)
      val moveSlice = ArraySlice.slice (data, n - numToMove, SOME numToMove)
    in
      back := ArraySlice.foldl op:: [] moveSlice;
      weight := P.m (!weight, totalWeight (!maxd) moveSlice);
      len := n - numToMove
    end

  fun heavyPush {data, len, weight, maxd, back} e =
      let
          val _ = if !len >= maxSize then raise Full else ()
      val de = Elem.depth e
      val maxd' = Int.max (!maxd, de)
      val diff = maxd' - !maxd
      val weight' = P.p (P.l (!weight, diff), P.fromDepth maxd' de)
    in
      upd data (!len, e);
      len := (!len + 1);
      weight := weight';
      maxd := maxd'
    end

  fun choose (q as {data, len, weight, maxd, back}) =
    case !back of
      e :: es => (back := es; SOME e)
    | [] => if !len = 0 then NONE else (populateBack q; choose q)

  fun swap d (i, j) =
    let val tmp = nth d i
    in upd d (i, nth d j);
       upd d (j, tmp)
    end

  fun lt d (i, j) = Elem.depth (nth d i) < Elem.depth (nth d j)
  fun le d (i, j) = Elem.depth (nth d i) <= Elem.depth (nth d j)
  fun ge d (i, j) = not (lt d (i, j))
  fun gt d (i, j) = not (le d (i, j))

  (* TODO: for locality, swap siblings when possible? *)
  fun siftUp d i =
    if i = 0 orelse gt d (i, parent i) then ()
    else (swap d (i, parent i); siftUp d (parent i))

  fun siftDown (d, len) i =
    if left i < len andalso gt d (i, left i)
       andalso (right i >= len orelse le d (left i, right i))
    then (swap d (i, left i); siftDown (d, len) (left i))
    else if right i < len andalso gt d (i, right i) andalso le d (right i, left i)
    then (swap d (i, right i); siftDown (d, len) (right i))
    else ()

  fun insert (q as {data, len, weight, maxd, back}, e) =
    let
      val _ =
        (List.app (heavyPush q) (List.rev (!back))
         before back := [])
        handle Subscript => (print "130\n"; raise Subscript)

      val n = !len
      val _ = if n >= maxSize then raise Full else ()
      val de = Elem.depth e
      val maxd' = Int.max (!maxd, de)
      val diff = maxd' - !maxd
      val weight' = P.p (P.l (!weight, diff), P.fromDepth maxd' de)
    in
        upd data (n, e)
        handle Subscript => (print "139\n"; raise Subscript);
        siftUp data n
        handle Subscript => (print "141\n"; raise Subscript);
      len := n + 1;
      weight := weight';
      maxd := maxd'
    end

  fun split (q as {data, len, weight, maxd, back}) =
    let
      val _ =
        List.app (heavyPush q) (List.rev (!back))
        before back := []

      val tw = !weight
      val n = !len
      val md = !maxd

      fun findPrefixLen wi i =
        if P.gt (P.l (wi, 2), tw) orelse i = n then (wi, i)
        else findPrefixLen (P.p (wi, P.fromDepth md (Elem.depth (nth data i))))
                           (i+1)

      val (ws, s) = findPrefixLen P.zero 0
      val front = ArraySlice.slice (data, 0, SOME s)
      val result = ArraySlice.foldr op:: [] front

      (* Requires i < n, and data[i,n) is a valid binary heap. Swaps elements
       * forward and sifts them down, working towards the front *)
      fun fixup (i, n) =
        if i = 0 then ()
        else ( upd data (i-1, nth data (n-1))
             ; upd data (n-1, Elem.default)
             ; siftDown (data, n-1) (i-1)
             ; fixup (i-1, n-1)
             )
    in
      if s = n (* fixup doesn't work when s = n *)
      then ArraySlice.modify (fn _ => Elem.default) front
      else fixup (s, n);
      len := n - s;
      weight := P.m (tw, ws);
      if List.null result then NONE else SOME result
    end

end

(*structure E = struct type t = int val default = 0 fun depth x = x end
structure Q = HybridBinaryHeapQueue (E)*)

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
    { data : Elem.t array
    , len : int ref (* size of relevant prefix of `data` *)
    }

  type task_set = Elem.t list

  fun empty () =
    { data = Array.array (1024, Elem.default)
    , len = ref 0
    }

  fun isEmpty ({len, ...} : t) =
    !len = 0

  fun size ({len, ...} : t) = !len

  val numts = List.length

  (* fun weight ({weight, ...} : t) = weight
  fun capacity ({data, ...} : t) = Array.length data *)

  (* left, right, and parent indices *)
  fun left i = 2 * i + 1
  fun right i = 2 * (i + 1)
  fun parent i = (i + 1) div 2 - 1

  (* Assume for now that we don't need to resize.
   * Requires: depth(e) >= depth(e') for any e' in the queue already *)
  fun push ({data, len}, e) =
    (upd data (!len, e); len := !len + 1)

  fun fromSet es =
    let val q = empty ()
    in List.app (fn e => push (q, e)) es;
       q
    end

  fun choose {data, len} =
    case !len of
      0 => NONE
    | n => (len := n - 1; SOME (nth data (n-1)))

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

  fun insert (q as {data, len}, e) =
    ( push (q, e)
    ; siftUp data (!len - 1)
    )

  fun top {data, len} =
    case !len of
      0 => NONE
    | n => let val e = nth data 0
           in upd data (0, Elem.default);
              swap data (0, n-1);
              len := n-1;
              siftDown (data, n-1) 0;
              SOME e
           end

  fun totalWeightAndMaxDepth (data, n) =
    let
      fun addWeight (e, (w, md)) =
        let
          val de = Elem.depth e
          val md' = Int.max (md, de)
          val w' = P.p (P.l (w, md' - md), P.fromDepth md' de)
        in
          (w', md')
        end

      val s = ArraySlice.slice (data, 0, SOME n)
    in
      ArraySlice.foldl addWeight (P.zero, 0) s
    end

  fun split {data, len} =
    let
      val n = !len
      val (tw, md) = totalWeightAndMaxDepth (data, n)

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
      if List.null result then NONE else SOME result
    end

end

(*structure E = struct type t = int val default = 0 fun depth x = x end
structure Q = HeavySplitBinaryHeapQueue (E)*)

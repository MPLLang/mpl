functor BinaryHeapQueue (Elem : sig
                                  type t
                                  val default : t
                                  val weight : t -> int
                                end)
  (*:> QUEUE where type task = Elem.t*) =
struct

  fun nth a i = Array.sub (a, i)
  fun upd a (i, x) = Array.update (a, i, x)

  type t = {data : Elem.t array, len : int ref, weight : int ref}

  fun empty () =
    { data = Array.array (1024, Elem.default)
    , len = ref 0
    , weight = ref 0
    }

  (* fun weight ({weight, ...} : t) = weight
  fun capacity ({data, ...} : t) = Array.length data *)

  (* left, right, and parent indices *)
  fun left i = 2 * i + 1
  fun right i = 2 * (i + 1)
  fun parent i = (i + 1) div 2 - 1

  (* Assume for now that we don't need to resize.
   * Requires: pushed element e has less weight than every element
   *   in the queue *)
  fun push {data, len, weight} e =
    ( upd data (!len, e)
    ; len := (!len + 1)
    ; weight := (!weight + Elem.weight e)
    )

  fun choose {data, len, weight} =
    case !len of
      0 => NONE
    | n => let val e = nth data (n-1)
           in len := n - 1;
              upd data (n-1, Elem.default);
              weight := !weight - Elem.weight e;
              SOME e
           end

  fun swap d (i, j) =
    let val tmp = nth d i
    in upd d (i, nth d j);
       upd d (j, tmp)
    end

  fun lt d (i, j) = Elem.weight (nth d i) < Elem.weight (nth d j)
  fun ge d (i, j) = not (lt d (i, j))

  (* TODO: for locality, swap siblings when possible? *)
  fun siftUp d i =
    if i = 0 orelse lt d (i, parent i) then ()
    else (swap d (i, parent i); siftUp d (parent i))

  fun siftDown (d, len) i =
    if left i < len andalso lt d (i, left i)
       andalso (right i >= len orelse ge d (left i, right i))
    then (swap d (i, left i); siftDown (d, len) (left i))
    else if right i < len andalso lt d (i, right i) andalso ge d (right i, left i)
    then (swap d (i, right i); siftDown (d, len) (right i))
    else ()

  fun insert (q as {data, len, weight}) e =
    ( push q e
    ; siftUp data (!len - 1)
    )

  fun top {data, len, weight} =
    case !len of
      0 => NONE
    | n => let val e = nth data 0
           in upd data (0, Elem.default);
              swap data (0, n-1);
              len := n-1;
              weight := !weight - Elem.weight e;
              siftDown (data, n-1) 0;
              SOME e
           end

end

structure E = struct type t = int val default = 0 fun weight x = x end
structure Q = BinaryHeapQueue (E)

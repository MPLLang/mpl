(* A priority queue for use in basicpq.sml *)
functor MkBinaryHeap (structure Key :
                      sig
                        type t
                        val cmp : t * t -> order
                        val toString : t -> string
                      end) :>
sig
  type 'a t

  exception BinaryHeap of string

  val new : int -> 'a t (* give initial capacity *)
  val size : 'a t -> int
  val capacity : 'a t -> int
  val full : 'a t -> bool
  val empty : 'a t -> bool
  val toString : 'a t -> string

  val peekMin : 'a t -> Key.t * 'a

  (* destructive modifications *)
  val insert : 'a t -> Key.t * 'a -> unit
  val deleteMin : 'a t -> Key.t * 'a
end =
struct

  exception BinaryHeap of string

  type 'a t =
    { data : (Key.t * 'a) option array ref
    , size : int ref
    }

  fun new cap =
    if cap <= 0 then raise BinaryHeap "invalid initial capacity" else
    { data = ref (Array.array (cap, NONE))
    , size = ref 0
    }

  fun size ({size, ...} : 'a t) = !size
  fun capacity ({data, ...} : 'a t) = Array.length (!data)
  fun full h = (size h = capacity h)
  fun empty h = (size h = 0)

  fun toString (h as {data, size} : 'a t) =
    "[" ^
     String.concatWith "," (List.tabulate (!size, fn i =>
      case Array.sub (!data, i) of
        NONE => "_"
      | SOME (k, v) => Key.toString k)) ^
    "]"

  fun swap A (i, j) =
    let val (x, y) = (Array.sub (A, i), Array.sub (A, j))
    in Array.update (A, i, y); Array.update (A, j, x)
    end

  fun key ({data, size} : 'a t) i =
    let val SOME (k, _) = Array.sub (!data, i) in k end

  fun keylt h (i, j) = (Key.cmp (key h i, key h j) = LESS)
  fun keygt h (i, j) = (Key.cmp (key h i, key h j) = GREATER)

  fun parent i = ((i+1) div 2) - 1
  fun left i = 2*(i+1) - 1
  fun right i = 2*(i+1)

  (* possibly broken at data[i] *)
  fun siftDown (h as {data, size} : 'a t) i =
    if (left i >= !size  orelse keylt h (i, left i)) andalso
       (right i >= !size orelse keylt h (i, right i)) then
      () (* not broken *)
    else if right i >= !size orelse keylt h (left i, right i) then
      ( swap (!data) (i, left i)
      ; siftDown h (left i)
      )
    else
      ( swap (!data) (i, right i)
      ; siftDown h (right i)
      )

  (* possibly broken at data[i] *)
  fun siftUp (h as {data, size} : 'a t) i =
    if i = 0 orelse keygt h (i, parent i) then ()
    else ( swap (!data) (i, parent i)
         ; siftUp h (parent i)
         )

  fun resize (h as {data, size} : 'a t) =
    if not (full h) then () else
    let fun choose i = if i < !size then Array.sub (!data, i) else NONE
        val data' = Array.tabulate (2 * !size, choose)
    in data := data'
    end

  fun insert (h as {data, size} : 'a t) (k, v) =
    ( resize h
    ; Array.update (!data, !size, SOME (k, v))
    ; size := !size + 1
    ; siftUp h (!size - 1)
    )

  fun peekMin (h as {data, size} : 'a t) =
    if empty h then raise BinaryHeap "empty" else valOf (Array.sub (!data, 0))

  fun deleteMin (h as {data, size} : 'a t) =
    if empty h then raise BinaryHeap "empty" else
    let val SOME (k, v) = Array.sub (!data, 0)
    in Array.update (!data, 0, Array.sub (!data, !size - 1));
       Array.update (!data, !size - 1, NONE);
       size := !size - 1;
       siftDown h 0;
       (k, v)
    end

end


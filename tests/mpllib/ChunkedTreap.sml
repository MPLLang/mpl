functor ChunkedTreap(
  structure A:
  sig
    type 'a t
    type 'a array = 'a t
    val tabulate: int * (int -> 'a) -> 'a t
    val sub: 'a t * int -> 'a
    val subseq: 'a t -> {start: int, len: int} -> 'a t
    val update: 'a t * int * 'a -> 'a t
    val length: 'a t -> int
  end

  structure Key:
  sig
    type t
    type key = t
    val comparePriority: key * key -> order
    val compare: key * key -> order
    val toString: key -> string
  end

  val leafSize: int
) :
sig
  type 'a t
  type 'a bst = 'a t
  type key = Key.t

  val empty: unit -> 'a bst
  val singleton: key * 'a -> 'a bst

  val toString: 'a bst -> string

  val size: 'a bst -> int
  val join: 'a bst * 'a bst -> 'a bst
  val updateKey: 'a bst -> (key * 'a) -> 'a bst
  val lookup: 'a bst -> key -> 'a option
end =
struct

  type key = Key.t

  datatype 'a t =
    Empty
  | Chunk of (key * 'a) A.t
  | Node of {left: 'a t, right: 'a t, key: key, value: 'a, size: int}

  type 'a bst = 'a t

  fun toString t =
    case t of
      Empty => "()"
    | Chunk arr =>
        "(" ^ String.concatWith " "
        (List.tabulate (A.length arr, fn i => Key.toString (#1 (A.sub (arr, i)))))
        ^ ")"
    | Node {left, key, right, ...} =>
        "(" ^ toString left ^ " " ^ Key.toString key ^ " " ^ toString right ^ ")"

  fun empty () = Empty

  fun singleton (k, v) =
    Chunk (A.tabulate (1, fn _ => (k, v)))

  fun size Empty = 0
    | size (Chunk a) = A.length a
    | size (Node {size=n, ...}) = n

  fun makeChunk arr {start, len} =
    if len = 0 then
      Empty
    else
      Chunk (A.subseq arr {start = start, len = len})

  fun expose Empty = NONE

    | expose (Node {key, value, left, right, ...}) =
        SOME (left, key, value, right)

    | expose (Chunk arr) =
        let
          val n = A.length arr
          val half = n div 2
          val left = makeChunk arr {start = 0, len = half}
          val right = makeChunk arr {start = half+1, len = n-half-1}
          val (k, v) = A.sub (arr, half)
        in
          SOME (left, k, v, right)
        end


  fun nth t i =
    case t of
      Empty => raise Fail "ChunkedTreap.nth Empty"
    | Chunk arr => A.sub (arr, i)
    | Node {left, right, key, value, ...} =>
        if i < size left then
          nth left i
        else if i = size left then
          (key, value)
        else
          nth right (i - size left - 1)


  fun makeNode left (k, v) right =
    let
      val n = size left + size right + 1
      val node = Node {left=left, right=right, key=k, value=v, size=n}
    in
      if n <= leafSize then
        Chunk (A.tabulate (n, nth node))
      else
        node
    end


  fun join (t1, t2) =
    if size t1 + size t2 = 0 then
      Empty
    else if size t1 + size t2 <= leafSize then
      Chunk (A.tabulate (size t1 + size t2, fn i =>
        if i < size t1 then nth t1 i else nth t2 (i - size t1)))
    else
    case (expose t1, expose t2) of
      (NONE, _) => t2
    | (_, NONE) => t1
    | (SOME (l1, k1, v1, r1), SOME (l2, k2, v2, r2)) =>
        case Key.comparePriority (k1, k2) of
          GREATER => makeNode l1 (k1, v1) (join (r1, t2))
        | _ => makeNode (join (t1, l2)) (k2, v2) r2

  local
    fun fail k =
      raise Fail ("ChunkedTreap.updateKey: key not found: " ^ Key.toString k)
  in
  fun updateKey t (k, v) = ((*print ("updateKey " ^ toString t ^ " (" ^ Key.toString k ^ ", ...)" ^ "\n");*)
    case t of
      Empty => fail k

    | Chunk arr =>
        let
          val n = A.length arr
          fun loop i =
            if i >= n then
              fail k
            else case Key.compare (k, #1 (A.sub (arr, i))) of
              EQUAL => A.update (arr, i, (k, v))
            | GREATER => loop (i+1)
            | LESS => fail k
        in
          Chunk (loop 0)
        end

    | Node {left, right, key, value, ...} =>
        case Key.compare (k, key) of
          LESS => makeNode (updateKey left (k, v)) (key, value) right
        | GREATER => makeNode left (key, value) (updateKey right (k, v))
        | EQUAL => makeNode left (k, v) right)
  end


  fun lookup t k =
    case t of
      Empty => NONE

    | Chunk arr =>
        let
          val n = A.length arr
          fun loop i =
            if i >= n then
              NONE
            else case Key.compare (k, #1 (A.sub (arr, i))) of
              EQUAL => SOME (#2 (A.sub (arr, i)))
            | GREATER => loop (i+1)
            | LESS => NONE
        in
          loop 0
        end

    | Node {left, right, key, value, ...} =>
        case Key.compare (k, key) of
          LESS => lookup left k
        | GREATER => lookup right k
        | EQUAL => SOME value

end

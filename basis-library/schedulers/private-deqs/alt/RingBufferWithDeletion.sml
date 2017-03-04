(* A RingBuffer is a double-ended queue. It has amortized O(1) push/pop at
 * both ends. Internally it has a capacity and automatically doubles in size
 * when capacity is reached. Pushing an element returns a handle to that
 * element, allowing elements to deleted by directly referencing them. Deletions
 * are implemented via tombstones, which are cleaned out at every resize. *)
structure RingBufferWithDeletion :>
sig
  type 'a t (* ring buffer with elements 'a *)
  type 'a h (* a handle pointing to an element of type 'a *)

  (* `new cap` makes an empty ring buffer with initial capacity `cap` *)
  val new : int -> 'a t

  val size : 'a t -> int
  val empty : 'a t -> bool (* equivalent to (size q = 0) *)
  val app : ('a -> unit) -> 'a t -> unit

  val peekBot : 'a t -> 'a option
  val peekTop : 'a t -> 'a option

  (* adding a new element returns a handle *)
  val pushBot : 'a * 'a t -> 'a h
  val pushTop : 'a t * 'a -> 'a h

  (* popping an element implicitly invalidates its handle *)
  val popBot : 'a t -> 'a option
  val popTop : 'a t -> 'a option

  (* delete an element from anywhere within a ring buffer (places tombstone);
   * returns whether or not it succeeded (fails when the handle is invalid). *)
  val remove : 'a h -> bool

  (* contains (q, xh) is true if xh is a handle pointing into q *)
  val contains : 'a t * 'a h -> bool

  val toString : ('a -> string) -> 'a t -> string
  val toList : 'a t -> 'a list
end =
struct
  exception RingBuffer of string

  datatype 'a h = Handle of ('a t * int ref)

  (* data[start,start+range) contains elems and tombstones
   * size is number of elements within data[start,start+range) *)
  and 'a t = Buffer of { data  : ('a * 'a h) option array ref
                       , start : int ref
                       , range : int ref
                       , size  : int ref
                       }

  fun new cap =
    if cap <= 0
    then raise RingBuffer "Initial RingBuffer capacity must be at least 1"
    else Buffer { data  = ref (Array.array (cap, NONE))
                , start = ref (cap div 2)
                , range = ref 0
                , size  = ref 0
                }

  fun size (Buffer {size, ...}) = !size
  fun empty q = (size q = 0)
  fun capacity (Buffer {data, ...}) = Array.length (!data)
  fun atCapacity (q as Buffer {range, ...}) = (capacity q = !range)

  (* Assume 0 <= i < !range. Returns (start+i) mod (capacity q). *)
  (*fun wrapIdx (q as Buffer {start, ...}) i =
    let val i' = i + !start
    in if i' < capacity q then i' else i' - capacity q
    end*)

  fun wrapSub (a, i) =
    let val n = Array.length a
    in Array.sub (a, if i < n then i else i - n)
    end

  fun wrapUpdate (a, i, x) =
    let val n = Array.length a
    in Array.update (a, if i < n then i else i - n, x)
    end

  fun iterate f b (Buffer {data = ref D, start = ref s, range = ref n, ...}) =
    let
      fun loop b i =
        if i = n then b else
        case wrapSub (D, s+i) of
          SOME e => loop (f (b, e)) (i+1)
        | NONE => loop b (i+1)
    in
      loop b 0
    end

  (* Parallel? Difficult because this code is a dependency for ParallelBasic! *)
  fun app (f : 'a -> unit) (q : 'a t) =
    iterate (fn (_, (x, _)) => f x) () q

  (* Require newCap >= !size *)
  fun resize (q as Buffer {data, start, range, size}) newCap =
    let
      val newData = Array.array (newCap, NONE)
      fun f (j, e as (_, Handle (_, p))) =
        ( p := j (* update the handle *)
        ; Array.update (newData, j, SOME e)
        ; j+1
        )
    in
      ( if iterate f 0 q = !size then () else raise RingBuffer "resize error"
      ; data := newData
      ; start := 0
      ; range := !size
      )
    end

  fun maybeResize (q as Buffer {range, ...}) =
    if !range < capacity q then () else resize q (!range * 2)

  (* Double the capacity of q, if it is full. *)
  (*
  fun resize (q as Buffer {data, start, range, size}) =
    if not (full q) then () else
    let
      fun select i = Option.map (fn e as (_,(_,p)) => (p := i; e))
        (if i < !start then NONE else
         if i < capacity q then Array.sub (!data, i) else
         if i < !start + !range then Array.sub (!data, i - capacity q)
         else NONE)
      val data' = Array.tabulate (2 * capacity q, select)
    in
      data := data'
    end
  *)

  fun pushBot (x : 'a, q as Buffer {data, start, range, size}) : 'a h =
    ( maybeResize q
    ; start := (if !start = 0 then capacity q - 1 else !start - 1)
    ; range := !range + 1
    ; size := !size + 1
    ; let val xh = Handle (q, ref (!start))
      in ( Array.update (!data, !start, SOME (x, xh))
         ; xh
         )
      end
    )

  fun pushTop (q as Buffer {data, start, range, size}, x : 'a) : 'a h =
    ( maybeResize q
    ; let val i = !start + !range
          val i' = if i >= capacity q then i - capacity q else i
          val xh = Handle (q, ref i')
      in ( Array.update (!data, i', SOME (x, xh))
         ; range := !range + 1
         ; size := !size + 1
         ; xh
         )
      end
    )

  (* move !start to point to first element *)
  fun restoreStart (q as Buffer {data, start, range, size}) =
    if Option.isSome (Array.sub (!data, !start)) then ()
    else ( start := (if !start = capacity q - 1 then 0 else !start + 1)
         ; range := !range - 1
         ; restoreStart q
         )

  fun restoreRange (q as Buffer {data, start, range, size}) =
    if Option.isSome (wrapSub (!data, !start + !range - 1)) then ()
    else ( range := !range - 1
         ; restoreRange q
         )

  fun popBot (q as Buffer {data, start, range, size}) : 'a option =
    if empty q then NONE else
    case Array.sub (!data, !start) of
      NONE => raise RingBuffer "popBot failure"
    | SOME (x, _) =>
        ( Array.update (!data, !start, NONE)
        ; size := !size - 1
        ; start := (if !start = capacity q - 1 then 0 else !start + 1)
        ; range := !range - 1
        ; if !size = 0 then () else restoreStart q
        ; SOME x
        )

  fun popTop (q as Buffer {data, start, range, size}) : 'a option =
    if empty q then NONE else
    case wrapSub (!data, !start + !range - 1) of
      NONE => raise RingBuffer "popTop failure"
    | SOME (x, _) =>
        ( wrapUpdate (!data, !start + !range - 1, NONE)
        ; range := !range - 1
        ; size := !size - 1
        ; if !size = 0 then () else restoreRange q
        ; SOME x
        )

  fun peekBot (q as Buffer {data, start, range, size}) : 'a option =
    Option.map (fn (x,_) => x) (Array.sub (!data, !start))

  fun peekTop (q as Buffer {data, start, range, size}) : 'a option =
    Option.map (fn (x,_) => x) (wrapSub (!data, !start + !range - 1))

  fun contains (q1 as Buffer {data=d1, ...}, Handle (q2 as Buffer {data=d2, ...}, p)) =
    d1 = d2 andalso (* queues match *)
    case Array.sub (!d1, !p) of
      NONE => false
    | SOME (_, Handle (_, p')) => p = p' (* handles match *)

  fun remove (Handle (q as Buffer {data, start, range, size}, p)) =
    case Array.sub (!data, !p) of
      NONE => false
    | SOME (_, Handle (_, p')) =>
        if p <> p' then false
        else ( Array.update (!data, !p, NONE)
             ; size := !size - 1
             ; if !size = 0 then range := 0
               else ( restoreStart q
                    ; restoreRange q
                    )
             ; true
             )

  fun toString xtos (Buffer {data, start, range, size}) =
    "[" ^
    (String.concatWith "," (List.tabulate (Array.length (!data), fn i =>
      (if i = !start then "*" else "") ^
      (case Array.sub (!data, i) of
        NONE => "_"
      | SOME (x,_) => xtos x)))) ^
    "]"

  fun toList q =
    List.rev (iterate (fn (l, (x,_)) => x :: l) [] q)
end

(*
open RingBuffer
fun pq q = print (toString Int.toString q ^ "\n");
val q = new 2 : int t; pq q;
val h0 = pushBot (0, q); pq q;
*)

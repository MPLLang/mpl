(* Unoptimized ring buffer implementation. *)
structure RingBuffer :>
sig
  type 'a t

  (* `new cap` makes an empty ring buffer with capacity `cap` *)
  val new : int -> 'a t

  (* nondestructive *)
  val size : 'a t -> int
  val capacity : 'a t -> int
  val full : 'a t -> bool
  val empty : 'a t -> bool
  val toList : 'a t -> 'a list
  val toString : ('a -> string) -> 'a t -> string
  val app : ('a -> unit) -> 'a t -> unit

  val peekBot : 'a t -> 'a option
  val peekTop : 'a t -> 'a option

  (* destructive *)
  val pushBot : 'a * 'a t -> unit
  val pushTop : 'a t * 'a -> unit
  val popBot : 'a t -> 'a option
  val popTop : 'a t -> 'a option

  val popBotDiscard : 'a t -> bool
end =
struct
  exception RingBuffer of string

  type 'a t =
    { data : 'a option array ref
    , start : int ref
    , size : int ref
    }

  fun new cap =
    { data = ref (Array.array (cap, NONE))
    , start = ref (cap div 2)
    , size = ref 0
    }

  fun size ({size, ...} : 'a t) = !size
  fun capacity ({data, ...} : 'a t) = Array.length (!data)
  fun full q = (size q = capacity q)
  fun empty q = (size q = 0)

  (* assume 0 <= i < size q *)
  fun shiftIdx (q as {start, ...} : 'a t) i =
    let val i' = i + !start
    in if i' < capacity q then i' else i' - capacity q
    end

  fun nth (q as {data, start, size} : 'a t) i =
    if i < 0 orelse i >= !size
    then raise RingBuffer "nth: bad index"
    else Option.valOf (Array.sub (!data, shiftIdx q i))

  (* TODO: parallel? difficult because this is dependency for ParallelBasic *)
  fun app f (q as {data, start, size} : 'a t) =
    let
      val n = !size
      fun loop i =
        if i = n then ()
        else ( ignore (f (Option.valOf (Array.sub (!data, shiftIdx q i))))
             ; loop (i+1)
             )
    in
      loop 0
    end

  (* Double the size of q, if it is full. *)
  fun resize (q as {data, start, size} : 'a t) =
    if not (full q) then () else
    let
      fun select i =
        if i < !start then NONE else
        if i < capacity q then Array.sub (!data, i) else
        if i < !start + !size then Array.sub (!data, i - capacity q)
        else NONE
      val data' = Array.tabulate (2 * capacity q, select)
    in
      data := data'
    end

  fun pushBot (x : 'a, q as {data, start, size} : 'a t) =
    ( resize q (* double size if full *)
    ; start := (if !start = 0 then capacity q - 1 else !start - 1)
    ; size := !size + 1
    ; Array.update (!data, !start, SOME x)
    )

  fun pushTop (q as {data, start, size} : 'a t, x : 'a) =
    let in
      resize q; (* double size if full  *)
      Array.update (!data, shiftIdx q (!size), SOME x);
      size := !size + 1
    end

  fun popBot (q as {data, start, size} : 'a t) : 'a option =
    if empty q then NONE
    else let
         in Array.sub (!data, !start)
            before (Array.update (!data, !start, NONE);
                    start := (if !start = capacity q - 1
                              then 0
                              else !start + 1);
                    size := !size - 1)
         end

  fun popBotDiscard (q as {data, start, size} : 'a t) : bool =
    not (empty q)
    andalso ( Array.update (!data, !start, NONE)
            ; start := (if !start = capacity q - 1 then 0 else !start + 1)
            ; size := !size - 1
            ; true
            )

  fun peekBot (q as {data, start, size} : 'a t) : 'a option =
    if empty q then NONE else Array.sub (!data, !start)

  fun popTop (q as {data, start, size} : 'a t) : 'a option =
    if empty q then NONE
    else let val i = shiftIdx q (!size - 1)
         in Array.sub (!data, i)
            before (Array.update (!data, i, NONE);
                    size := !size - 1)
         end

  fun peekTop (q as {data, start, size} : 'a t) : 'a option =
    if empty q then NONE else Array.sub (!data, shiftIdx q (!size - 1))

  fun toList q = List.tabulate (size q, nth q)

  fun toString xtos ({data, start, size} : 'a t) =
    "[" ^
    (String.concatWith "," (List.tabulate (Array.length (!data), fn i =>
      (if i = !start then "*" else "") ^
      (case Array.sub (!data, i) of
        NONE => "_"
      | SOME x => xtos x)))) ^
    "]"
end

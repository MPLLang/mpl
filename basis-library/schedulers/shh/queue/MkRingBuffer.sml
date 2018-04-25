functor MkRingBuffer (val initialCapacity : int) :>
sig
  type 'a t

  val new : int -> (unit -> unit) t

  (* nondestructive *)
  val size : 'a t -> int
  val capacity : 'a t -> int
  val full : 'a t -> bool
  val empty : 'a t -> bool
  val toList : 'a t -> 'a list
  (*val toString : ('a -> string) -> 'a t -> string*)
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

  type 'a t = {data : 'a array, start : int, size : int} ref

  (*fun arraySub x = Array.unsafeSub x
  fun arrayUpdate x = Array.unsafeUpdate x*)

  fun arraySub x = Array.sub x
  fun arrayUpdate x = Array.update x

  fun new p =
    let (*val data = MLton.Parallel.Unsafe.arrayUninit initialCapacity*)
        val data = Array.array (initialCapacity, fn _ => ())
        val _ = MLton.HM.registerQueue (Word32.fromInt p, data)
    in ref { data = data
           , start = initialCapacity div 2
           , size = 0
           }
    end

  fun size (ref {size, ...} : 'a t) = size
  fun capacity (ref {data, ...} : 'a t) = Array.length data
  fun full q = (size q = capacity q)
  fun empty q = (size q = 0)

  (* assume 0 <= i < size q *)
  fun shiftIdx (data, start, i) =
    let val i' = i + start
    in if i' < Array.length data then i' else i' - Array.length data
    end

  fun nth (q as ref {data, start, size} : 'a t) i =
    if i < 0 orelse i >= size
    then raise RingBuffer "nth: bad index"
    else arraySub (data, shiftIdx (data, start, i))

  (* TODO: parallel? difficult because this is dependency for ParallelBasic *)
  fun app f (q as ref {data, start, size} : 'a t) =
    let
      fun loop i =
        if i = size then ()
        else ( ignore (f (arraySub (data, shiftIdx (data, start, i))))
             ; loop (i+1)
             )
    in
      loop 0
    end

  (* Double the size of q, if it is full. *)
  fun resize (q as ref {data, start, size} : 'a t) =
    ( print (Int.toString (MLton.Parallel.processorNumber ()) ^ ": attempted to resize\n")
    ; OS.Process.exit OS.Process.failure
    ; ()
    )
    (* if size < Array.length data then () else
    let
      fun select i =
        if i < Array.length data then arraySub (data, i)
        else if i < start + size then arraySub (data, i - Array.length data)
        else arraySub (data, 0) (* choose arbitrary junk elem *)
    in
      q := { data = Array.tabulate (2 * Array.length data, select)
           , start = start
           , size = size
           }
    end *)

  fun pushBot' (x : 'a, q as ref {data, start, size} : 'a t) =
    let val start' = if start = 0 then Array.length data - 1 else start - 1
    in ( q := {data = data, start = start', size = size + 1}
       ; arrayUpdate (data, start', x)
       )
    end
  fun pushBot (x, q) = (resize q; pushBot' (x, q))

  fun pushTop' (q as ref {data, start, size} : 'a t, x : 'a) =
    let val i = start + size
        val i' = if i < Array.length data then i else i - Array.length data
    in ( arrayUpdate (data, i', x)
       ; q := {data = data, start = start, size = size + 1}
       )
    end
  fun pushTop (q, x) = (resize q; pushTop' (q, x))

  fun popBot (q as ref {data, start, size} : 'a t) : 'a option =
    if size = 0 then NONE else
    let
      val result = arraySub (data, start)
      val start' = if start = Array.length data - 1 then 0 else start + 1
    in
      ( q := {data = data, start = start', size = size - 1}
      ; SOME result
      )
    end

  fun popBotDiscard (q as ref {data, start, size} : 'a t) : bool =
    (size > 0) andalso
    let
      val start' = if start = Array.length data - 1 then 0 else start + 1
    in
      ( q := {data = data, start = start', size = size - 1}
      ; true
      )
    end

  fun peekBot (q as ref {data, start, size} : 'a t) : 'a option =
    if size = 0 then NONE else SOME (arraySub (data, start))

  fun popTop (q as ref {data, start, size} : 'a t) : 'a option =
    if size = 0 then NONE else
    let
      val i = start + size - 1
      val i' = if i < Array.length data then i else i - Array.length data
      val result = arraySub (data, i')
    in
      ( q := {data = data, start = start, size = size - 1}
      ; SOME result
      )
    end

  fun peekTop (q as ref {data, start, size} : 'a t) : 'a option =
    if size = 0 then NONE
    else SOME (arraySub (data, shiftIdx (data, start, size - 1)))

  fun toList q = List.tabulate (size q, nth q)
end

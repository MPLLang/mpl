structure RingBuffer :>
sig
  type 'a t

  val new: {capacity: int} -> 'a t

  (* nondestructive *)
  val size: 'a t -> int
  val capacity: 'a t -> int
  val full: 'a t -> bool
  val empty: 'a t -> bool

  val peekTop: 'a t -> 'a option
  val peekBot: 'a t -> 'a option

  (* destructive *)
  val pushBot: 'a t * 'a -> bool
  val pushTop: 'a t * 'a -> bool
  val popBot: 'a t -> 'a option
  val popTop: 'a t -> 'a option
  
end =
struct
  datatype 'a t =
    B of
      { data: 'a option array
      , start: int ref
      , size: int ref
      , lock: SpinLock.t
      }

  (*fun arraySub x = Array.unsafeSub x
  fun arrayUpdate x = Array.unsafeUpdate x*)

  fun arraySub (a, i) =
    if i < 0 orelse i >= Array.length a then
      raise Subscript
    else
      MLton.HM.arraySubNoBarrier (a, i)

  fun arrayUpdate (a, i, x) =
    if i < 0 orelse i >= Array.length a then
      raise Subscript
    else
      MLton.HM.arrayUpdateNoBarrier (a, i, x)

  fun new {capacity} =
    let
      val data = Array.array (capacity, NONE)
    in
      B { data = data
        , start = ref 0
        , size = ref 0
        , lock = SpinLock.new ()
        }
    end

  fun size (B {size, ...} : 'a t) = !size
  fun capacity (B {data, ...} : 'a t) = Array.length data
  fun full q = (size q = capacity q)
  fun empty q = (size q = 0)

  val myWorkerId = MLton.Parallel.processorNumber
  fun die strfn =
    ( print ("ERROR: " ^ Int.toString (myWorkerId ()) ^ ": " ^ strfn () ^ "\n")
    ; OS.Process.exit OS.Process.failure
    )

  fun check (B {data, start, size, ...}) msg =
    let
      val capacity = Array.length data
      val start = !start
      val size = !size
      
      val okay =
        start >= 0 andalso start < capacity
        andalso
        size >= 0 andalso size <= capacity
    in
      if okay then ()
      else die (fn _ => msg ^ " capacity=" ^ Int.toString capacity ^ " start=" ^ Int.toString start ^ " size=" ^ Int.toString size)
    end


  fun pushBot (q as B {data, start, size, lock} : 'a t, x : 'a) =
    if full q then false else
    ( SpinLock.lock lock
    ; if full q then (SpinLock.unlock lock; false) else
      let
        val _ = check q "before pushBot"
        val start' = if !start = 0 then Array.length data - 1 else !start - 1
      in
        ( start := start'
        ; size := !size + 1
        ; arrayUpdate (data, start', SOME x)
        ; check q "after pushBot"
        ; SpinLock.unlock lock
        ; true
        )
      end
    )


  fun pushTop (q as B {data, start, size, lock} : 'a t, x : 'a) =
    if full q then false else
    ( SpinLock.lock lock
    ; if full q then (SpinLock.unlock lock; false) else
      let
        val _ = check q "before pushTop"
        val i = !start + !size
        val i' = if i < Array.length data then i else i - Array.length data
      in
        ( arrayUpdate (data, i', SOME x)
        ; size := !size + 1
        ; check q "after pushTop"
        ; SpinLock.unlock lock
        ; true
        )
      end
    )


  fun popBot (q as B {data, start, size, lock} : 'a t) : 'a option =
    if !size = 0 then NONE else
    ( SpinLock.lock lock
    ; if !size = 0 then (SpinLock.unlock lock; NONE) else
      let
        val _ = check q "before popBot"
        val result = arraySub (data, !start)
        val _ = arrayUpdate (data, !start, NONE)
        val start' = if !start = Array.length data - 1 then 0 else !start + 1
      in
        ( start := start'
        ; size := !size - 1
        ; check q "after popBot"
        ; SpinLock.unlock lock
        ; result
        )
      end
    )


  fun popTop (q as B {data, start, size, lock} : 'a t) : 'a option =
    if !size = 0 then NONE else
    ( SpinLock.lock lock
    ; if !size = 0 then (SpinLock.unlock lock; NONE) else
      let
        val _ = check q "before popTop"
        val i = !start + !size - 1
        val i' = if i < Array.length data then i else i - Array.length data
        val result = arraySub (data, i')
        val _ = arrayUpdate (data, i', NONE)
      in
        ( size := !size - 1
        ; check q "after popTop"
        ; SpinLock.unlock lock
        ; result
        )
      end
    )


  fun peekTop (q as B {data, start, size, lock} : 'a t) : 'a option =
    if !size = 0 then NONE else
    ( SpinLock.lock lock
    ; if !size = 0 then (SpinLock.unlock lock; NONE) else
      let
        val _ = check q "before peekTop"
        val i = !start + !size - 1
        val i' = if i < Array.length data then i else i - Array.length data
        val result = arraySub (data, i')
        (* val _ = arrayUpdate (data, i', NONE) *)
      in
        ( (*size := !size - 1
        ;*) check q "after peekTop"
        ; SpinLock.unlock lock
        ; result
        )
      end
    )


  fun peekBot (q as B {data, start, size, lock} : 'a t) : 'a option =
    if !size = 0 then NONE else
    ( SpinLock.lock lock
    ; if !size = 0 then (SpinLock.unlock lock; NONE) else
      let
        val _ = check q "before peekBot"
        val result = arraySub (data, !start)
        (* val _ = arrayUpdate (data, !start, NONE)
        val start' = if !start = Array.length data - 1 then 0 else !start + 1 *)
      in
        ( (*start := start'
        ; size := !size - 1
        ;*) check q "after peekBot"
        ; SpinLock.unlock lock
        ; result
        )
      end
    )

end

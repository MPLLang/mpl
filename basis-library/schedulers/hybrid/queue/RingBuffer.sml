structure RingBuffer :>
sig
  type 'a t

  exception Full

  val new: {capacity: int} -> 'a t

  (* nondestructive *)
  val size: 'a t -> int
  val capacity: 'a t -> int
  val full: 'a t -> bool
  val empty: 'a t -> bool

  (* destructive *)
  val pushBot: 'a t * 'a -> unit
  val pushTop: 'a t * 'a -> unit
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

  exception Full

  (*fun arraySub x = Array.unsafeSub x
  fun arrayUpdate x = Array.unsafeUpdate x*)

  fun arraySub x = MLton.HM.arraySubNoBarrier x
  fun arrayUpdate x = MLton.HM.arrayUpdateNoBarrier x

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


  fun pushBot (q as B {data, start, size, lock} : 'a t, x : 'a) =
    if full q then raise Full else
    let
      val _ = SpinLock.lock lock
      val start' = if !start = 0 then Array.length data - 1 else !start - 1
    in
      ( start := start'
      ; size := !size + 1
      ; arrayUpdate (data, start', SOME x)
      ; SpinLock.unlock lock
      )
    end


  fun pushTop (q as B {data, start, size, lock} : 'a t, x : 'a) =
    if full q then raise Full else
    let
      val _ = SpinLock.lock lock
      val i = !start + !size
      val i' = if i < Array.length data then i else i - Array.length data
    in
      ( arrayUpdate (data, i', SOME x)
      ; size := !size + 1
      ; SpinLock.unlock lock
      )
    end


  fun popBot (q as B {data, start, size, lock} : 'a t) : 'a option =
    if !size = 0 then NONE else
    let
      val _ = SpinLock.lock lock
      val result = arraySub (data, !start)
      val _ = arrayUpdate (data, !start, NONE)
      val start' = if !start = Array.length data - 1 then 0 else !start + 1
    in
      ( start := start'
      ; size := !size - 1
      ; SpinLock.unlock lock
      ; result
      )
    end


  fun popTop (q as B {data, start, size, lock} : 'a t) : 'a option =
    if !size = 0 then NONE else
    let
      val _ = SpinLock.lock lock
      val i = !start + !size - 1
      val i' = if i < Array.length data then i else i - Array.length data
      val result = arraySub (data, i')
      val _ = arrayUpdate (data, i', NONE)
    in
      ( size := !size - 1
      ; SpinLock.unlock lock
      ; result
      )
    end
end

structure ArrayQueue :
sig
  type 'a t

  val new : int -> 'a t
  val empty : 'a t -> bool

  val setDepth : 'a t -> int -> unit

  val pushBot : 'a * 'a t -> unit

  val popBot : 'a t -> 'a option

  val tryPopTop : 'a t -> ('a * int) option

  val pollHasWork : 'a t -> bool

  val toList : 'a t -> 'a list
end =
struct
  val capacity = 128
  type 'a t = {data : 'a option array,
               start : Word64.word ref,
               frontier : int ref,
               owner : int}

  structure Thread = MLton.Thread.Basic

  fun myWorkerId () =
    MLton.Parallel.processorNumber ()

  fun die strfn =
    ( print (Int.toString (myWorkerId ()) ^ ": " ^ strfn ())
    ; OS.Process.exit OS.Process.failure
    )

  fun arraySub (a, i) =
    Array.sub (a, i)

  fun arrayUpdate (a, i, x) =
    MLton.HM.arrayUpdateNoBarrier (a, Int64.fromInt i, x)

  val mask = Word64.- (Word64.<< (0w1, 0w62), 0w1)
  fun isLockedVal s =
    Word64.>> (s, 0w62) <> 0w0
  val lockedByMeMarker = Word64.<< (0w1, 0w62)
  val lockedByGCMarker = Word64.<< (0w2, 0w62)
  val lockedByOtherMarker = Word64.<< (0w3, 0w62)
  fun makeLockedVal owner s =
    if myWorkerId () = owner then
      Word64.orb (Word64.andb (s, mask), lockedByMeMarker)
    else
      Word64.orb (Word64.andb (s, mask), lockedByOtherMarker)

  fun lockGetStart (q as {start, owner, ...} : 'a t) =
    let
      fun loop s =
        if isLockedVal s then loop (!start) else
        let
          val s' = MLton.Parallel.compareAndSwap start (s, makeLockedVal owner s)
        in
          if s = s' then s else loop s'
        end
    in
      Word64.toInt (loop (!start))
    end

  fun tryLockGetStart (q as {start, owner, ...} : 'a t) =
    let
      val s = !start
    in
      if isLockedVal s then
        NONE
      else
        let
          val s' = MLton.Parallel.compareAndSwap start (s, makeLockedVal owner s)
        in
          if s = s' then SOME (Word64.toInt s) else NONE
        end
    end

  (* fun unlockSetStart (q as {start, ...} : 'a t) s =
    start := Word64.fromInt s *)

  (* fun getStart ({start, ...} : 'a t) = Word64.toInt (!start) *)
  fun setStart ({start, owner, ...} : 'a t) s =
    let
      val old = !start
    in
      (* really we just need an atomic store, but this is good
       * enough for simple implementation. *)
      if old <> MLton.Parallel.compareAndSwap start (old, Word64.fromInt s) orelse
         old <> makeLockedVal owner (Word64.andb (old, mask))
      then
        die (fn _ => "scheduler bug: unlock by setting start, but start = " ^ Word64.toString old)
      else
        ()
    end

  fun new p =
    let
      val data = Array.array (capacity, NONE)
    in
      { data = data
      , start = ref (0w0 : Word64.word)
      , frontier = ref 0
      , owner = p
      }
    end

  fun size (q as {start, frontier, ...} : 'a t) =
    let
      val s = lockGetStart q
      val result = !frontier - s
    in
      setStart q s;
      result
    end

  fun pollHasWork (q as {start, frontier, ...} : 'a t) =
    let
      val s = Word64.toInt (Word64.andb (!start, mask))
      val f = !frontier
    in
      f > s
    end

  fun empty q = (size q = 0)

  fun setDepth (q as {start, frontier, ...} : 'a t) d =
    let
      val s = lockGetStart q
      val f = !frontier
    in
      if s <> f then
        die (fn _ => "scheduler bug: can only set depth on empty queue")
      else
        ( frontier := d
        ; setStart q d
        )
    end

  fun pushBot (x, q as {data, start, frontier, ...} : 'a t) =
    let
      val s = lockGetStart q
      val f = !frontier
    in
      ( arrayUpdate (data, f, SOME x)
      ; frontier := f + 1
      ; setStart q s
      )
    end

  fun popBot (q as {data, start, frontier, ...} : 'a t) =
    let
      val s = lockGetStart q
      val f = !frontier
    in
      if s = f then
        (setStart q s; NONE)
      else
        let
          val result = arraySub (data, f-1)
        in
          frontier := f-1;
          arrayUpdate (data, f-1, NONE);
          setStart q s;
          result
        end
    end

  fun tryPopTop (q as {data, start, frontier, ...} : 'a t) =
    case tryLockGetStart q of
      NONE => NONE
    | SOME s =>
        let
          val f = !frontier
        in
          if s = f then
            (setStart q s; NONE)
          else
            let
              val result = arraySub (data, s)
            in
              arrayUpdate (data, s, NONE);
              setStart q (s+1);
              SOME (valOf result, s)
            end
        end

  fun toList (q as {data, start, frontier, ...} : 'a t) =
    let
      val s = lockGetStart q
      val f = !frontier
      val sl = ArraySlice.slice (data, s, SOME (f - s))
      val result = ArraySlice.foldr (fn (x, l) => valOf x :: l) [] sl
    in
      setStart q s;
      result
    end

end

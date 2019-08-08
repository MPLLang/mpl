structure ArrayQueue :
sig
  type 'a t

  val new : int -> 'a t
  val empty : 'a t -> bool

  val setDepth : 'a t -> int -> unit

  val pushFront : 'a * 'a t -> unit

  val popFront : 'a t -> 'a option
  val popBack : 'a t -> ('a * int) option

  val pollHasWork : 'a t -> bool

  val toList : 'a t -> 'a list
end =
struct
  val capacity = 128
  type 'a t = {data : 'a option array,
               start : Word64.word ref,
               frontier : int ref,
               owner : int}

  fun arraySub (a, i) =
    Array.sub (a, i)

  fun arrayUpdate (a, i, x) =
    MLton.HM.arrayUpdateNoBarrier (a, Int64.fromInt i, x)

  val mask = Word64.- (Word64.<< (0w1, 0w62), 0w1)
  fun isLockedVal s =
    Word64.>> (s, 0w62) <> 0w0
  val lockedByMeMarker = Word64.<< (0w1, 0w62)
  val lockedByOtherMarker = Word64.<< (0w2, 0w62)
  fun makeLockedVal owner s =
    if MLton.Parallel.processorNumber () = owner then
      Word64.orb (s, lockedByMeMarker)
    else
      Word64.orb (s, lockedByOtherMarker)

  fun lockGetStart (q as {start, owner, ...} : 'a t) =
    let
      val s = !start
    in
      if isLockedVal s then
        lockGetStart q
      else if s <> MLton.Parallel.compareAndSwap start (s, makeLockedVal owner s) then
        lockGetStart q
      else
        Word64.toInt s
    end

  (* fun unlockSetStart (q as {start, ...} : 'a t) s =
    start := Word64.fromInt s *)

  (* fun getStart ({start, ...} : 'a t) = Word64.toInt (!start) *)
  fun setStart ({start, ...} : 'a t) s = start := Word64.fromInt s

  fun new p =
    let
      val data = Array.array (capacity, NONE)
    in
      { data = data
      , start = ref 0w0
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
        raise Fail "can only set depth on empty queue"
      else
        ( frontier := d
        ; setStart q d
        )
    end

  fun pushFront (x, q as {data, start, frontier, ...} : 'a t) =
    let
      val s = lockGetStart q
      val f = !frontier
    in
      ( arrayUpdate (data, f, SOME x)
      ; frontier := f + 1
      ; setStart q s
      )
    end

  fun popFront (q as {data, start, frontier, ...} : 'a t) =
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

  fun popBack (q as {data, start, frontier, ...} : 'a t) =
    let
      val s = lockGetStart q
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

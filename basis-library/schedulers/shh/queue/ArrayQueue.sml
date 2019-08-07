structure ArrayQueue : QUEUE =
struct
  val capacity = 128
  type 'a t = {data : 'a option array,
               start : Word64.word ref,
               frontier : int ref}

  fun arraySub (a, i) =
    Array.sub (a, i)

  fun arrayUpdate (a, i, x) =
    MLton.HM.arrayUpdateNoBarrier (a, Int64.fromInt i, x)

  fun getStart ({start, ...} : 'a t) = Word64.toInt (!start)
  fun setStart ({start, ...} : 'a t) s = start := Word64.fromInt s

  fun new p =
    let
      val data = Array.array (capacity, NONE)
    in
      { data = data
      , start = ref 0w0
      , frontier = ref 0
      }
    end

  fun size (q as {start, frontier, ...} : 'a t) = !frontier - getStart q
  fun empty q = (size q = 0)

  fun setDepth (q as {start, frontier, ...} : 'a t) d =
    if not (empty q) then
      raise Fail "can only set depth on empty queue"
    else
      ( setStart q d
      ; frontier := d
      )

  fun pushFront (x, {data, start, frontier} : 'a t) =
    let
      val f = !frontier
    in
      ( arrayUpdate (data, f, SOME x)
      ; frontier := f + 1
      )
    end

  fun popFront (q as {data, start, frontier} : 'a t) =
    let
      val s = getStart q
      val f = !frontier
    in
      if s = f then
        NONE
      else
        let
          val result = arraySub (data, f-1)
        in
          frontier := f-1;
          arrayUpdate (data, f-1, NONE);
          result
        end
    end

  fun popBack (q as {data, start, frontier} : 'a t) =
    let
      val s = getStart q
      val f = !frontier
    in
      if s = f then
        NONE
      else
        let
          val result = arraySub (data, s)
        in
          setStart q (s+1);
          arrayUpdate (data, s, NONE);
          SOME (valOf result, s)
        end
    end

  fun toList (q as {data, start, frontier} : 'a t) =
    let
      val sl = ArraySlice.slice (data, getStart q, SOME (size q))
    in
      ArraySlice.foldr (fn (x, l) => valOf x :: l) [] sl
    end

end

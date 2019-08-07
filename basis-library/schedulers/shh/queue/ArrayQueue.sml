structure ArrayQueue : QUEUE =
struct
  val capacity = 128
  type 'a t = {data : 'a option array, start : int ref, frontier : int ref}

  fun arraySub (a, i) =
    Array.sub (a, i)

  fun arrayUpdate (a, i, x) =
    MLton.HM.arrayUpdateNoBarrier (a, Int64.fromInt i, x)

  fun new p =
    let
      val data = Array.array (capacity, NONE)
      (* val _ = MLton.HM.registerQueue (Word32.fromInt p, data) *)
    in
      { data = data
      , start = ref 0
      , frontier = ref 0
      }
    end

  fun size ({start, frontier, ...} : 'a t) = !frontier - !start
  fun empty q = (size q = 0)

  fun setDepth (q as {start, frontier, ...} : 'a t) d =
    if not (empty q) then
      raise Fail "can only set depth on empty queue"
    else
      ( start := d
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

  fun popFront ({data, start, frontier} : 'a t) =
    let
      val s = !start
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

  fun popBack ({data, start, frontier} : 'a t) =
    let
      val s = !start
      val f = !frontier
    in
      if s = f then
        NONE
      else
        let
          val result = arraySub (data, s)
        in
          start := s+1;
          arrayUpdate (data, s, NONE);
          SOME (valOf result, s)
        end
    end

  fun toList ({data, start, frontier} : 'a t) =
    let
      val sl = ArraySlice.slice (data, !start, SOME (!frontier - !start))
    in
      ArraySlice.foldr (fn (x, l) => valOf x :: l) [] sl
    end

end

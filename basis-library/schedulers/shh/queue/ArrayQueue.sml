structure ArrayQueue : QUEUE =
struct
  (* exception ArrayQueue of string *)
  (* val _ = MLton.Exn.addExnMessager (fn ArrayQueue s => SOME ("ArrayQueue \"" ^ s ^ "\"") | _ => NONE) *)

  val capacity = 128
  type 'a t = {data : 'a option array, start : int ref, frontier : int ref}

  (*fun arraySub x = Array.unsafeSub x
  fun arrayUpdate x = Array.unsafeUpdate x*)

  fun arraySub (a, i) =
    Array.sub (a, i)
    (* handle Subscript => raise ArrayQueue ("arraySub " ^ Int.toString i ^ "\n") *)
  (* fun arrayUpdate (a, i, x) =
    Array.update (a, i, x)
    handle Subscript => raise ArrayQueue ("arrayUpdate " ^ Int.toString i ^ "\n") *)

  (*val arraySub = Unsafe.Array.sub*)
  fun arrayUpdate (a, i, x) =
    MLton.HM.arrayUpdateNoBarrier (a, Int64.fromInt i, x)
  (*val arrayUpdate = MLton.HM.arrayUpdateNoBarrier*)

  fun new p =
    let
      val data = Array.array (capacity, NONE)
      (* val _ = MLton.HM.registerQueue (Word32.fromInt p, data) *)
    in
      { data = data
      , start = ref 1 (* start at 1 for silly optimization below *)
      , frontier = ref 1
      }
    end

  fun size ({start, frontier, ...} : 'a t) = !frontier - !start
  fun empty q = (size q = 0)

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

      (* a silly optimization. permitted because start/frontier bottom out
       * at 1, and always only NONEs outside [start,frontier) range. *)
      val result = arraySub (data, f-1)
      val _ = arrayUpdate (data, f-1, NONE)

      val _ =
        case f - s of
          0 => ()
        | 1 => (start := 1; frontier := 1)
        | _ => frontier := f-1
    in
      result
    end

  fun popBack ({data, start, frontier} : 'a t) =
    let
      val s = !start
      val f = !frontier

      (* a silly optimization. see comment in popFront. *)
      val result = arraySub (data, s)
      val _ = arrayUpdate (data, s, NONE)

      val _ =
        case f - s of
          0 => ()
        | 1 => (start := 1; frontier := 1)
        | _ => start := s+1
    in
      result
    end

  fun toList ({data, start, frontier} : 'a t) =
    let
      val sl = ArraySlice.slice (data, !start, SOME (!frontier - !start))
    in
      ArraySlice.foldr (fn (x, l) => valOf x :: l) [] sl
    end

end

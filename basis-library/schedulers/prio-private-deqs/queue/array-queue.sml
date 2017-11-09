functor ArrayQueue
  (Elem : sig
            type t
            val depth : t -> int
          end)
  : QUEUE where type task = Elem.t =
struct

  structure P = Potential

  type task = Elem.t
  type task_set = Elem.t list

  fun numts ts = List.length ts
  fun fromSet ts = raise Fail "ArrayQueue.fromSet"

  type t =
    { data : Elem.t list array
    , front : int ref
    , back : int ref
    , pot : P.t
    }

  val maxSize = 64

  exception Full

  fun empty () =
    { data = Array.array (maxSize, [])
    , front = ref 0
    , back = ref 0
    , pot = ref P.zero
    }

  fun isEmpty ({front, back, ...} : t) =
    !back - !front = 0

  fun size _ =
    raise Fail "ArrayQueue.size"
    
  fun linsert (ary, i, x) =
    Array.update (ary, i, x :: Array.sub (ary, i))

  (* requires depth(e) strictly larger than any depth in the queue *)
  fun push (e, {data, front, back, pot} : t) =
    let
      val de = Elem.depth e
      val back' = de + 1
      val diff = back' - !back
      val pot' = P.p (0w1, P.l (!pot, diff))
    in
      ( linsert (data, de, e)
      ; back := back'
      ; pot := pot'
      )
    end

  fun insert (e, {data, front, back, pot} : t) =
    let
      val de = Elem.depth e
      val front' = Int.min (de, !front)
      val back' = Int.max (de+1, !back)
      val diff = back' - !back
      val pot' = P.p (P.l (!pot, diff), P.fromDepth (back'-1) de)
    in
      ( linsert (data, de, e)
      ; front := front'
      ; back := back'
      ; pot := pot'
      )
    end

  fun choose ({data, front, back, pot} : t) =
    let
      val i = !back - 1
    in
      case Array.sub (data, i) of
        e :: es =>
          ( Array.update (data, i, es)
          ; SOME e
          )
      | [] =>
          

  val choose : t -> task option
  val split : t -> task_set option

end

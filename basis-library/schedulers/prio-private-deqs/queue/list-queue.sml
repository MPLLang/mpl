functor ListQueue (Elem : sig
                            type t
                            val weight : t -> int
                          end)
  :> QUEUE where type task = Elem.t =
struct

  type task = Elem.t
  type t = (int * task list) ref
  type task_set = t

  fun empty () = ref (0, [])

  fun isEmpty q =
      let val (w, es) = !q in
          case es of
              [] => true
            | _ => false
      end

  fun rev qr =
      let val (w, q) = !qr
      in
          (w, List.rev q)
      end

  fun fromSet s = s

  fun weight q =
      let val (w, es) = !q
      in
          w
      end

  fun push (q, e) =
      let val (w, es) = !q
      in
          q := (Elem.weight e + w, e :: es)
      end

  fun insert (q, e) =
      let
          val (w, es) = !q
          val we = Elem.weight e
          fun insert' front back =
              case back of
                  [] => List.revAppend (front, [e])
                | x :: back' =>
                  if we < Elem.weight x
                  then List.revAppend (front, e :: back)
                  else insert' (x :: front) back'
      in
          q := (w + we, insert' [] es)
      end

  fun choose q =
      let val (w, es) = !q
      in
          case es of
              [] => NONE
            | e :: es => (q := (w - Elem.weight e, es);
                          SOME e)
      end

  fun split q =
    let
      fun split' (wl, l) (wr, r) =
        if 4 * wl >= (wl + wr) orelse List.null r
        then ((wl, l), (wr, List.rev r))
        else let val e = List.hd r
                 val we = Elem.weight e
             in split' (wl + we, e :: l) (wr - we, List.tl r)
             end

      val (q1, q2) = split' (0, []) (rev (q))
    in
      case q1 of
        (_, []) => NONE
       | _ => (q := q2; SOME (ref q1))
    end

end

(* structure Q = ListQueue (struct type t = int fun weight x = x end)

fun fromList xs =
  case xs of
    [] => Q.empty ()
  | x :: xs' => Q.push (fromList xs') x

val q1 = fromList [1,3,8,15,15]
val (s1, q1') = Q.split q1

val q2 = fromList [1,1,100,100]
val (s2, q2') = Q.split q2 *)

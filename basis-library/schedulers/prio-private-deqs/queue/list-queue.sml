functor ListQueue (Elem : sig
                            type t
                            val depth : t -> int
                          end)
  : QUEUE where type task = Elem.t =
struct

  structure P = Potential
  type potential = P.potential
  type task = Elem.t
  type t = (potential * int * task list) ref
  type task_set = t

  fun empty () = ref (P.zero, 0, [])

  fun isEmpty q =
      let val (_, _, es) = !q in
          case es of
              [] => true
            | _ => false
      end

  fun fromSet s = s

  fun weight q =
      let val (w, _, _) = !q
      in
          w
      end

  fun push (q, e) =
      let val d = Elem.depth e
          val (w, md, es) = !q
          val md' = Int.max (d, md)
          val we = P.fromDepth md' d
          val w' = P.l (w, md' - md)
      in
          q := (P.p (w', we), md', e :: es)
      end

  fun insert (q, e) =
      let
          val (w, md, es) = !q
          val d = Elem.depth e
          val md' = Int.max (d, md)
          val we = P.fromDepth md' d
          val w' = P.l (w, md' - md)
          fun insert' front back =
              case back of
                  [] => List.revAppend (front, [e])
                | x :: back' =>
                  if d < Elem.depth x
                  then List.revAppend (front, e :: back)
                  else insert' (x :: front) back'
      in
          q := (P.p (w', we), md', insert' [] es)
      end

  fun choose q =
      let val (w, md, es) = !q
      in
          case es of
              [] => NONE
           |  e :: es => (q := (P.m (w, P.fromDepth md (Elem.depth e)), md, es);
                                SOME e)
      end

  fun split q =
    let
      fun split' (wl, mdl, l) (wr, mdr, r) =
        if P.ge (P.l (wl, 2), P.p (wl, wr)) orelse List.null r
        then ((wl, mdl, l), (wr, mdr, List.rev r))
        else let val e = List.hd r
                 val we = P.fromDepth mdl (Elem.depth e)
             in split' (P.p (wl, we), mdl, e :: l)
                       (P.m (wr, we), mdr, List.tl r)
             end
      val (w, md, es) = !q
      val (q1, q2) = split' (P.zero, md, []) (w, md, List.rev es)
    in
      case q1 of
        (_, _, []) => NONE
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

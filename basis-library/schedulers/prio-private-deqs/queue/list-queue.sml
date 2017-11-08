functor ListQueue
  (Elem : sig
            type t
            val depth : t -> int
          end)
  : QUEUE where type task = Elem.t =
struct

  structure P = Potential
  type potential = P.potential
  type task = Elem.t
  type t = task list ref
  type task_set = t

  exception Full

  fun empty () = ref []

  fun isEmpty (ref es) = List.null es

  fun fromSet s = s

  fun numts (ref l) = List.length l

  fun size (ref es) =
    case es of
      _ :: _ :: _ => 2
    | [_] => 1
    | _ => 0

  fun push (q, e) =
    q := e :: !q
  
  fun insert (q, e) =
    let
      val es = !q
      val de = Elem.depth e
      fun insert' front back =
        case back of
          [] => List.revAppend (front, [e])
        | x :: back' =>
            if de > Elem.depth x
            then List.revAppend (front, e :: back)
            else insert' (x :: front) back'
    in
      q := insert' [] es
    end

  fun choose (q as ref es) =
    case es of
      e :: es' => (q := es'; SOME e)
    | [] => NONE
  
  fun split (q as ref es) =
    let
      val maxDepth = case es of [] => 0 | e :: _ => Elem.depth e
      fun pot e = P.fromDepth maxDepth (Elem.depth e)
      val (totpot, reves) =
        List.foldl
          (fn (e, (p, es')) => (P.p (p, pot e), e :: es'))
          (P.zero, [])
          es

      fun split' (front, p) back =
        if P.ge (P.l (p, 2), totpot) orelse List.null back
        then (front, List.rev back)
        else let
               val e = List.hd back
             in
               split' (e :: front, P.p (p, pot e)) (List.tl back)
             end

      val (toSend, es') = split' ([], P.zero) reves
      val _ = q := es'
    in
      case toSend of
        [] => NONE
      | _ => SOME (ref toSend)
    end
  
  (*
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
  *)
end

(*structure Q = ListQueue (struct type t = int fun depth x = x end)

fun fromList xs =
  List.foldl (fn (x, q) => q before Q.push (q, x)) (Q.empty ()) xs*)

(*val q1 = fromList [1,3,8,15,15]
val s1 = Q.split q1

val q2 = fromList [1,1,100,100]
val s2 = Q.split q2*)

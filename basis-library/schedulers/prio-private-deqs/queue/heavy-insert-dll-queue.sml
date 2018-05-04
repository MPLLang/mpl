functor HeavyInsertDLLQueue
  (Elem : sig
            type t
            val depth : t -> int
          end)
  : QUEUE where type task = Elem.t =
struct

  structure P = Potential
  structure DLL = DoublyLinkedList

  type task = Elem.t
  type task_set = task list

  exception Full

  type t = task DLL.t
  type hand = task DLL.hand

  fun taskOfHand h = DLL.taskOfHand h

  fun fromSet s =
    let val q = DLL.new ()
    in foldl (fn (x, _) => ignore (DLL.pushBack (x, q))) () s;
       q
    end

  fun numts s = List.length s

  fun empty () =
    DLL.new ()

  fun isEmpty q = DLL.isEmpty q

  (* a hack *)
  fun size (q as (front, back)) =
    case !front of
      DLL.Node (_, _, ref (DLL.Node _), _) => 2
    | DLL.Node _ => 1
    | DLL.Leaf => 0

  fun push (q, e) = DLL.pushFront (e, q)

  fun choose q = DLL.popFront q

  fun insert (q, e) =
    case DLL.findl (fn e' => Elem.depth e' < Elem.depth e) q of
      SOME h => DLL.insertBefore (q, h) e
    | NONE => DLL.pushBack (e, q)

  fun split q =
    let
      val maxDepth =
        case DLL.peekFront q of
          SOME e => Elem.depth e
        | NONE => 0
      
      fun pot e = P.fromDepth maxDepth (Elem.depth e)
      val totpot = DLL.foldl (fn (e, p) => P.p (p, pot e)) P.zero q

      fun split' (s, p) =
        if P.ge (P.l (p, 2), totpot)
        then s
        else case DLL.popBack q of
               SOME e => split' (e :: s, P.p (p, pot e))
             | NONE => s
    in
      case split' ([], P.zero) of
        [] => NONE
      | es => SOME es
    end
  
  (*
  fun split q =
    case DLL.popBack q of
      NONE => NONE
    | SOME e => SOME [e]
  *)

  fun tryRemove (q, h) =
    if DLL.isInList h then
      (DLL.remove (q, h); true)
    else
        false

  fun app f q =
      DLL.foldlh (fn (x, h, _) => (f (x, h); ())) () q
end

(*structure Q = HeavyInsertDLLQueue (struct type t = int fun depth x = x end)*)

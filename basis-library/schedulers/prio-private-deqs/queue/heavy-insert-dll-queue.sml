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

  fun fromSet s =
    let val q = DLL.new ()
    in List.foldr (fn (e, _) => DLL.pushFront (e, q)) () s;
       q
    end

  fun numts s = List.length s

  fun empty () =
    DLL.new ()

  fun isEmpty q = DLL.isEmpty q

  (* a hack *)
  fun size (q as (front, back)) =
    case !front of
      DLL.Node (_, _, ref (DLL.Node _)) => 2
    | DLL.Node _ => 1
    | DLL.Leaf => 0

  fun push (q, e) = DLL.pushFront (e, q) 

  fun choose q = DLL.popFront q

  fun insert (q, e) =
    ignore (DLL.insertBefore (q, DLL.findl (fn e' => Elem.depth e' < Elem.depth e) q) e)

  fun split q =
    case DLL.popBack q of
      SOME e => SOME [e]
    | NONE => NONE

end

(*structure Q = HeavyInsertDLLQueue (struct type t = int fun depth x = x end)*)

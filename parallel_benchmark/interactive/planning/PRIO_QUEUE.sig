signature PQ_EL =
sig
    type t
    val compare : t * t -> bool
end

signature PRIO_QUEUE =
sig
    type elt
    type pqueue
    type token

    exception EmptyHeap

    val empty : pqueue
    val is_empty : pqueue -> bool

    val find_min : pqueue -> elt
    val merge : pqueue -> pqueue -> pqueue
    val insert : pqueue -> elt -> pqueue * token
    val delete_min : pqueue -> pqueue * elt
    (* decrease_key is destructive even though it returns a pqueue *)
    val decrease_key : pqueue -> token -> elt -> pqueue
end

functor WorkQueue (W : sig type work val numberOfProcessors : unit -> int end) 
  : PARALLEL_WORKQUEUE =
struct
  structure WS = WorkStealingTS (structure W = W)
  open WS
end

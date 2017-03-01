structure Policy : POLICY =
struct
  val suspendEntireQueues = false
  val stealOldestFromSelf = true
  val resumeWorkLocally = true
  val stealEntireQueues = false (* NA *)
  val stealFromSuspendedQueues = false (* NA *)
  val policyName = "ws8"
end

functor WorkQueue (W : sig type work val numberOfProcessors : unit -> int end) 
  : PARALLEL_WORKQUEUE =
struct
  structure WS = WorkStealing (structure W = W
                               structure P = Policy)
  open WS
end

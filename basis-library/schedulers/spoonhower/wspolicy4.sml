structure Policy : POLICY =
struct
  val suspendEntireQueues = true
  val stealOldestFromSelf = false (* NA *)
  val resumeWorkLocally = false (* NA *)
  val stealFromSuspendedQueues = true
  val stealEntireQueues = true
  val policyName = "ws4"
end

functor WorkQueue (W : sig type work val numberOfProcessors : unit -> int end) 
  : PARALLEL_WORKQUEUE =
struct
  structure WS = WorkStealing (structure W = W
                               structure P = Policy)
  open WS
end

signature MLTON_PARALLEL_BASIC =
sig

  (* the empty type *)
  type void
  (* the type of a parallel task -- work never returns normally *)
  type work = unit -> void
  (* uniquely identifies a job in the queue *)
  type token
  (* a suspended computation waiting for a value of type 'a *)
  type 'a t

  (* Suspends the current thread of execution and runs the given function,
    passing in a reified form of the original thread.  The given function must
    not call any other function in this signature. *)
  val suspend : ('a t -> unit) -> 'a
  (* Re-enable a suspended computation and continue with the current job *)
  val resume : 'a t * 'a -> unit
  (* similar to suspend, but doesn't maintain the priority associated with this job *)
  val capture : ('a t -> unit) -> 'a

  (* Add the given work to the queue; the work is consider to have lower
    priority than the current task.  Returns a token that may be used to
    remove this task from the scheduling queue.  This operation may suspend
    under some scheduling policies. *)
  (* "Right" adds to the right of the current task -- lower priority than the
    current task *)
  val addRight : work -> token
  (* "Left" adds to the left of the current task -- higher priority than the
    current task *)
  val addLeft : work -> token
  (* Remove a task from the work queue if no processor has yet started work on
    it.  Returns true if the task was successfully removed and, we are
    guaranteed that no other processor will execute that task.  At most call
    to remove work (for a given task) will return true. *)
  val remove : token -> bool

  (* Adds to the queue after the current task has finished.  (Finished is
    defined as the next time this task suspends: possibly one of suspend,
    capture, addRight, addLift, return, or yield.) This operation never
    suspend the current task. *)
  val delayedAdd : work -> unit

  (* End the current task *)
  val return : unit -> void

  (* Temporarily yield, but continue as scheduling policy permits. *)
  val yield : unit -> unit

  (* general errors related to parallelism *)
  exception Parallel of string

  (* informational *)
  (*val policyName : string
  val numberOfProcessors : int
  val maxBytesLive : unit -> Word64.word
  val gcTime : unit -> Word64.word
  val successfulSteals : unit -> int
  val failedSteals : unit -> int
  val suspends : unit -> int
  val resetStatistics : unit -> unit*)

end

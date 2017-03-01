signature MLTON_PARALLEL_BASIC =
sig

  (* the empty type *)
  type void
  (* the type of a parallel task -- work never returns normally *)
  type work = unit -> void
  (* uniquely identifies a job in the queue *)
  type token
  (* a suspended computation waiting for a value of type 'a *)
  type t

  (* Re-enable a suspended computation and continue with the current job *)
  val resume : t -> unit
  (* similar to suspend, but doesn't maintain the priority associated with this job *)
  val capture : (t -> unit) -> unit

  (* Add the given work to the queue; the work is consider to have lower
    priority than the current task.  Returns a token that may be used to
    remove this task from the scheduling queue.  This operation may suspend
    under some scheduling policies. *)
  (* "Right" adds to the right of the current task -- lower priority than the
    current task *)
  val addRight : work * int -> token
  (* RAM_NOTE: Disabled until reintegrated *)
  (* (* "Left" adds to the left of the current task -- higher priority than the *)
  (*   current task *) *)
  (* val addLeft : work -> token *)
  (* Remove a task from the work queue if no processor has yet started work on
    it.  Returns true if the task was successfully removed and, we are
    guaranteed that no other processor will execute that task.  At most call
    to remove work (for a given task) will return true. *)
  val remove : token -> bool

  (* End the current task *)
  val return : unit -> void

  (* general errors related to parallelism *)
  exception Parallel of string

  (* informational *)
  val policyName : string
  val numberOfProcessors : int
  val successfulSteals : unit -> int
  val failedSteals : unit -> int
  val suspends : unit -> int
end

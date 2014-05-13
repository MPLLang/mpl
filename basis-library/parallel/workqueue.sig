signature PARALLEL_WORKQUEUE =
sig

  (* processor identifier *)
  type proc = int
  (* abstract type of work *)
  type work
  (* identifies a piece of work *)
  type token
  type susp

  (* these take the identifier of the current processor as their first
   argument *)

  (* create a new identifier for work *)
  val newWork : proc -> token
  (* atomically add new work to the queue; highest-priority work appears first *)
  val addWork : proc * (token * work) list -> unit

  (* remove the next, highest priority work.  the boolean indicates whether
   this is a "non-local" (i.e. stolen) job. *)
  val getWork : proc -> (bool * work) option
  (* begin work -- added jobs should be considered children *)
  val startWork : proc -> unit
  (* mark the most recent unit of work as done *)
  val finishWork : proc -> unit

  (* instead of 'finish', users may also call 'suspend' temporarily ending
    execution of a unit of work *)
  val suspendWork : proc -> susp
  (* mark suspended job as available -- it will then be returned by some
   future 'get' *)
  val resumeWork : proc * susp * (token * work) -> unit

  (* removes a piece of work from the queue if it hasn't already been returned
    by getWork, work can only be removed after it has been added. *)
  val removeWork : proc * token -> bool

  (* is there higher priority work for the given processor? *)
  val shouldYield : proc -> bool 

  (* name of the current policy *)
  val policyName : string

  val reportSuccessfulSteals : unit -> int
  val reportFailedSteals : unit -> int
  val resetSteals : unit -> unit

end

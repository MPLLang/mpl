signature PARALLEL_WORKQUEUE =
sig

  (* processor identifier *)
  type proc = int
  (* abstract type of work *)
  type work
  (* identifies a piece of work *)
  type token
  type susp
  type unlocker = unit -> unit

  (* these take the identifier of the current processor as their first
   argument *)

  (* atomically add new work to the queue; highest-priority work appears first *)
  val addWork : proc * work -> token

  (* remove the next, highest priority work.  the boolean indicates whether
   this is a "non-local" (i.e. stolen) job. *)
  val getWork : proc -> (bool * unlocker * work) option

  (* removes a piece of work from the queue if it hasn't already been returned
    by getWork, work can only be removed after it has been added. *)
  val removeWork : proc * token -> bool

  val reportSuccessfulSteals : unit -> int
  val reportFailedSteals : unit -> int
  val resetSteals : unit -> unit
end

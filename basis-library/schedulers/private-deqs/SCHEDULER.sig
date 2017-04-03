signature SCHEDULER =
sig
  structure ForkJoin : FORK_JOIN
end

(*signature SCHEDULER =
sig

  type vertex
  type task = unit -> unit

  val new : unit -> vertex

  (* `push (t, v)` registers t as a dependency for v and pushes t onto the work
   * stack. v should be thought of as a join vertex which has not yet been
   * assigned any computation. We use `sync v` to assign some computation to v
   * (specifically, the current continuation) and also wait for dependencies to
   * complete. *)
  val push : task * vertex -> unit

  (* Attempt to pop a task off the task stack. If it fails (because the task
   * stack is empty) then the desired task must have been stolen.
   * `popDiscard` throws away the task, and only returns success. *)
  (*val pop : unit -> task option*)
  val popDiscard : unit -> bool

  (* `sync v` assigns the current continuation to v, waits until all stolen
   * dependencies for v have completed, and then executes v.
   * NOTE: it really only makes sense to call `sync` if the task stack is
   * empty. *)
  val sync : vertex -> unit

end*)

(* NOTE: The semantics of these functions would be easier to describe if each
 * vertex had its own task stack. Then `sync v` would execute all dependencies
 * of v, which are either stolen (and so the incounter is appropriately
 * incremented) or stored locally with v.
 *
 * ...but then, would pop give a single task, or a single vertex?
 *)

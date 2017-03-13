(* TODO: what exactly are the semantics of `sync`? What happens if we create
 * a new t, pass it to a different processor, and sync it there? This is
 * definitely bad... is it broken? *)
signature SCHEDULER =
sig

  type t
  type task = unit -> unit

  (* Create a new join point. *)
  val new : unit -> t

  (* Register a task with a join point, and push it onto the task stack. *)
  val push : t -> task -> unit

  (* Attempt to pop a task off the task stack. If it fails (because the task
   * stack is empty) then the desired task must have been stolen.
   * `popDiscard` throws away the task, and only returns success. *)
  val pop : unit -> task option
  val popDiscard : unit -> bool

  (* Wait for all registered tasks to complete. *)
  val sync : t -> unit

end

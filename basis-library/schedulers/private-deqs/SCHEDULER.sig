(* TODO: what exactly are the semantics of `sync`? What happens if we create
 * a new t, pass it to a different processor, and sync it there? This is
 * definitely bad... is it broken? *)
signature SCHEDULER =
sig

  type vertex
  type task = unit -> unit

  val new : unit -> vertex

  (* `push v t` registers v as an outgoing dependency of t and pushes t onto
   * the work stack *)
  val push : vertex -> task -> unit

  (* Attempt to pop a task off the task stack. If it fails (because the task
   * stack is empty) then the desired task must have been stolen.
   * `popDiscard` throws away the task, and only returns success. *)
  val pop : unit -> task option
  val popDiscard : unit -> bool

  (* `sync v` assigns the current continuation to v, waits until all incoming
   * dependences for v have completed, and then executes v. *)
  val sync : vertex -> unit

end

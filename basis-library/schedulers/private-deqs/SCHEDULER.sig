(* TODO: what exactly are the semantics of `sync`? What happens if we create
 * a new t, pass it to a different processor, and sync it there? This is
 * definitely bad... is it broken? *)
signature SCHEDULER =
sig

  type t
  type work = unit -> unit

  (* Create a new join point. *)
  val new : unit -> t

  (* Register a piece of work with a join point, and push the work onto the
   * work stack. *)
  val push : t * work -> unit

  (* Attempt to pop a thunk off the work stack. If it fails (because the work
   * stack is empty) then the desired piece of work must have been stolen.
   * `popDiscard` throws away the piece of work, and only returns success. *)
  val pop : unit -> work option
  val popDiscard : unit -> bool

  (* Wait for all registered work to complete. *)
  val sync : t -> unit

end

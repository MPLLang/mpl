signature SCHEDULER =
sig
  (* general errors related to parallelism *)
  exception Parallel of string

  (* a handle on an exposed thunk *)
  type 'a t

  (* push a thunk onto the work stack *)
  val push : (unit -> 'a) -> 'a t

  (* attempt to pop a thunk off the work stack; if it fails, then
   * the desired piece of work must have been stolen. *)
  val pop : unit -> bool

  (* wait for a stolen piece of work to complete *)
  val sync : 'a t -> 'a

end

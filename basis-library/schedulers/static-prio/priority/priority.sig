signature PRIORITY =
sig

    exception InvariantViolated of string
    exception Uninitialized
    exception AlreadyInitialized

    type t

    (* Functions must be called in this order:
     * new (any number of times)
     * init
     * new_lessthan (any number of times)
     * check
     *
     * After calling check, functions may be called in this order:
     * ( (new*, init)* + (new_lessthan*, check)* )*
     *)

    val count : unit -> int

    val init : unit -> unit
    val check : unit -> unit

    val new : unit -> t
    val new_lessthan : t -> t -> unit

    val ple : t * t -> bool
    val plt : t * t -> bool
    val pe : t * t -> bool

    (* Returns the "next" lower priority. next bot returns top. *)
    val next : t -> t

    val toInt : t -> int (* Returns a positive integer unique to a priority *)
    val fromInt : int -> t
    val toString : t -> string

    val installDist : (t -> int) -> unit
    val chooseFromDist : real (* unit *) -> t

    val top : unit -> t
    val bot : t

end

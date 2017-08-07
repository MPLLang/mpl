signature WORLD =
sig

    type t

    val init : unit -> t

    val dim : t -> int * int (* (width, height) *)
    val map : t -> graph
    val pts : t -> (int * int) Vector.vector
    val obst : t -> obst ChunkedTreeSequence.seq

    val pos : t -> int * int
    (* (delta-x, delta-y) *)
    val move : t -> int * int -> t

    val time : t -> int

    (* Mostly for visualization *)
    val register_short_plan : t -> (int * int) list -> t
    val register_long_plan : t -> (int * int) list -> t

end

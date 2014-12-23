signature RUNTIME_INFO =
sig
    type t

    val new: unit -> t
    val get: unit -> t
    val set: t -> unit
end

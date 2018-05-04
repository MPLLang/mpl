signature THREAD =
sig

exception IncompatiblePriorities

type 'a t
val spawn : (unit -> 'a) -> Priority.t -> 'a t
val sync : 'a t -> 'a
val poll : 'a t -> 'a option

val fork : (unit -> 'a) * (unit -> 'b) -> 'a * 'b

val cancel : 'a t -> unit

end

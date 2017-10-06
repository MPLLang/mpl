signature INTERRUPT =
sig

    type handler = int * MLton.Thread.Runnable.t -> MLton.Thread.Runnable.t

    val init : handler -> Time.time -> unit

    val block : int -> unit
    val unblock : int -> unit
    val atomically : int -> ('a -> 'b) -> 'a -> 'b

end

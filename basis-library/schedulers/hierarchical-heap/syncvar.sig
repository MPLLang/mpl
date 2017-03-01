signature MLTON_PARALLEL_SYNCVAR =
sig

  type 'a t

  val empty : unit -> 'a t

  val write : 'a t * 'a -> unit

  (* true if read led to a suspension *)
  val read : 'a t -> bool * 'a

end

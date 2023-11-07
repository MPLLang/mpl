signature FORK_JOIN =
sig
  val par: (unit -> 'a) * (unit -> 'b) -> 'a * 'b
  val parfor: int -> int * int -> (int -> unit) -> unit

  val alloc: int -> 'a array

  (* synonym for par *)
  val fork: (unit -> 'a) * (unit -> 'b) -> 'a * 'b

  val idleTimeSoFar: unit -> Time.time
  val workTimeSoFar: unit -> Time.time
  val maxForkDepthSoFar: unit -> int
end

signature SPORK_JOIN =
sig
  (* synonym for par *)
  val fork: (unit -> 'a) * (unit -> 'b) -> 'a * 'b 

  val par: (unit -> 'a) * (unit -> 'b) -> 'a * 'b
  val spork: (unit -> 'a) * (unit -> 'b) * ('a -> 'c) * ('a * 'b -> 'c) -> 'c
  val parfor: int -> (int * int) -> (int -> unit) -> unit
  val pareduce: int * int * 'a * (int * 'a -> 'a) * ('a * 'a -> 'a) -> 'a
  val pareduce': int * int * 'a * (int -> 'a) * ('a * 'a -> 'a) -> 'a
  val alloc: int -> 'a array

  val idleTimeSoFar: unit -> Time.time
  val workTimeSoFar: unit -> Time.time
  val maxForkDepthSoFar: unit -> int
end

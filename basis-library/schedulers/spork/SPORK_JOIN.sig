signature SPORK_JOIN =
sig
  val par: (unit -> 'a) * (unit -> 'b) -> 'a * 'b
  val spork: (unit -> 'a) * (unit -> 'b) * ('a -> 'c) * ('a * 'b -> 'c) -> 'c
  val pareduce: int * int * 'a * (int * 'a -> 'a) * ('a * 'a -> 'a) -> 'a
  val pareduce': int * int * 'a * (int -> 'a) * ('a * 'a -> 'a) -> 'a
  val alloc: int -> 'a array

  val idleTimeSoFar: unit -> Time.time
  val workTimeSoFar: unit -> Time.time
  val maxForkDepthSoFar: unit -> int
end

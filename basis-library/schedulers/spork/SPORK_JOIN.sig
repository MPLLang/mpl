signature SPORK_JOIN =
sig
  (* synonym for par *)
  val fork: (unit -> 'a) * (unit -> 'b) -> 'a * 'b 
  val par: (unit -> 'a) * (unit -> 'b) -> 'a * 'b
  val sporkFair: {body: unit -> 'a, spwn: unit -> 'b, seq: 'a -> 'c, sync: 'a * 'b -> 'c} -> 'c
  val sporkKeep: {body: unit -> 'a, spwn: unit -> 'b, seq: 'a -> 'c, sync: 'a * 'b -> 'c} -> 'c
  val sporkGive: {body: unit -> 'a, spwn: unit -> 'b, seq: 'a -> 'c, sync: 'a * 'b -> 'c} -> 'c
  val sporkFair': {body: unit -> 'a, spwn: unit -> 'b, seq: 'a -> 'c, sync: 'a * 'b -> 'c, unstolen: 'a -> 'c} -> 'c
  val sporkKeep': {body: unit -> 'a, spwn: unit -> 'b, seq: 'a -> 'c, sync: 'a * 'b -> 'c, unstolen: 'a -> 'c} -> 'c
  val sporkGive': {body: unit -> 'a, spwn: unit -> 'b, seq: 'a -> 'c, sync: 'a * 'b -> 'c, unstolen: 'a -> 'c} -> 'c

  val pareduce: (int * int) -> 'a -> (int * 'a -> 'a) -> ('a * 'a -> 'a) -> 'a
  val pareduceSplit: (int * int) -> 'a -> (int * 'a -> 'a) -> ('a * 'a -> 'a) -> 'a
  val parfor: int -> (int * int) -> (int -> unit) -> unit
  val alloc: int -> 'a array

  val idleTimeSoFar: unit -> Time.time
  val workTimeSoFar: unit -> Time.time
  val maxForkDepthSoFar: unit -> int
end
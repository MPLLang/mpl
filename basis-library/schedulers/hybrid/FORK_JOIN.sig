signature FORK_JOIN =
sig
  val par: (unit -> 'a) * (unit -> 'b) -> 'a * 'b
  val parfor: int -> int * int -> (int -> unit) -> unit

  val choice: {prefer_cpu: unit -> 'a, prefer_gpu: unit -> 'a} -> 'a

  val alloc: int -> 'a array

  (* synonym for par *)
  val fork: (unit -> 'a) * (unit -> 'b) -> 'a * 'b

  (* other scheduler hooks *)
  val communicate: unit -> unit
  val getIdleTime: int -> Time.time

  val maxForkDepthSoFar: unit -> int
end

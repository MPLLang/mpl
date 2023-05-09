signature FORK_JOIN =
sig
  val par: (unit -> 'a) * (unit -> 'b) -> 'a * 'b
  val parfor: int -> int * int -> (int -> unit) -> unit

  (* intermediate type 'package, final type 'result *)
  type ('package, 'result) gpu_task

  val gpu: 
    { spawn: unit -> 'package
    , poll: 'package -> bool
    , finish: 'package -> 'result
    }
    -> ('package, 'result) gpu_task

  val gpuWithCleanup: 
    { spawn: unit -> 'package
    , poll: 'package -> bool
    , finish: 'package -> 'a
    , cleanup: 'a -> 'result
    }
    -> ('package, 'result) gpu_task

  val choice: {cpu: unit -> 'a, gpu: ('b, 'a) gpu_task} -> 'a

  val alloc: int -> 'a array

  (* synonym for par *)
  val fork: (unit -> 'a) * (unit -> 'b) -> 'a * 'b

  (* other scheduler hooks *)
  val communicate: unit -> unit
  val getIdleTime: int -> Time.time

  val maxForkDepthSoFar: unit -> int
end

structure SpinLock :>
sig
  type t
  val new: unit -> t
  val trylock: t -> bool
  val lock: t -> unit
  val unlock: t -> unit
end =
struct

  fun die msg =
    let
      val me = MLton.Parallel.processorNumber ()
      val msg' = "[worker " ^ Int.toString me ^ "] ERROR: " ^ msg ^ "\n"
    in
      TextIO.output (TextIO.stdErr, msg');
      OS.Process.exit OS.Process.failure
    end

  (* ref stores the worker id of its owner *)
  type t = int ref

  fun new () = ref (~1)

  fun trylock r =
    let
      val me = MLton.Parallel.processorNumber ()
      val owner = !r
    in
      if owner = me then
        die "tried to take lock already owned by itself"
      else
        (owner < 0)
        andalso
        owner = MLton.Parallel.compareAndSwap r (owner, me)
    end

  fun lock r =
    if trylock r then () else lock r

  fun unlock r =
    let
      val me = MLton.Parallel.processorNumber ()
      val owner = !r
    in
      if owner <> me then
        die "tried to unlock a lock it does not hold"
      else
        r := ~1
    end

end

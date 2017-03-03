functor WorkQueue (W : sig type work val numberOfProcessors : unit -> int end) : PARALLEL_WORKQUEUE =
struct

  type proc = int
  type work = W.work
  type token = int ref

  local
    val lockInit = _import "Parallel_lockInit" runtime private: int ref -> int ref;
    val thelock = lockInit (ref 0)
    val lockTake = _import "Parallel_lockTake" runtime private: int ref -> unit;
    val lockRelease = _import "Parallel_lockRelease" runtime private: int ref -> unit;
  in
  fun lock () = lockTake thelock
  fun unlock () = lockRelease thelock
  end

  val fetchAndAdd = _import "Parallel_fetchAndAdd" runtime private: Int32.int ref * Int32.int -> Int32.int;

  (* initialize state *)
  val queue = ref nil : (token * work) list ref

  fun newWork _ = ref 0

  fun addWork (_, tws) =
    let in
      lock ();
      queue := tws @ (!queue);
      unlock ()
    end

  fun getWork _ =
    let
      fun loop () =
          case !queue
           of nil => NONE
            | (t, w)::ws =>
              let in
                queue := ws;
                if fetchAndAdd (t, 2) = 0 then
                  SOME w
                else
                  loop ()
              end
    in
      lock ();
      loop ()
      before unlock ()
    end

  fun finishWork _ = ()

  fun removeWork (_, t) =
      if fetchAndAdd (t, 1) = 0 then true else false

  fun shouldYield _ = true

  val policyName = "sim"

end

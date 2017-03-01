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

  datatype 'a mlist =
     Cons of 'a * 'a mlist ref
   | Nil

  (* initialize state *)
  val (head, tail) = (ref Nil, ref Nil)

  fun newWork _ = ref 0

  fun addWork (_, tws) =
    let
      fun add (c, w) =
          tail := (case !tail of
                     Cons (_, r) =>
                     let
                       val n = Cons ((w, c), ref (!r))
                       val () = r := n
                     in
                       n
                     end
                   | Nil =>
                     let
                       val n = Cons ((w, c), ref Nil)
                       val () = head := n
                     in
                       n
                     end)
    in
      lock ();
      app add tws
      before unlock ()
    end

  fun getWork _ =
    let
      fun loop () =
          case !head
           of Nil => NONE
            | Cons ((w, c), t) =>
              let in
                (* first remove from the queue *)
                head := !t;
                case !t of Nil => tail := Nil
                         | _ => ();
                if fetchAndAdd (c, 2) = 0 then
                  SOME w
                else (* cancelled *)
                  loop ()
              end
    in
      lock ();
      loop ()
      before unlock ()
    end

  fun startWork _ = ()

  fun finishWork _ = ()

  fun removeWork (_, c) =
      if fetchAndAdd (c, 1) = 0 then true else false

  fun shouldYield _ = true

  val policyName = "pbf"

end

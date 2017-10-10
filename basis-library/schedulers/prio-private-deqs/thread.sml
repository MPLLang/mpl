structure Thread :> THREAD =
struct

structure T = MLton.Thread
structure P = Priority
structure Bag = ListBag

exception Thread
exception IncompatiblePriorities

datatype 'a result =
         Waiting
         | Finished of 'a
         | Raised of exn

fun writeResult fr f () =
    fr := (Finished (f ()) handle e => Raised e)

type 'a t = 'a result ref * P.t * (P.t * Task.t) Bag.t

fun run (fr, _, bag) f () =
    ( writeResult fr f ();
      (case Bag.dump bag of
           NONE => raise Thread
         | SOME l => List.app push l);
      returnToSched ()
    )

fun spawn f r' =
    let
        val p = processorNumber ()
        val r = curPrio p
        val td = (ref Waiting, r, Bag.new ())
        val task = newTask (Task.Thunk (run td f))
    in
        (if P.pe (r, r') then push else insert) (r', task);
        td
    end

fun poll (result, _, bag) =
    if not (Bag.isDumped bag) then NONE
    else case !result of
             Finished x => SOME x
           | Raised e => raise e
           | Waiting => raise Thread

fun sync (result, r', bag) =
    let val p = processorNumber ()
        val r = curPrio p
        val _ = if P.ple (r, r') then ()
                else
                    raise IncompatiblePriorities
        fun f rt =
            if Bag.insert (bag, rt) then
                (* Successfully waiting on the thread *)
                ()
            else
        (* Bag was just dumped, so we can directly add the task *)
                push rt
        val _ = if Bag.isDumped bag then ()
                else suspend f
    in
        case !result of
            Finished x => x
          | Raised e => raise e
          | Waiting => raise Thread
    end

end

structure Priority = Priority
structure IO = IO

structure Basic =
struct
val init = init
val finalizePriorities = finalizePriorities
fun currentPrio () = curPrio (processorNumber ())
val suspend = suspend
val suspend = suspendIO
end

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

type 'a t = 'a result ref * P.t * (P.t * Task.t) Bag.t * Q.hand

fun run (fr, bag) f () =
    ( writeResult fr f ();
      (case Bag.dump bag of
           NONE => raise Thread
         | SOME l => List.app (ignore o push) l);
      returnToSched ()
    )

fun spawn f r' =
    let
        val p = processorNumber ()
        val r = curPrio p
        val fr = ref Waiting
        val bag = Bag.new ()
        val task = newTask (Task.Thunk (run (fr, bag) f))
        val hand = (if P.pe (r, r') then push else insert) (r', task)
    in
        (fr, r, bag, hand)
    end

fun poll (result, _, bag, _) =
    if not (Bag.isDumped bag) then NONE
    else case !result of
             Finished x => SOME x
           | Raised e => raise e
           | Waiting => raise Thread

fun sync (result, r', bag, _) =
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
                ignore (push rt)
        val _ = if Bag.isDumped bag then ()
                else suspend f
    in
        case !result of
            Finished x => x
          | Raised e => raise e
          | Waiting => raise Thread
    end

fun fork (f, g) =
    let val r = curPrio (processorNumber ())
        val (gt as (_, _, _, h)) = spawn g r
        val fr = f ()
        val gr = if tryRemove (r, h) then
                     g ()
                 else
                     sync gt
    in
        (fr, gr)
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

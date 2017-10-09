structure Thread =
struct

structure T = MLton.Thread
structure P = Priority

exception Thread
exception IncompatiblePriorities

datatype 'a result =
         Waiting
         | Finished of 'a
         | Raised of exn

fun writeResult fr f () =
    fr := (Finished (f ()) handle e => Raised e)

type 'a t = 'a result ref * P.t * (P.t * Task.t) Bag.t

fun run (fr, bag) f () =
    ( writeResult fr f ();
      returnToSched ()
    )

fun spawn f r' =
    let
        val p = processorNumber ()
        val r = curPrio p
        val td = (ref Waiting, r, Bag.new ())
        val task = newTask (Thunk f)
    in
        (if P.pe (r, r') then push else insert) (r, t);
        td
    end

fun poll (result, _, bag) =
    if not (Bag.isDumped bag) then NONE
    else case !result of
             Finished x => SOME x
           | Raised e => raise e
           | Waiting => raise Thread

fun suspend' (bag, k) () =
    if Bag.insert (bag, k)
    then suspend ()
    else T.switch (fn _ => runnable k)

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
structure IO = IOSDL

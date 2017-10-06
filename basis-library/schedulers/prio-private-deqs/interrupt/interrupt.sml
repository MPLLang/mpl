structure Interrupt :> INTERRUPT =
struct

type handler = MLton.Thread.Runnable.t -> MLton.Thread.Runnable.t
structure P = MLton.Parallel

val procsInit = ref 0

val prim_sig = MLton.Itimer.signal MLton.Itimer.Real
val sec_sig = Posix.Signal.usr1

val numberOfProcessors = MLton.Parallel.numberOfProcessors
val signalThread = _import "signal_thread" runtime private: int * SysWord.word -> unit;

val inCriticalSection = Array.array (numberOfProcessors, false)

fun signalOthers p sg n =
      if n = numberOfProcessors then ()
      else if n = p then signalOthers p sg (n + 1)
      else
          (signalThread (n, Posix.Signal.toWord sg);
           signalOthers p sg (n + 1))

fun interruptFst iv handler k =
    let val p = processorNumber ()
    in
        signalOthers p sec_sig 0;
        MLton.Itimer.set (MLton.Itimer.Real,
                          {interval = Time.zeroTime, value = iv});
        (if Array.sub (inCriticalSection, p) then k
         else
             handler (p, k))
    end

fun interrupt handler k =
    let val p = processorNumber ()
    in
        if Array.sub (inCriticalSection, p) then k
        else
            handler (p, k)
    end

fun init (handler: handler) (interval: Time.time) =
    let val p = P.processorNumber ()
        val pi = P.fetchAndAdd (procsInit, 1)
    in
        S.setHandler (prim_sig, (S.Handler.handler
                                     (interruptFst interval handler)));
        S.setHandler (sec_sig, (S.Handler.handler
                                    (interrupt handler)));
        (if p = 0 then
             (
               S.Mask.unblock (S.Mask.some [prim_sig]);
               S.Mask.block (S.Mask.some [sec_sig]);
             )
         else
             (
               S.Mask.block (S.Mask.some [prim_sig]);
               S.Mask.unblock (S.Mask.some [sec_sig])
             )
        );
        (if pi = P - 1 then
             MLton.Itimer.set (MLton.Itimer.Real,
                               {interval = Time.zeroTime, value = interval})
         else
             ())
    end

fun block p =
    Array.update (inCriticalSection, p, true)

fun unblock p =
    Array.update (inCriticalSection, p, false)

fun atomically p f a =
    (block p;
     f a
     before (unblock p))

end

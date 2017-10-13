structure Real : REAL =
struct

open Real

val realLock = ref 0
fun lockMutex m =
    if !m <> 0 then
        lockMutex m
    else
        if MLton.Parallel.compareAndSwap m (0, 1) = 0 then
            ()
        else
            lockMutex m
fun unlockMutex m = m := 0

fun fromLargeInt l =
    (lockMutex realLock;
     Real.fromLargeInt l
     before unlockMutex realLock)
end

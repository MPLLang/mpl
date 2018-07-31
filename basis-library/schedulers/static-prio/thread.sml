structure Thread : THREAD =
struct

structure F = MLtonParallelFutureSuspend
structure B = MLtonParallelBasic

exception IncompatiblePriorities (* Never actually raised *)

type 'a t = 'a F.t

fun spawn f prio = F.futurePrio (prio, f)
fun sync t = F.touch t
fun fork (f, g) = MLtonParallelForkJoin.fork (f, g)

end

structure Priority = Priority
structure Basic = MLtonParallelBasic
structure IO = IO

structure Atomic :> ATOMIC =
struct
  val lockInit = MLton.Parallel.Deprecated.lockInit
  val takeLock = MLton.Parallel.Deprecated.takeLock
  val releaseLock = MLton.Parallel.Deprecated.releaseLock

  val printLock = let val x = ref 0w0 in (lockInit x; x) end
  val print = fn f => (takeLock printLock; print (f ()); releaseLock printLock)

  type 'a t = Word32.word ref * 'a ref
  fun new x = (let val lock = ref 0w0 in (lockInit lock; lock) end, ref x)
  fun write ((lock, r), x) = (takeLock lock; r := x; releaseLock lock)
  fun read (lock, r) =
    (takeLock lock; let val x = !r in (releaseLock lock; x) end)

(*
  type 'a t = 'a ref
  fun new x = ref x
  fun write (r, x) = r := x
  fun read r = !r
*)

end

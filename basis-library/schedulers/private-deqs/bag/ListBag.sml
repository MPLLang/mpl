structure ListBag :> BAG =
struct

  exception ListBag

  val vcas = MLton.Parallel.compareAndSwap
  fun cas (r, old, new) = vcas r (old, new) = old

  val UNLOCKED = 0
  val LOCKED = 1
  val DUMPED = 2

  type 'a t = int ref * 'a list ref

  fun new () = (ref UNLOCKED, ref [])

  (* Atomically transition r from UNLOCKED to desired (either LOCKED or DUMPED),
   * indicating success. Taking the lock only fails if someone else managed to
   * transition the lock to DUMPED. *)
  fun takeLock (r, desired) =
    let val status = !r
    in (status <> DUMPED) andalso
       ( (status = UNLOCKED andalso cas (r, UNLOCKED, desired))
         orelse
         takeLock (r, desired)
       )
    end

  fun releaseLock r =
    if cas (r, LOCKED, UNLOCKED) then ()
    else raise ListBag

  fun insert ((lock, xs), x) =
    takeLock (lock, LOCKED)
    andalso ( xs := (x :: !xs)
            (*; print ("Inserted elem\n")*)
            ; releaseLock lock
            ; true
            )

  fun dump (lock, xs) =
    if takeLock (lock, DUMPED) then SOME (!xs) else NONE

  fun isDumped (lock, _) = (!lock = DUMPED)

end

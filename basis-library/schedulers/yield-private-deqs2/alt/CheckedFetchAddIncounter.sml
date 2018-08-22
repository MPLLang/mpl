(* This version includes error checking (to make sure a handle is never
 * decremented twice). *)
structure FetchAddIncounter :> INCOUNTER where type t = int ref =
struct
  type t = int ref

  (* second entry tells us whether or not h has been decremented already *)
  type h = t * int ref

  exception Decrement

  fun new () = ref 0

  fun poll (ref x) = x

  fun decrement (c, dr) =
    if MLtonParallelAtomic.compareAndSwap (dr, 0, 1)
    then MLtonParallelAtomic.fetchAndAdd (c, ~1) = 1
    else raise Decrement

  fun increment c =
    ( ignore (MLtonParallelAtomic.fetchAndAdd (c, 1))
    ; (c, ref 0)
    )

end

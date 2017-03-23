structure FetchAddIncounter :> INCOUNTER where type t = int ref =
struct
  type t = int ref
  type h = t

  exception Decrement

  fun new () = ref 0

  fun new2 () =
    let val c = ref 2 in (c, c) end

  fun poll (ref x) = x

  fun decrement c =
    MLtonParallelAtomic.fetchAndAdd (c, ~1) = 1

  fun increment c =
    ( ignore (MLtonParallelAtomic.fetchAndAdd (c, 1))
    ; c
    )

end

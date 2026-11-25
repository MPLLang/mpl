structure Concurrency =
struct
  open Concurrency

  fun faaArray (a, i) x =
    let
      val rx = Array.sub (a, i)
    in
      (Array.update (a, i, rx + x); rx)
    end
end

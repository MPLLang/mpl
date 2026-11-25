structure Concurrency =
struct
  open Concurrency
  val faaArray = MLton.Parallel.arrayFetchAndAdd
end

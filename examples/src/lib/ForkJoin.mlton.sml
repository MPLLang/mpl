structure ForkJoin:
sig
  val par: (unit -> 'a) * (unit -> 'b) -> 'a * 'b
  val parfor: int -> int * int -> (int -> unit) -> unit
  val alloc: int -> 'a array
end =
struct
  fun par (f, g) = (f (), g ())
  fun parfor (g:int) (lo, hi) (f: int -> unit) =
    if lo >= hi then () else (f lo; parfor g (lo+1, hi) f)
  fun alloc n = ArrayExtra.alloc n
end

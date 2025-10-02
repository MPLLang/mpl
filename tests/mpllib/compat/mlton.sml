structure ForkJoin:
sig
  val par: (unit -> 'a) * (unit -> 'b) -> 'a * 'b
  val parfor: int -> int * int -> (int -> unit) -> unit
  val alloc: int -> 'a array
end =
struct
  fun par (f, g) = (f (), g ())
  fun parfor (g: int) (lo, hi) (f: int -> unit) =
    if lo >= hi then () else (f lo; parfor g (lo + 1, hi) f)
  fun alloc n = ArrayExtra.alloc n
end

structure VectorExtra:
sig
  val unsafeFromArray: 'a array -> 'a vector
end =
struct open VectorExtra end

structure Concurrency =
struct
  val numberOfProcessors = 1

  fun cas r (x, y) =
    let val current = !r
    in if MLton.eq (x, current) then r := y else (); current
    end

  fun casArray (a, i) (x, y) =
    let val current = Array.sub (a, i)
    in if MLton.eq (x, current) then Array.update (a, i, y) else (); current
    end
end

structure ReadFile = PosixReadFile

structure WriteFile = PosixWriteFile

structure GCStats:
sig
  val report: unit -> unit
end =
struct

  fun p name thing =
    print (name ^ ": " ^ thing () ^ "\n")

  fun report () =
    let in print ("======== GC Stats ========\n"); print "none yet...\n"
    end

end

structure RuntimeStats:
sig
  type t
  val get: unit -> t
  val benchReport: {before: t, after: t} -> unit
end =
struct
  type t = unit
  fun get () = ()
  fun benchReport _ =
    ( print ("======== Runtime Stats ========\n")
    ; print ("none yet...\n")
    ; print ("====== End Runtime Stats ======\n")
    )
end

(* already provided by the compiler *)
structure ForkJoin = ForkJoin

structure Concurrency =
struct
  val numberOfProcessors = MLton.Parallel.numberOfProcessors
  val cas = MLton.Parallel.compareAndSwap
  val casArray = MLton.Parallel.arrayCompareAndSwap
end

structure VectorExtra:
sig
  val unsafeFromArray: 'a array -> 'a vector
end =
struct open VectorExtra end

structure ReadFile = PosixReadFile

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

structure MLtonParallelBasic :> MLTON_PARALLEL_BASIC =
struct
  val compareAndSwap = _import "Parallel_compareAndSwap" runtime private: Int32.int ref * Int32.int * Int32.int -> bool;
  val fetchAndAdd = _import "Parallel_fetchAndAdd" runtime private: Int32.int ref * Int32.int -> Int32.int;
  val takeLock = _import "Parallel_lockTake" runtime private: int ref -> unit;
  val releaseLock = _import "Parallel_lockRelease" runtime private: int ref -> unit;

  val printLock = ref ~1
  fun atomicPrint str =
    ( takeLock printLock
    ; print str
    ; releaseLock printLock
    ; ()
    )

    fun atomicPrint' strfn =
      ( takeLock printLock
      ; print (strfn ())
      ; releaseLock printLock
      ; ()
      )

  val processorNumber = MLtonParallelInternal.processorNumber
  val P = MLtonParallelInternal.numberOfProcessors

  fun meStr () = Int.toString (processorNumber ())

  val initFlags = Vector.tabulate (P, fn i => ref 0)
  fun myInitFlagStr () = "(" ^ Int.toString (!(Vector.sub (initFlags, processorNumber ()))) ^ ")"
  fun markInitialized () =
    let val p = processorNumber ()
    in if compareAndSwap (Vector.sub (initFlags, p), 0, 1) then ()
       else ( atomicPrint "Ahh!\n"
            ; OS.Process.atExit (fn () => atomicPrint ("Multiple attempted inits at " ^ Int.toString p ^ "\n"))
            ; OS.Process.exit OS.Process.failure
            )
    end

  fun loop () = loop ()

  fun run0 () =
    ( ()
    (*; atomicPrint (meStr() ^ " entering " ^ myInitFlagStr() ^ "\n")*)
    ; markInitialized ()
    ; atomicPrint' (fn () => meStr() ^ " initialized " ^ myInitFlagStr() ^ "\n")
    )

  fun run () =
    ( ()
    ; run0 ()
    ; loop ()
    )

  val () = atomicPrint "RUNNING DEBUG BASIC\n"

  val () = (_export "Parallel_run": (unit -> unit) -> unit;) run
  val () = (_import "Parallel_init" runtime private: unit -> unit;) ()

  val () = run0 ()



  (* ignore me; just necessary to satisfy signature *)
  type t = unit
  exception Parallel of string
  fun new' _ = raise Parallel "not implemented"
  fun new _ = raise Parallel "not implemented"
  fun edge _ = raise Parallel "not implemented"
  fun release _ = raise Parallel "not implemented"
  fun yield _ = raise Parallel "not implemented"
  fun self _ = raise Parallel "not implemented"
  fun checkCounters _ = raise Parallel "not implemented"
end

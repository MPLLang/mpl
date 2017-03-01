(* Copyright (C) 2017 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_PARALLEL =
  sig
    structure Deprecated:
              sig
                val yield: unit -> unit
                val lockInit: Word32.word ref -> unit;
                val takeLock: Word32.word ref -> unit;
                val releaseLock: Word32.word ref -> unit;
              end

    (* Perhaps some of these should be in module-specific Unsafe structures? *)
    structure Unsafe:
              sig
                val initPrimitiveThread:
                    unit MLtonThread.t -> MLtonThread.Runnable.t
                val arrayUninit: int -> 'a Array.array
              end

    val numberOfProcessors: int
    val processorNumber: unit -> int

    val registerThreadFunction: (unit -> unit) -> unit
    val initThreads: unit -> unit;
  end

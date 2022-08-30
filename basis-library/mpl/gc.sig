(* Copyright (C) 2020 Sam Westrick.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MPL_GC =
sig
  (* An estimate of the current size of the heap. It should be reasonably
   * accurate, although there could be concurrent allocations and/or
   * collections happening, so no guarantees.
   *)
  val currentHeapSize: unit -> IntInf.int

  (* How many times the read barrier has been triggered, which
   * checks that the program is disentangled.
   *)
  val numberDisentanglementChecks: unit -> IntInf.int

  (* How many times entanglement has been detected at a read barrier.
   *)
  val numberEntanglementsDetected: unit -> IntInf.int

  val numberSuspectsMarked: unit -> IntInf.int
  val numberSuspectsCleared: unit -> IntInf.int

  val getControlMaxCCDepth: unit -> int

  (* The following are all cumulative statistics (initially 0, and only
   * increase throughout execution).
   *
   * Each stat comes in two flavors: total, and per-processor. The total
   * stat is always just a sum of the corresponding per-proc stats.
   *
   * Naming convention for `stat` of type `t`:
   *   val stat: unit -> t
   *   val statOfProc: int -> t
   *)

  val bytesAllocated: unit -> IntInf.int
  val bytesAllocatedOfProc: int -> IntInf.int

  val localBytesReclaimed: unit -> IntInf.int
  val localBytesReclaimedOfProc: int -> IntInf.int

  val numLocalGCs: unit -> IntInf.int
  val numLocalGCsOfProc: int -> IntInf.int

  val localGCTime: unit -> Time.time
  val localGCTimeOfProc: int -> Time.time

  val promoTime: unit -> Time.time
  val promoTimeOfProc: int -> Time.time

  val rootBytesReclaimed: unit -> IntInf.int
  val rootBytesReclaimedOfProc: int -> IntInf.int

  val internalBytesReclaimed: unit -> IntInf.int
  val internalBytesReclaimedOfProc: int -> IntInf.int

  val numRootCCs: unit -> IntInf.int
  val numRootCCsOfProc: int -> IntInf.int

  val numInternalCCs: unit -> IntInf.int
  val numInternalCCsOfProc: int -> IntInf.int

  val rootCCTime: unit -> Time.time
  val rootCCTimeOfProc: int -> Time.time

  val internalCCTime: unit -> Time.time
  val internalCCTimeOfProc: int -> Time.time
end

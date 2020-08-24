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
end

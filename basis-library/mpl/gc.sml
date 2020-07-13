(* Copyright (C) 2020 Sam Westrick.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MPLGC :> MPL_GC =
struct

  local
    open Primitive.MLton
  in
    val numberOfProcessors = Int32.toInt Parallel.numberOfProcessors
    val gcState = GCState.gcState

    (* these names got out of hand, huh? *)
    fun getLocalGCMillisecondsOfProc p =
      GC.getLocalGCMillisecondsOfProc (gcState (), Word32.fromInt p)
    fun getPromoMillisecondsOfProc p =
      GC.getPromoMillisecondsOfProc (gcState (), Word32.fromInt p)
    fun getCumulativeStatisticsNumLocalGCsOfProc p =
      GC.getCumulativeStatisticsNumLocalGCsOfProc (gcState (), Word32.fromInt p)
    fun getCumulativeStatisticsBytesAllocatedOfProc p =
      GC.getCumulativeStatisticsBytesAllocatedOfProc (gcState (), Word32.fromInt p)
    fun getCumulativeStatisticsLocalBytesReclaimedOfProc p =
      GC.getCumulativeStatisticsLocalBytesReclaimedOfProc (gcState (), Word32.fromInt p)
  end

  exception NotYetImplemented of string
  exception InvalidProcessorNumber of int

  fun checkProcNum p =
    if p < 0 orelse p >= numberOfProcessors then
      raise InvalidProcessorNumber p
    else
      ()

  fun millisecondsToTime ms = Time.fromMilliseconds (C_UIntmax.toLargeInt ms)

  fun currentHeapSize () =
    raise NotYetImplemented "MPL.GC.currentHeapSize"

  fun bytesAllocatedOfProc p =
    ( checkProcNum p
    ; C_UIntmax.toLargeInt (getCumulativeStatisticsBytesAllocatedOfProc p)
    )

  fun localBytesReclaimedOfProc p =
    ( checkProcNum p
    ; C_UIntmax.toLargeInt (getCumulativeStatisticsLocalBytesReclaimedOfProc p)
    )

  fun numLocalGCsOfProc p =
    ( checkProcNum p
    ; C_UIntmax.toLargeInt (getCumulativeStatisticsNumLocalGCsOfProc p)
    )

  fun localGCTimeOfProc p =
    ( checkProcNum p
    ; millisecondsToTime (getLocalGCMillisecondsOfProc p)
    )

  fun promoTimeOfProc p =
    ( checkProcNum p
    ; millisecondsToTime (getPromoMillisecondsOfProc p)
    )

  fun sumAllProcs (f: 'a * 'a -> 'a) (perProc: int -> 'a) =
    let
      fun loop b i =
        if i >= numberOfProcessors then b else loop (f (b, perProc i)) (i+1)
    in
      loop (perProc 0) 1
    end

  fun bytesAllocated () =
    C_UIntmax.toLargeInt
    (sumAllProcs C_UIntmax.+ getCumulativeStatisticsBytesAllocatedOfProc)

  fun localBytesReclaimed () =
    C_UIntmax.toLargeInt
    (sumAllProcs C_UIntmax.+ getCumulativeStatisticsLocalBytesReclaimedOfProc)

  fun numLocalGCs () =
    C_UIntmax.toLargeInt
    (sumAllProcs C_UIntmax.+ getCumulativeStatisticsNumLocalGCsOfProc)

  fun localGCTime () =
    millisecondsToTime (sumAllProcs C_UIntmax.+ getLocalGCMillisecondsOfProc)

  fun promoTime () =
    millisecondsToTime (sumAllProcs C_UIntmax.+ getPromoMillisecondsOfProc)

end

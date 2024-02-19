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
    fun getNumCCsOfProc p =
      GC.getNumCCsOfProc (gcState (), Word32.fromInt p)
    fun getCCMillisecondsOfProc p =
      GC.getCCMillisecondsOfProc (gcState (), Word32.fromInt p)
    fun getCCBytesReclaimedOfProc p =
      GC.getCCBytesReclaimedOfProc (gcState (), Word32.fromInt p)

    fun bytesInScopeForLocal () =
      C_UIntmax.toLargeInt (GC.bytesInScopeForLocal (gcState ()))

    fun bytesInScopeForCC () =
      C_UIntmax.toLargeInt (GC.bytesInScopeForCC (gcState ()))

    fun numberDisentanglementChecks () =
      C_UIntmax.toLargeInt (GC.numberDisentanglementChecks (gcState ()))

    fun numberEntanglements () =
      C_UIntmax.toLargeInt (GC.numberEntanglements (gcState ()))

    fun approxRaceFactor () =
      (GC.approxRaceFactor (gcState ()))

    fun getControlMaxCCDepth () =
      Word32.toInt (GC.getControlMaxCCDepth (gcState ()))

    fun numberSuspectsMarked () =
      C_UIntmax.toLargeInt (GC.numberSuspectsMarked (gcState ()))

    fun numberSuspectsCleared () =
      C_UIntmax.toLargeInt (GC.numberSuspectsCleared (gcState ()))

    fun bytesPinnedEntangled () =
      C_UIntmax.toLargeInt (GC.bytesPinnedEntangled (gcState ()))

    fun bytesPinnedEntangledWatermark () =
      C_UIntmax.toLargeInt (GC.bytesPinnedEntangledWatermark (gcState ()))
    fun maxStackSizeForHeartbeat () =
      C_UIntmax.toLargeInt (GC.maxStackSizeForHeartbeat (gcState ()))
  
    fun maxStackFramesWalkedForHeartbeat () =
      C_UIntmax.toLargeInt (GC.maxStackFramesWalkedForHeartbeat (gcState ()))
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

  fun numCCsOfProc p =
    ( checkProcNum p
    ; C_UIntmax.toLargeInt (getNumCCsOfProc p)
    )

  fun ccTimeOfProc p =
    ( checkProcNum p
    ; millisecondsToTime (getCCMillisecondsOfProc p)
    )

  fun ccBytesReclaimedOfProc p =
    ( checkProcNum p
    ; C_UIntmax.toLargeInt (getCCBytesReclaimedOfProc p)
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

  fun numCCs () =
    C_UIntmax.toLargeInt
    (sumAllProcs C_UIntmax.+ getNumCCsOfProc)

  fun ccTime () =
    millisecondsToTime
    (sumAllProcs C_UIntmax.+ getCCMillisecondsOfProc)

  fun ccBytesReclaimed () =
    C_UIntmax.toLargeInt
    (sumAllProcs C_UIntmax.+ getCCBytesReclaimedOfProc)


  (* ======================================================================
   * DEPRECATED
   *)

  exception Deprecated of string

  fun d name (_: 'a) : 'b =
    raise Deprecated ("MPL.GC." ^ name)
  
  val rootBytesReclaimed = d "rootBytesReclaimed"
  val rootBytesReclaimedOfProc = d "rootBytesReclaimedOfProc"
  val internalBytesReclaimed = d "internalBytesReclaimed"
  val internalBytesReclaimedOfProc = d "internalBytesReclaimedOfProc"
  val numRootCCs = d "numRootCCs"
  val numRootCCsOfProc = d "numRootCCsOfProc"
  val numInternalCCs = d "numInternalCCs"
  val numInternalCCsOfProc = d "numInternalCCsOfProc"
  val rootCCTime = d "rootCCTime"
  val rootCCTimeOfProc = d "rootCCTimeOfProc"
  val internalCCTime = d "internalCCTime"
  val internalCCTimeOfProc = d "internalCCTimeOfProc"

end

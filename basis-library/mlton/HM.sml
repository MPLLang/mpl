(* Copyright (C) 2019-2020 Sam Westrick
 * Copyright (C) 2015 Ram Raghunathan
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonHM :> MLTON_HM =
struct
  structure PrimHM = Primitive.MLton.HM
  val gcState = Primitive.MLton.GCState.gcState

  fun registerQueue (p, q) = PrimHM.registerQueue (gcState (), p, q)
  fun registerQueueTop (p, top) = PrimHM.registerQueueTop (gcState (), p, top)
  fun registerQueueBot (p, bot) = PrimHM.registerQueueBot (gcState (), p, bot)

  fun arrayUpdateNoBarrier (a, i, x) =
    PrimHM.arrayUpdateNoBarrier (a, SeqIndex.fromInt i, x)

  val refAssignNoBarrier = PrimHM.refAssignNoBarrier

  fun arraySubNoBarrier (a, i) =
    PrimHM.arraySubNoBarrier (a, SeqIndex.fromInt i)

  val refDerefNoBarrier = PrimHM.refDerefNoBarrier
end

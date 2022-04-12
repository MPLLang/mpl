(* Copyright (C) 2019-2020 Sam Westrick
 * Copyright (C) 2015 Ram Raghunathan
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonHM :> MLTON_HM =
struct
  structure PrimHM = Primitive.MLton.HM

  val registerQueue: Word32.word * 'a array -> unit = PrimHM.registerQueue
  val registerQueueTop: Word32.word * Word64.word ref -> unit = PrimHM.registerQueueTop
  val registerQueueBot: Word32.word * Word32.word ref -> unit = PrimHM.registerQueueBot

  fun arrayUpdateNoBarrier (a, i, x) =
    PrimHM.arrayUpdateNoBarrier (a, SeqIndex.fromInt i, x)

  val refAssignNoBarrier = PrimHM.refAssignNoBarrier

  fun arraySubNoBarrier (a, i) =
    PrimHM.arraySubNoBarrier (a, SeqIndex.fromInt i)

  val refDerefNoBarrier = PrimHM.refDerefNoBarrier
end

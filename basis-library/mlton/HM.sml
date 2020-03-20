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

  val arrayUpdateNoBarrier = PrimHM.arrayUpdateNoBarrier
  val refAssignNoBarrier = PrimHM.refAssignNoBarrier

  val arraySubNoBarrier = PrimHM.arraySubNoBarrier
  val refDerefNoBarrier = PrimHM.refDerefNoBarrier
end

(* Copyright (C) 2019 Sam Westrick
 * Copyright (C) 2015 Ram Raghunathan
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonHM :> MLTON_HM =
struct
  structure PrimHM = Primitive.MLton.HM

  val registerQueue: Word32.word * 'a array -> unit = PrimHM.registerQueue

  val arrayUpdateNoBarrier = PrimHM.arrayUpdateNoBarrier
  val refAssignNoBarrier = PrimHM.refAssignNoBarrier
end

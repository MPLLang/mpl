(* Copyright (C) 2019 Sam Westrick
 * Copyright (C) 2015 Ram Raghunathan
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_HM =
sig
  val registerQueue: Word32.word * 'a array -> unit
  val registerQueueTop: Word32.word * Word64.word ref -> unit
  val registerQueueBot: Word32.word * Word32.word ref -> unit
  val arrayUpdateNoBarrier : 'a array * SeqIndex.int * 'a -> unit
  val refAssignNoBarrier : 'a ref * 'a -> unit
end

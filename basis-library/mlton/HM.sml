(* Copyright (C) 2019 Sam Westrick
 * Copyright (C) 2015 Ram Raghunathan
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonHM :> MLTON_HM =
struct
  structure PrimHM = Primitive.MLton.HM

  structure HierarchicalHeap =
  struct
    type 'a t = unit

    exception HHDeprecated

    fun new _ = raise HHDeprecated

    fun set _ = raise HHDeprecated
    fun get _ = raise HHDeprecated

    fun setLevel _ = raise HHDeprecated
    fun getLevel _ = raise HHDeprecated
    fun getLowestPrivateLevel _ = raise HHDeprecated

    fun appendChild _ = raise HHDeprecated
    fun setReturnValue _ = raise HHDeprecated
    fun mergeIntoParentAndGetReturnValue _ = raise HHDeprecated
    fun promoteChunks _ = raise HHDeprecated

    fun setDead _ = raise HHDeprecated

    fun setUseHierarchicalHeap _ = raise HHDeprecated
  end

  (* val enterGlobalHeap: unit -> unit = PrimHM.enterGlobalHeap
  val exitGlobalHeap: unit -> unit = PrimHM.exitGlobalHeap *)

  val registerQueue: Word32.word * 'a array -> unit = PrimHM.registerQueue
  val registerQueueLock: Word32.word * Word32.word ref -> unit =
      PrimHM.registerQueueLock

  val arrayUpdateNoBarrier = PrimHM.arrayUpdateNoBarrier
  val refAssignNoBarrier = PrimHM.refAssignNoBarrier
end

(*
structure MLtonHM:> MLTON_HM =
    struct
        structure PrimHM = Primitive.MLton.HM

        structure HierarchicalHeap =
            struct
                structure PrimHH = PrimHM.HierarchicalHeap

                type 'a t = 'a PrimHH.t

                val new: unit -> unit t = PrimHH.newHierarchicalHeap

                val set: 'a t -> unit = PrimHH.setHierarchicalHeap

                val get: unit -> unit t = PrimHH.getHierarchicalHeap

                fun setLevel ((hh, level): 'a t * int): unit =
                    PrimHH.setLevel (hh, Word32.fromInt level)

                fun getLevel (hh: 'a t): int = Word32.toInt (PrimHH.getLevel hh)

                fun getLowestPrivateLevel (hh: 'a t): int = Word32.toInt (PrimHH.getLowestPrivateLevel hh)

                fun appendChild ((parentHH, childHH, stealLevel):
                                 ('a t * 'b t * int)): unit =
                    PrimHH.appendChildHeap (parentHH,
                                            childHH,
                                            Word32.fromInt stealLevel)

                val setReturnValue: 'a t * 'b -> 'b t = PrimHH.setReturnValue

                val mergeIntoParentAndGetReturnValue: 'a t -> 'a =
                    PrimHH.mergeIntoParentHeapAndGetReturnValue

                val promoteChunks : 'a t -> unit = PrimHH.promoteChunks

                val setDead: 'a t -> unit = PrimHH.setDead

                val setUseHierarchicalHeap: bool -> unit =
                    PrimHH.setCurrentThreadUseHierarchicalHeap
            end

        val enterGlobalHeap: unit -> unit = PrimHM.enterGlobalHeap
        val exitGlobalHeap: unit -> unit = PrimHM.exitGlobalHeap
        (* val explicitEnterGlobalHeap: Word32.word -> unit =
            PrimHM.explicitEnterGlobalHeap *)
        (* val explicitExitGlobalHeap: unit -> Word32.word =
            PrimHM.explicitExitGlobalHeap *)

        val registerQueue: Word32.word * 'a array -> unit = PrimHM.registerQueue
        val registerQueueLock: Word32.word * Word32.word ref -> unit =
            PrimHM.registerQueueLock

        val arrayUpdateNoBarrier = PrimHM.arrayUpdateNoBarrier
        val refAssignNoBarrier = PrimHM.refAssignNoBarrier
    end
*)

(* Copyright (C) 2015 Ram Raghunathan
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

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
        val explicitEnterGlobalHeap: Word32.word -> unit =
            PrimHM.explicitEnterGlobalHeap
        val explicitExitGlobalHeap: unit -> Word32.word =
            PrimHM.explicitExitGlobalHeap

        val registerQueue: Word32.word * 'a array -> unit = PrimHM.registerQueue
        val registerQueueLock: Word32.word * Word32.word ref -> unit =
            PrimHM.registerQueueLock
    end

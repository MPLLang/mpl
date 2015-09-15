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

                type t = PrimHH.t

                val new: unit -> t = PrimHH.newHierarchicalHeap

                val set: t -> unit = PrimHH.setHierarchicalHeap
                val get: unit -> t = PrimHH.getHierarchicalHeap

                val setLevel: t * int -> unit =
                 fn (hh, level) => PrimHH.setLevel (hh, Word32.fromInt level)
                val getLevel: t -> int =
                 fn hh => Word32.toInt (PrimHH.getLevel hh)
                val promoteChunks: t -> unit = PrimHH.promoteChunks

                val appendChild: (t * t * int) -> unit =
                 fn (parentHH, childHH, stealLevel) =>
                    PrimHH.appendChildHeap (parentHH,
                                            childHH,
                                            Word32.fromInt stealLevel)
                val mergeIntoParent: t -> unit = PrimHH.mergeIntoParentHeap

                val useHierarchicalHeap: unit -> unit =
                    PrimHH.setCurrentThreadUseHierarchicalHeap
            end

        val enterGlobalHeap: unit -> unit = PrimHM.enterGlobalHeap
        val exitGlobalHeap: unit -> unit = PrimHM.exitGlobalHeap
        val explicitEnterGlobalHeap: Word32.word -> unit =
            PrimHM.explicitEnterGlobalHeap
        val explicitExitGlobalHeap: unit -> Word32.word =
            PrimHM.explicitExitGlobalHeap

        val registerQueue: int * 'a array -> unit = PrimHM.registerQueue
        val registerQueueLock: int * int ref -> unit = PrimHM.registerQueueLock
    end

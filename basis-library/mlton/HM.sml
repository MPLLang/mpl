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

                val appendChild: (t * t) -> unit = PrimHH.appendChildHeap
                val mergeIntoParent: t -> unit = PrimHH.mergeIntoParentHeap

                val set: t -> unit = PrimHH.setHierarchicalHeap
                val get: unit -> t = PrimHH.getHierarchicalHeap
            end

        val enterGlobalHeap: bool -> unit = PrimHM.enterGlobalHeap
        val exitGlobalHeap: bool -> unit = PrimHM.exitGlobalHeap
    end

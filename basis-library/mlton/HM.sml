(* Copyright (C) 2015 Ram Raghunathan
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonHM:> MLTON_HM =
    struct
        structure HierarchicalHeap =
            struct
                structure PrimHH = Primitive.MLton.HM.HierarchicalHeap

                type t = PrimHH.t

                val new: unit -> t = PrimHH.newHierarchicalHeap
                val appendChild: (t * t) -> unit = PrimHH.appendChildHeap
                val mergeIntoParent: t -> unit = PrimHH.mergeIntoParentHeap
            end
    end

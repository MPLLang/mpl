(* Copyright (C) 2015 Ram Raghunathan
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_HM =
    sig
        structure HierarchicalHeap:
                  sig
                      type t

                      val new: unit -> t

                      val set: t -> unit
                      val get: unit -> t

                      val setLevel: t * int -> unit
                      val getLevel: t -> int
                      val promoteChunks: t -> unit

                      val appendChild: (t * t) -> unit
                      val mergeIntoParent: t -> unit

                      val useHierarchicalHeap: unit -> unit
                  end

        val enterGlobalHeap: unit -> unit
        val exitGlobalHeap: unit -> unit
        val explicitEnterGlobalHeap: Word32.word -> unit
        val explicitExitGlobalHeap: unit -> Word32.word
    end

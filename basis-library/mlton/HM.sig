(* Copyright (C) 2015 Ram Raghunathan
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_HM =
    sig
        structure HierarchicalHeap:
                  sig
                      type 'a t

                      val new: unit -> unit t

                      val set: 'a t -> unit
                      val get: unit -> unit t

                      val setLevel: 'a t * int -> unit
                      val getLevel: 'a t -> int

                      val appendChild: 'a t * 'b t * int -> unit
                      val setReturnValue: 'a t * 'b -> 'b t
                      val mergeIntoParentAndGetReturnValue: 'a t -> 'a
                      val promoteChunks: 'a t -> unit

                      val setDead: 'a t -> unit

                      val setUseHierarchicalHeap: bool -> unit
                  end

        val enterGlobalHeap: unit -> unit
        val exitGlobalHeap: unit -> unit
        val explicitEnterGlobalHeap: Word32.word -> unit
        val explicitExitGlobalHeap: unit -> Word32.word

        val registerQueue: Word32.word * 'a array -> unit
        val registerQueueLock: Word32.word * Word32.word ref -> unit
    end

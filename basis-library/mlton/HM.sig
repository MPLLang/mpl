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

                      val appendChild: (t * t) -> unit
                      val mergeIntoParent: t -> unit

                      val set: t -> unit
                      val get: unit -> t
                  end

        val enterGlobalHeap: unit -> unit
        val exitGlobalHeap: unit -> unit
    end

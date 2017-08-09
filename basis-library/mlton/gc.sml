(* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonGC =
   struct
      open Primitive.MLton.GC

      val pack : unit -> unit =
         fn () => pack ()
      val unpack : unit -> unit =
         fn () => unpack ()

      val setHashConsDuringGC : bool -> unit =
         fn b => setHashConsDuringGC b
      val setMessages : bool -> unit =
         fn b => setMessages b
      val setRusageMeasureGC : bool -> unit =
         fn b => setRusageMeasureGC b
      val setSummary : bool -> unit =
         fn b => setSummary b

      structure Statistics =
         struct
            local
               fun mk conv prim =
                  fn () => conv (prim ())
               val mkSize = mk C_Size.toLargeInt
               val mkUIntmax = mk C_UIntmax.toLargeInt
               val mkMilliseconds = mk (Time.fromMilliseconds o C_UIntmax.toLargeInt)
            in
               val bytesAllocated = mkUIntmax getBytesAllocated
               val lastBytesLive = mkSize getLastBytesLive
               val maxChunkPoolOccupancy = mkSize getMaxChunkPoolOccupancy
               val maxHeapOccupancy = mkSize getMaxHeapOccupancy
               val maxBytesLive = mkSize getMaxBytesLive
               val numCopyingGCs = mkUIntmax getNumCopyingGCs
               val numMarkCompactGCs = mkUIntmax getNumMarkCompactGCs
               val numMinorGCs = mkUIntmax getNumMinorGCs
            end
         end

   end

(* Copyright (C) 2019 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonGC =
   struct
      open Primitive.MLton.GC

      val gcState = Primitive.MLton.GCState.gcState

      val pack : unit -> unit =
         fn () => pack (gcState ())
      val unpack : unit -> unit =
         fn () => unpack (gcState ())

      val setHashConsDuringGC : bool -> unit =
         fn b => setHashConsDuringGC (gcState (), b)
      val setMessages : bool -> unit =
         fn b => setMessages (gcState (), b)
      val setRusageMeasureGC : bool -> unit =
         fn b => setRusageMeasureGC (gcState (), b)
      val setSummary : bool -> unit =
         fn b => setSummary (gcState (), b)

      structure Statistics =
         struct
            local
               fun mk conv prim =
                  fn () => conv (prim (gcState ()))
               val mkSize = mk C_Size.toLargeInt
               val mkUIntmax = mk C_UIntmax.toLargeInt
               val mkMilliseconds = mk (Time.fromMilliseconds o C_UIntmax.toLargeInt)
            in
               val bytesAllocated = mkUIntmax getBytesAllocated
               val bytesPromoted = mkUIntmax getBytesPromoted
               val lastBytesLive = mkSize getLastBytesLive
               val maxChunkPoolOccupancy = mkSize (fn _ => getMaxChunkPoolOccupancy ())
               val maxHeapOccupancy = mkSize getMaxHeapOccupancy
               val maxBytesLive = mkSize getMaxBytesLive
               val numCopyingGCs = mkUIntmax getNumCopyingGCs
               val numMarkCompactGCs = mkUIntmax getNumMarkCompactGCs
               val numMinorGCs = mkUIntmax getNumMinorGCs

               fun localGCTimeOfProc p =
                 Time.fromMilliseconds (C_UIntmax.toLargeInt
                 (getLocalGCMillisecondsOfProc (gcState (), Word32.fromInt p)))
               fun promoTimeOfProc p =
                 Time.fromMilliseconds (C_UIntmax.toLargeInt
                 (getPromoMillisecondsOfProc (gcState (), Word32.fromInt p)))
            end
         end

   end

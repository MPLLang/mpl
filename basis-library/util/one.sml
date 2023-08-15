(* Copyright (C) 2006-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 2023 Sam Westrick.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure One:
   sig
      type 'a t

      val make: (unit -> 'a) -> 'a t
      val use: 'a t * ('a -> 'b) -> 'b
   end =
   struct

      (* SAM_NOTE: using Word8 instead of bool here to work around the
       * compilation bug with primitive compareAndSwap... (The compilation
       * passes splitTypes1 and splitTypes2 cause a crash when compareAndSwap
       * is used on certain data types, including bool.)
       *
       * Here I use 0w0 for false, and 0w1 for true
       *
       * When we fix compilation for compareAndSwap, we can switch back
       * to using bool.
       *)

      datatype 'a t = T of {more: unit -> 'a,
                            static: 'a,
                            staticIsInUse: Primitive.Word8.word ref}

      fun make f = T {more = f,
                      static = f (),
                      staticIsInUse = ref 0w0}

      val cas = Primitive.MLton.Parallel.compareAndSwap

      fun use (T {more, static, staticIsInUse}, f) =
         let
            val () = Primitive.MLton.Thread.atomicBegin ()
            val claimed =
               (!staticIsInUse) = 0w0
               andalso
               0w0 = cas (staticIsInUse, 0w0, 0w1)
            val d =
               if not claimed then
                  (Primitive.MLton.Thread.atomicEnd ();
                   more ())
               else
                  (Primitive.MLton.Thread.atomicEnd ();
                   static)
        in
           DynamicWind.wind (fn () => f d,
                             fn () => if claimed then staticIsInUse := 0w0 else ())
        end
   end

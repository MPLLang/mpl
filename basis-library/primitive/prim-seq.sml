(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Primitive names are special -- see atoms/prim.fun. *)

structure Primitive = struct

open Primitive

structure Array =
   struct
      open Array
      val allocUnsafe = _prim "Array_alloc": SeqIndex.int -> 'a array;
      val copyArrayUnsafe = _prim "Array_copyArray": 'a array * SeqIndex.int * 'a array * SeqIndex.int * SeqIndex.int -> unit;
      val copyVectorUnsafe = _prim "Array_copyVector": 'a array * SeqIndex.int * 'a vector * SeqIndex.int * SeqIndex.int -> unit;
      val length = _prim "Array_length": 'a array -> SeqIndex.int;
      (* There is no maximum length on arrays, so maxLen' = SeqIndex.maxInt'. *)
      (* val maxLen': SeqIndex.int = SeqIndex.maxInt' *)
      val subUnsafe = _prim "Array_sub": 'a array * SeqIndex.int -> 'a;
      val uninitIsNop = _prim "Array_uninitIsNop": 'a array -> bool;
      val uninitUnsafe = _prim "Array_uninit": 'a array * SeqIndex.int -> unit;
      val updateUnsafe = _prim "Array_update": 'a array * SeqIndex.int * 'a -> unit;

      structure Raw :> sig
                          type 'a rawarr
                          val allocUnsafe: SeqIndex.int -> 'a rawarr
                          val length: 'a rawarr -> SeqIndex.int
                          val toArrayUnsafe: 'a rawarr -> 'a array
                          val uninitIsNop: 'a rawarr -> bool
                          val uninitUnsafe: 'a rawarr * SeqIndex.int -> unit
                       end =
        struct
           type 'a rawarr = 'a array
           val allocUnsafe = _prim "Array_allocRaw": SeqIndex.int -> 'a rawarr;
           val length = length
           val toArrayUnsafe = _prim "Array_toArray": 'a rawarr -> 'a array;
           val uninitIsNop = uninitIsNop
           val uninitUnsafe = uninitUnsafe
        end
   end

structure Vector =
   struct
      open Vector 
      (* Don't mutate the array after you apply fromArray, because vectors 
       * are supposed to be immutable and the optimizer depends on this.  
       *)
      val fromArrayUnsafe = _prim "Array_toVector": 'a array -> 'a vector;
      val length = _prim "Vector_length": 'a vector -> SeqIndex.int;
      val subUnsafe = _prim "Vector_sub": 'a vector * SeqIndex.int -> 'a;
      val vector0 = _prim "Vector_vector": unit -> 'a vector;
   end

(* ----------------------------------------------------------------------------
 * Flattened representations of arrays and vectors
 *
 * SAM_NOTE: plenty of duplication with the Array and Vector structures above.
 * Personally I prefer it this way for clarity, because in general we don't
 * want to assume that the {array, vector} types support the same operations as
 * {array_aos, vector_aos}.
 *
 * SAM_NOTE: there is a bit of ad-hoc polymorphism going on here: many of
 * these array primitives are essentially polymorphic w.r.t. array vs array_aos
 * (and vector vs vector_aos). You'll see that the names of the primitives
 * are identical in almost all cases...
 * ----------------------------------------------------------------------------
 *)

structure ArrayAos =
   struct
      open ArrayAos
      val allocUnsafe = _prim "Array_allocAos": SeqIndex.int -> 'a array_aos;
      val copyArrayUnsafe = _prim "Array_copyArray": 'a array_aos * SeqIndex.int * 'a array_aos * SeqIndex.int * SeqIndex.int -> unit;
      val copyVectorUnsafe = _prim "Array_copyVector": 'a array_aos * SeqIndex.int * 'a VectorAos.t * SeqIndex.int * SeqIndex.int -> unit;
      val length = _prim "Array_length": 'a array_aos -> SeqIndex.int;
      (* There is no maximum length on arrays, so maxLen' = SeqIndex.maxInt'. *)
      (* val maxLen': SeqIndex.int = SeqIndex.maxInt' *)
      val subUnsafe = _prim "Array_sub": 'a array_aos * SeqIndex.int -> 'a;
      val uninitIsNop = _prim "Array_uninitIsNop": 'a array_aos -> bool;
      val uninitUnsafe = _prim "Array_uninit": 'a array_aos * SeqIndex.int -> unit;
      val updateUnsafe = _prim "Array_update": 'a array_aos * SeqIndex.int * 'a -> unit;

      structure Raw :> sig
                          type 'a rawarr
                          val allocUnsafe: SeqIndex.int -> 'a rawarr
                          val length: 'a rawarr -> SeqIndex.int
                          val toArrayUnsafe: 'a rawarr -> 'a array_aos
                          val uninitIsNop: 'a rawarr -> bool
                          val uninitUnsafe: 'a rawarr * SeqIndex.int -> unit
                       end =
        struct
           type 'a rawarr = 'a array_aos
           val allocUnsafe = _prim "Array_allocRawAos": SeqIndex.int -> 'a rawarr;
           val length = length
           val toArrayUnsafe = _prim "Array_toArray": 'a rawarr -> 'a array_aos;
           val uninitIsNop = uninitIsNop
           val uninitUnsafe = uninitUnsafe
        end
   end

structure VectorAos =
   struct
      open VectorAos
      (* Don't mutate the array after you apply fromArray, because vectors 
       * are supposed to be immutable and the optimizer depends on this.  
       *)
      val fromArrayUnsafe = _prim "Array_toVector": 'a ArrayAos.t -> 'a vector_aos;
      val length = _prim "Vector_length": 'a vector_aos -> SeqIndex.int;
      val subUnsafe = _prim "Vector_sub": 'a vector_aos * SeqIndex.int -> 'a;
      val vector0 = _prim "Vector_vectorAos": unit -> 'a vector_aos;
   end

end

(* Copyright (C) 2025 Sam Westrick.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* The purpose of the following is slightly different from the Int_ChooseInt
 * functors: rather than "plug in" the true type of Int.int into parameter 'a
 * (which allows the type signature of the different implementations of `f` to
 * depend upon the choice of `Int.int`), we instead mandate that the type
 * signatures of the implementations of `f` all be agnostic to the underlying
 * type of `Int.int`.
 *
 * We can use this, for example, to select more efficient implementations of
 * functions depending on the integer bitwidth.
 *
 * If desired, the following pattern could be repeated for any polymorphic
 * arity, e.g., `type ('a, 'b) t` or `type ('a, 'b, 'c) t`, etc.
 *)

signature CHOOSE_FROM_INT_ARG =
   sig
      type 'a t
      val fInt8: 'a t
      val fInt16: 'a t
      val fInt32: 'a t
      val fInt64: 'a t
      val fIntInf: 'a t
   end

functor ChooseFromInt_Int8 (A : CHOOSE_FROM_INT_ARG) :
   sig val f : 'a A.t end =
   struct val f = A.fInt8 val _ = A.fInt16 val _ = A.fInt32 val _ = A.fInt64 val _ = A.fIntInf end
functor ChooseFromInt_Int16 (A : CHOOSE_FROM_INT_ARG) :
   sig val f : 'a A.t end =
   struct val _ = A.fInt8 val f = A.fInt16 val _ = A.fInt32 val _ = A.fInt64 val _ = A.fIntInf end
functor ChooseFromInt_Int32 (A : CHOOSE_FROM_INT_ARG) :
   sig val f : 'a A.t end =
   struct val _ = A.fInt8 val _ = A.fInt16 val f = A.fInt32 val _ = A.fInt64 val _ = A.fIntInf end
functor ChooseFromInt_Int64 (A : CHOOSE_FROM_INT_ARG) :
   sig val f : 'a A.t end =
   struct val _ = A.fInt8 val _ = A.fInt16 val _ = A.fInt32 val f = A.fInt64 val _ = A.fIntInf end
functor ChooseFromInt_IntInf (A : CHOOSE_FROM_INT_ARG) :
   sig val f : 'a A.t end =
   struct val _ = A.fInt8 val _ = A.fInt16 val _ = A.fInt32 val _ = A.fInt64 val f = A.fIntInf end
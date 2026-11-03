(* Copyright (C) 2009,2017,2019,2022 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ALLOCATE_VARIABLES_STRUCTS =
   sig
      structure Rssa: RSSA
      structure Machine: MACHINE
      sharing Rssa.BackendAtoms = Machine.BackendAtoms
   end

signature ALLOCATE_VARIABLES =
   sig
      include ALLOCATE_VARIABLES_STRUCTS
      (* TODO: mess with isLoop stuff? *)
      val allocate:
         {function: Rssa.Function.t,
          paramOffsets: (Rssa.Var.t * Rssa.Type.t) vector -> {offset: Bytes.t, ty: Rssa.Type.t, volatile: bool} vector,
          sporkNesting: {maxSporkNestLength: int,
                         spidInfo: Rssa.Spid.t -> {index: int,
                                                   tokenPolicy: Word32.word,
                                                   spwn: Rssa.Label.t},
                         sporkDataTy: Rssa.Type.t option,
                         sporkNest: Rssa.Label.t -> Rssa.Spid.t vector},
          varInfo: Rssa.Var.t -> {
                                  (* If (isSome operand) then a stack slot or
                                   * temporary needs to be allocated for the
                                   * variable.
                                   *)
                                  operand: Machine.Operand.t option ref option,
                                  ty: Machine.Type.t
                                  }
          }
         -> {(* If handlers are used, handlersInfo gives the stack offsets
              * where the handler and link (old exnStack) should be stored.
              *)
             handlersInfo: {handlerOffset: Bytes.t,
                            linkOffset: Bytes.t} option,
             labelInfo:
             Rssa.Label.t -> {(* Live operands at the beginning of the block. *)
                              live: Machine.Operand.t vector,
                              (* Live operands at the beginning of the block, 
                               * excepting its formals.
                               *)
                              liveNoFormals: Machine.Operand.t vector,
                              (* Size of frame including return address. *)
                              size: Bytes.t},
             (* If sporks are used, sporkInfo gives the stack offset
              * where the sporkData should be stored.
              *)
             sporkInfo: {sporkDataOffset: Bytes.t}}
   end

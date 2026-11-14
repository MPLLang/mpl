(* Copyright (C) 2019 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature C_CODEGEN_STRUCTS =
   sig
      structure Machine: MACHINE
   end

signature C_CODEGEN =
   sig
      include C_CODEGEN_STRUCTS

      structure C:
         sig
            val int: int -> string
         end

      val implementsPrim: 'a Machine.Prim.t -> bool
      val output: {program: Machine.Program.t,
                   outputC: unit -> {file: File.t,
                                     print: string -> unit,
                                     done: unit -> unit}
                   } -> unit
      val outputWith: {program: Machine.Program.t,
                       outputC: unit -> {file: File.t,
                                         print: string -> unit,
                                         done: unit -> unit},
                       outputChunks: ({chunks: Machine.Chunk.t list,
                                       labelBlock: Machine.Label.t -> Machine.Block.t,
                                       labelChunk: Machine.Label.t -> Machine.ChunkLabel.t,
                                       labelIndex: Machine.Label.t -> int}
                                      -> unit) option
                       } -> unit
      val outputDeclarations: {additionalMainArgs: string list,
                               includes: string list,
                               print: string -> unit,
                               program: Machine.Program.t,
                               rest: unit -> unit
                               } -> unit
   end

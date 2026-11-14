(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature VIBE_ARM_CODEGEN_STRUCTS =
   sig
      structure CCodegen: C_CODEGEN
      structure Machine: MACHINE
      sharing Machine = CCodegen.Machine
   end

signature VIBE_ARM_CODEGEN =
   sig
      include VIBE_ARM_CODEGEN_STRUCTS

      val implementsPrim: 'a Machine.Prim.t -> bool
      val output: {program: Machine.Program.t,
                   outputC: unit -> {file: File.t,
                                     print: string -> unit,
                                     done: unit -> unit},
                   outputS: unit -> {file: File.t,
                                     print: string -> unit,
                                     done: unit -> unit}
                   } -> unit
   end

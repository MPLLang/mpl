(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* x86-64 (AMD64) assembly, as data.
 *
 * This is the output of the vibe-amd64 codegen: instructions over physical
 * registers, labels, and the few directives that the codegen needs.  It
 * knows nothing of the Machine IR.  The instructions are printed in AT&T
 * syntax, and their operands are in AT&T order (sources first, destination
 * last).  The syntax that depends on the target (the prefix of a symbol,
 * the spelling of a local label, the names of sections) is the business of
 * the printer.
 *
 * Registers are numbers: 0-15 are rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi,
 * r8-r15 (the order of their encodings); 16-31 are xmm0-xmm15.
 *)

signature VIBE_AMD64_ASM_STRUCTS =
   sig
   end

signature VIBE_AMD64_ASM =
   sig
      include VIBE_AMD64_ASM_STRUCTS

      structure Size:
         sig
            (* of an integer operand: 1, 2, 4 or 8 bytes *)
            datatype t = B | W | L | Q

            val fromBytes: int -> t
            (* of a mnemonic *)
            val suffix: t -> string
         end

      structure Cond:
         sig
            datatype t =
               E | NE
             | B | AE    (* unsigned <, >=: carry set/clear *)
             | A | BE    (* unsigned >, <= *)
             | L | GE
             | G | LE
             | O | NO
             | P | NP

            val negate: t -> t
            val toString: t -> string
         end

      structure Label:
         sig
            datatype t =
               (* a C-level symbol; printed with the target's prefix *)
               Sym of string
               (* local to the assembler; printed with the target's prefix
                * for local labels
                *)
             | Local of string
               (* printed as it is *)
             | Plain of string

            val equals: t * t -> bool
         end

      structure Target:
         sig
            (* where a jump goes *)
            datatype t =
               Label of Label.t
               (* the nearest numeric local label n that follows (nf) or
                * precedes (nb)
                *)
             | Forward of int
             | Backward of int
         end

      structure Addr:
         sig
            datatype t =
               (* disp(base, index, scale) *)
               T of {base: int option,
                     index: (int * int) option,
                     disp: int}
               (* sym+disp(%rip) *)
             | Rip of Label.t * int
               (* the entry for sym in the global offset table *)
             | Got of Label.t

            (* is the register part of the address? *)
            val uses: t * int -> bool
         end

      structure Operand:
         sig
            datatype t =
               (* For an xmm register, the size is of no consequence. *)
               Reg of int * Size.t
             | Imm of IntInf.t
             | Mem of Addr.t

            val uses: t * int -> bool
         end

      structure Section:
         sig
            datatype t =
               Text
               (* read-only data that holds addresses *)
             | RelRo
               (* constants of 4 and of 8 bytes *)
             | Literal4
             | Literal8
         end

      structure Instr:
         sig
            datatype t =
               (* labels and directives *)
               Label of Label.t
             | LocalLabel of int             (* a numeric local label *)
             | Section of Section.t
             | P2align of int
             | Globl of Label.t
               (* not visible outside of the executable or library *)
             | Hidden of Label.t
             | Long of Label.t * Label.t     (* .long l - l' *)
             | Quad of Label.t option        (* an address, or 0 *)
             | LongImm of IntInf.t
             | QuadImm of IntInf.t
               (* An instruction: its mnemonic (with its size suffix, if it
                * has one) and its operands, in AT&T order.
                *)
             | Ins of string * Operand.t list
               (* cmovcc src, dst *)
             | Cmov of Cond.t * Size.t * Operand.t * int
               (* setcc of the low byte of a register *)
             | Setcc of Cond.t * int
               (* control *)
             | Jmp of Target.t
             | Jcc of Cond.t * Target.t
             | JmpInd of Operand.t
             | Call of Label.t
             | CallInd of Operand.t
             | Ret

            (* in the syntax of the current target; without indentation or
             * a newline
             *)
            val toString: t -> string
         end

      (* print instructions as an assembly file (or part of one) for the
       * current target
       *)
      val output: Instr.t list * (string -> unit) -> unit
   end

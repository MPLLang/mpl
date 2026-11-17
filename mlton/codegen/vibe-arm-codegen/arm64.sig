(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* ARM64 (AArch64) assembly, as data.
 *
 * This is the output of the vibe-arm codegen: instructions over physical
 * registers, labels, and the few directives that the codegen needs.  It
 * knows nothing of the Machine IR.  The syntax that depends on the target
 * (the prefix of a symbol, the spelling of a local label and of a
 * relocation, the names of sections) is the business of the printer, not
 * of whoever builds the instructions.
 *
 * Aliases are kept apart where the codegen distinguishes them (the
 * conditions hs/cs and lo/cc; [b] and [b, #0]), so that what is printed is
 * what was built.
 *)

signature ARM64_STRUCTS =
   sig
   end

signature ARM64 =
   sig
      include ARM64_STRUCTS

      structure Reg:
         sig
            (* a register, at the width that an instruction uses it *)
            datatype t =
               W of int   (* w0-w30 *)
             | X of int   (* x0-x30 *)
             | S of int   (* s0-s31 *)
             | D of int   (* d0-d31 *)
             | SP
             | WZR

            val toString: t -> string
         end

      structure Cond:
         sig
            datatype t =
               EQ | NE
             | CS | CC   (* carry set/clear *)
             | HS | LO   (* unsigned >=, <: the same codes as CS, CC *)
             | MI | PL
             | VS | VC
             | HI | LS
             | GE | LT
             | GT | LE

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
            (* where a branch goes *)
            datatype t =
               Label of Label.t
               (* the nearest numeric local label n that follows (nf) or
                * precedes (nb)
                *)
             | Forward of int
             | Backward of int
         end

      structure Operand:
         sig
            datatype shift = LSL | LSR | ASR
            datatype extend = SXTB | SXTH | SXTW | UXTB | UXTH | UXTW
            (* the last operand of a data-processing instruction *)
            datatype t =
               Reg of Reg.t
             | Imm of IntInf.t                 (* #n *)
             | Shifted of Reg.t * shift * int  (* r, lsl #n *)
             | Extended of Reg.t * extend      (* r, sxtb *)
         end

      structure Addr:
         sig
            datatype t =
               Base of Reg.t                 (* [b] *)
             | Offset of Reg.t * int         (* [b, #n] *)
               (* [b, i] if the shift is 0, else [b, i, lsl #n] *)
             | Index of Reg.t * Reg.t * int
             | PreIndex of Reg.t * int       (* [b, #n]! *)
             | PostIndex of Reg.t * int      (* [b], #n *)
         end

      structure Section:
         sig
            datatype t =
               Text
               (* read-only data that holds addresses *)
             | RelRo
         end

      structure Instr:
         sig
            datatype unop =
               MOV | MVN | NEG | NEGS
             | SXTB | SXTH | SXTW
             | FMOV | FABS | FNEG | FSQRT
             | FCVT | FCVTZS | FCVTZU | SCVTF | UCVTF
             | FRINTM | FRINTP | FRINTX | FRINTZ
            datatype binop =
               ADD | ADDS | SUB | SUBS
             | MUL | SMULH | SMULL | UMULH | SDIV | UDIV
             | AND | ORR | EOR
             | LSL | LSR | ASR | ROR
             | FADD | FSUB | FMUL | FDIV
            datatype ternop = MSUB | FMADD | FNMSUB
            datatype cmpop = CMP | CMN | FCMP
            datatype selop = CSEL | CSINC | FCSEL
            datatype loadop =
               LDR | LDRB | LDRH | LDRSW
             | LDAXR | LDAXRB | LDAXRH
            datatype storeop = STR | STRB | STRH
            datatype storexop = STLXR | STLXRB | STLXRH
            datatype pairop = LDP | STP

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
               (* data processing: destination first *)
             | Un of unop * Reg.t * Reg.t
             | Bin of binop * Reg.t * Reg.t * Operand.t
             | Tern of ternop * Reg.t * Reg.t * Reg.t * Reg.t
             | Cmp of cmpop * Reg.t * Operand.t
             | Cset of Reg.t * Cond.t
             | Sel of selop * Reg.t * Reg.t * Reg.t * Cond.t
               (* ubfx d, s, #lsb, #width *)
             | Ubfx of Reg.t * Reg.t * int * int
               (* constants *)
             | MovImm of Reg.t * IntInf.t    (* mov r, #n *)
               (* movz/movk r, #n, lsl #shift *)
             | Movz of Reg.t * int * int
             | Movk of Reg.t * int * int
               (* addresses of symbols: adrp, and then either AddLo (the
                * address itself) or LoadGot (the address is loaded from the
                * global offset table)
                *)
             | Adrp of Reg.t * Label.t * {got: bool}
             | AddLo of Reg.t * Reg.t * Label.t
             | LoadGot of Reg.t * Reg.t * Label.t
             | Adr of Reg.t * Label.t
               (* memory *)
             | Load of loadop * Reg.t * Addr.t
             | Store of storeop * Reg.t * Addr.t
               (* status, value, address *)
             | StoreX of storexop * Reg.t * Reg.t * Addr.t
             | Pair of pairop * Reg.t * Reg.t * Addr.t
               (* control *)
             | B of Target.t
             | Bcond of Cond.t * Target.t
             | Cbz of Reg.t * Target.t
             | Cbnz of Reg.t * Target.t
             | Bl of Label.t
             | Blr of Reg.t
             | Br of Reg.t
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

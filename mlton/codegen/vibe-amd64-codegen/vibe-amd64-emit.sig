(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Building the instructions of a chunk, for the vibe-amd64 codegen: the list
 * that they are collected in, and the short sequences that depend only on
 * what x86-64 can encode (a constant that doesn't fit an immediate, a load
 * that extends, the address of a symbol, a real constant).  It knows
 * nothing of where the operands of the Machine IR live; that is
 * VibeAmd64Access.
 *
 * Registers are numbered: 0-15 are rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi,
 * r8-r15; 16-31 are xmm0-xmm15.
 *
 * An integer value that is narrower than 64 bits is kept zero-extended in
 * a register.
 *)

signature VIBE_AMD64_EMIT_STRUCTS =
   sig
      structure Asm: VIBE_AMD64_ASM
      structure Machine: MACHINE
   end

signature VIBE_AMD64_EMIT =
   sig
      include VIBE_AMD64_EMIT_STRUCTS

      val rax: int
      val rcx: int
      val rdx: int
      val rsp: int
      val rsi: int
      val rdi: int
      val r10: int
      val r11: int
      val xmm0: int
      val xmm1: int
      (* the registers that hold GCState, StackTop and Frontier *)
      val gcState: int
      val stackTop: int
      val frontier: int

      val isReal: Machine.CType.t -> bool
      (* in bytes *)
      val size: Machine.CType.t -> int
      (* the size of an integer operand that holds a value of type ct *)
      val sz: Machine.CType.t -> Asm.Size.t
      (* register r, as an operand of the size of a value of type ct *)
      val reg: int * Machine.CType.t -> Asm.Operand.t
      (* [b + disp] *)
      val based: int * int -> Asm.Addr.t

      (* 2^k *)
      val pow2: int -> IntInf.t
      (* k such that n = 2^k *)
      val log2: IntInf.t -> int option
      (* Can n be an immediate of a 64-bit instruction (it is sign-extended
       * from 32 bits)?
       *)
      val simm32: IntInf.t -> bool

      (* the instructions of one chunk, and what adds to them *)
      type t =
         {emit: Asm.Instr.t -> unit,
          (* an instruction: its mnemonic and its operands, in AT&T order *)
          ins: string * Asm.Operand.t list -> unit,
          label: Asm.Label.t -> unit,
          jmp: Asm.Label.t -> unit,
          (* ld (ct, r, a): load register r with the value of type ct at a,
           * zero-extended
           *)
          ld: Machine.CType.t * int * Asm.Addr.t -> unit,
          (* st (ct, r, a): store the value of type ct in register r to a *)
          st: Machine.CType.t * int * Asm.Addr.t -> unit,
          (* mov (d, s, ct): copy a register; nothing, if d = s *)
          mov: int * int * Machine.CType.t -> unit,
          (* r = n, as a 64-bit value *)
          movImm: int * IntInf.t -> unit,
          lea: Asm.Addr.t * int -> unit,
          (* Instructions that change the flags may not be output: the
           * flags hold a condition that is still to be used.
           *)
          flagsLive: bool ref,
          (* the address of a constant with the given bits, of 4 or 8 bytes *)
          literal: int * IntInf.t -> Asm.Addr.t,
          (* symAddr (r, s, {got}): r = the address of symbol s, directly
           * or from the global offset table
           *)
          symAddr: int * string * {got: bool} -> unit,
          (* what has been emitted, in order, followed by the literals *)
          instrs: unit -> Asm.Instr.t list}

      (* The name is that of the chunk: it makes the labels of its literals
       * unique.
       *)
      val new: {name: string} -> t
   end

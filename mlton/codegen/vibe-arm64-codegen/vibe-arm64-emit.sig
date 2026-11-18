(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Building the instructions of a chunk, for the vibe-arm64 codegen: the list
 * that they are collected in, and the short sequences that depend only on
 * what ARM64 can encode (a constant that needs several instructions, an
 * offset that is too large for an addressing mode, the address of a
 * symbol).  It knows nothing of where the operands of the Machine IR live;
 * that is VibeArm64Access.
 *
 * Registers are numbered: 0-31 are x0-x30,sp; 32-63 are d0-d31.
 *)

signature VIBE_ARM64_EMIT_STRUCTS =
   sig
      structure Arm64: ARM64
      structure Machine: MACHINE
   end

signature VIBE_ARM64_EMIT =
   sig
      include VIBE_ARM64_EMIT_STRUCTS

      (* integer register r (0-31), at 64 bits *)
      val x: int -> Arm64.Reg.t
      (* register r, at the width of a value of type ct *)
      val reg: int * Machine.CType.t -> Arm64.Reg.t
      val regOp: Arm64.Reg.t -> Arm64.Operand.t
      val immOp: int -> Arm64.Operand.t
      (* scratch register k for a value of type ct: x<k> or d<16+k> *)
      val scratch: Machine.CType.t * int -> int

      (* 2^k *)
      val pow2: int -> IntInf.t
      (* k such that n = 2^k *)
      val log2: IntInf.t -> int option
      (* Can n be the immediate of a logical instruction on N-bit
       * registers?
       *)
      val bitmask: IntInf.t * int -> bool

      (* the instructions of one chunk, and what adds to them *)
      type t =
         {emit: Arm64.Instr.t -> unit,
          label: Arm64.Label.t -> unit,
          branch: Arm64.Label.t -> unit,
          (* load/store register r, as a value of type ct *)
          ldr: Machine.CType.t * int * Arm64.Addr.t -> unit,
          str: Machine.CType.t * int * Arm64.Addr.t -> unit,
          (* mov (d, s, ct): nothing, if d = s *)
          mov: int * int * Machine.CType.t -> unit,
          (* r = n *)
          movImm: int * IntInf.t -> unit,
          movInt: int * int -> unit,
          (* addImm (d, s, n): d = s + n; may clobber x17 *)
          addImm: int * int * int -> unit,
          (* mem (b, off, ct): an addressing mode for [b + off], accessed
           * at type ct; may clobber x17
           *)
          mem: int * int * Machine.CType.t -> Arm64.Addr.t,
          (* symAddr (r, s, {got}): r = the address of symbol s, directly
           * or from the global offset table
           *)
          symAddr: int * string * {got: bool} -> unit,
          (* what has been emitted, in order *)
          instrs: unit -> Arm64.Instr.t list}

      val new: unit -> t
   end

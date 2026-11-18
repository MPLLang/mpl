(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Access to the operands of the Machine IR, for the vibe-arm codegen: where
 * the value of an operand is as a block is output (a register, the frame,
 * the ML stack, the heap), and the instructions that read and write it.
 * What uses it (the primitives, the transfers) doesn't know which register,
 * if any, an operand is in.
 *
 * A temporary is in the register of its web (see VibeArmAnalysis), or, if
 * it has none, in the frame.  A stack slot that is promoted in a loop is in
 * the register of its web.  Within a block, a stack slot that is read more
 * than once is cached in a register that no web is using there.
 *
 * The frame of a chunk: [sp] and [sp, #8] hold the int/real result of the
 * last C call; then the temporaries; then an area in which to save the
 * caller-save registers around calls.
 *
 * The operations are those of one chunk, and they have state: they must be
 * called in the order that the code is output.  For each block:
 * startBlock; then, for each item, the reads and writes of the item, and
 * "defined" for each of its destinations.
 *
 * A register argument or result is a number: 0-31 are x0-x30,sp; 32-63 are
 * d0-d31.
 *)

signature VIBE_ARM_ACCESS_STRUCTS =
   sig
      structure Emit: VIBE_ARM_EMIT
      structure Items: VIBE_ARM_ITEMS
      sharing Emit.Machine = Items.Analysis.Machine
   end

signature VIBE_ARM_ACCESS =
   sig
      include VIBE_ARM_ACCESS_STRUCTS

      val cty: Emit.Machine.Operand.t -> Emit.Machine.CType.t

      type t =
         {(* the size of the frame, in bytes *)
          frameSize: int,

          (* The block that is output next.  (Outputs nothing.) *)
          startBlock: Emit.Machine.Block.t -> unit,
          (* the label of that block *)
          curBlock: unit -> Emit.Machine.Label.t,
          (* What follows is output with the webs of this block: for the
           * moves of an arm of an if-converted switch, which are output
           * in the block of the switch.
           *)
          enterBlock: Emit.Machine.Label.t -> unit,
          (* the definition of an operand has been output *)
          defined: Emit.Machine.Operand.t -> unit,

          (* value (z, d, t): a register holding the value of z: the one
           * that it lives in, if any, and otherwise d, after loading it.
           * May clobber x<t> and up.
           *)
          value: Emit.Machine.Operand.t * int * int -> int,
          (* load (z, d, t): put the value of z in register d.  May clobber
           * x<t> and up.
           *)
          load: Emit.Machine.Operand.t * int * int -> unit,
          (* a register in which to compute the value of a destination *)
          dstReg: Emit.Machine.Operand.t -> int,
          (* store (dst, r): set dst to the value in register r *)
          store: Emit.Machine.Operand.t * int -> unit,
          move: {dst: Emit.Machine.Operand.t,
                 src: Emit.Machine.Operand.t} -> unit,
          (* setDst (dst, old, set): set dst with the instruction that set
           * outputs, given a register for dst; if old, the register holds
           * the value of dst, for an instruction that may keep it.  Also
           * does "defined dst".
           *)
          setDst: Emit.Machine.Operand.t * bool * (int -> unit) -> unit,

          (* Forget which registers hold stack slots: StackTop is about to
           * change, or their registers are about to be written.
           *)
          clearCache: unit -> unit,
          (* the same, and don't cache any more in this block *)
          noCache: unit -> unit,

          (* sp has been lowered by this much, to pass C arguments *)
          setSpAdj: int -> unit,
          (* Before a C call that returns to the given block (if it
           * returns to one): save the caller-save registers that are live
           * there.  Returns what reloads them.
           *)
          saveForCall: (Emit.Machine.Label.t option
                        -> {restore: unit -> unit}),
          (* The call has been output: forget what it clobbered.  (Outputs
           * nothing.)
           *)
          afterCall: unit -> unit,
          (* Call a runtime function in the middle of an item, saving all
           * of the caller-save registers that hold anything around it.
           * The caller sets up the arguments and takes the result.
           *)
          callRuntime: string -> unit}

      val new: {analysis: Items.Analysis.t,
                emit: Emit.t,
                labelIndex: Emit.Machine.Label.t -> int,
                (* the number of temporaries of each type *)
                tempsMax: Emit.Machine.CType.t -> int} -> t
   end

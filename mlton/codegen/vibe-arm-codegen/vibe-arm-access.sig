(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Access to the operands of the Machine IR, for the vibe-arm codegen: where
 * the value of an operand is as a block is output (a register, the frame,
 * the ML stack, the heap), and the instructions that read and write it.
 * Everything that differs between the two register allocators is in here;
 * what uses this (the primitives, the transfers) is the same for both.
 *
 *  - The graph allocator: a temporary is in the register of its web (see
 *    VibeArmAnalysis), or, if it has none, in the frame.  Within a block, a
 *    stack slot that is read more than once is cached in a register that
 *    no web is using there.
 *  - The local allocator: every temporary has a home in the frame, and the
 *    registers are a cache of temporaries and stack slots that is filled
 *    on demand as a block is output.  What the registers hold at the start
 *    of a block is fixed by the first of its predecessors to be output;
 *    the other predecessors conform, with code on the edge if need be.
 *
 * The frame of a chunk: [sp] and [sp, #8] hold the int/real result of the
 * last C call; then the temporaries; then an area in which to save the
 * caller-save registers around calls.
 *
 * The operations are those of one chunk, and they have state: they must be
 * called in the order that the code is output.  For each block:
 * startBlock; then, for each item k (the transfer is the last),
 * beginItem k, the reads and writes of the item, "defined" for each of its
 * destinations, and endItem.
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

      (* (Local allocator.)  What the registers hold. *)
      type regs

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
          beginItem: int -> unit,
          endItem: unit -> unit,
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
          (* memOf (z, t, ct): an addressing mode for a memory operand that
           * is accessed at type ct.  May clobber x<t> and up.
           *)
          memOf: (Emit.Machine.Operand.t * int * Emit.Machine.CType.t
                  -> Emit.Arm64.Addr.t),
          (* a register in which to compute the value of a destination *)
          dstReg: Emit.Machine.Operand.t -> int,
          (* (Graph allocator.)  The register of a temporary that is
           * about to be defined.
           *)
          defHome: Emit.Machine.Temporary.t -> int option,
          (* store (dst, r): set dst to the value in register r *)
          store: Emit.Machine.Operand.t * int -> unit,
          move: {dst: Emit.Machine.Operand.t,
                 src: Emit.Machine.Operand.t} -> unit,

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
          callRuntime: string -> unit,

          (* (Local allocator; with the graph allocator, these do nothing,
           * and edgeNeedsCode is false.)
           *
           * Does the edge from here to a block need code, to make the
           * registers be what the block expects?  If nothing has fixed
           * what it expects, this does: what they hold now.
           *)
          edgeNeedsCode: Emit.Machine.Label.t -> bool,
          (* Before going to a block at the end of this one: output that
           * code.
           *)
          beforeGoto: Emit.Machine.Label.t -> unit,
          (* a copy of what the registers hold now *)
          copyRegs: unit -> regs,
          (* output that code elsewhere: from when the registers held this *)
          conformFrom: regs * Emit.Machine.Label.t -> unit}

      val new: {analysis: Items.Analysis.t,
                emit: Emit.t,
                labelIndex: Emit.Machine.Label.t -> int,
                (* which register allocator *)
                localRA: bool,
                (* the number of temporaries of each type *)
                tempsMax: Emit.Machine.CType.t -> int} -> t
   end

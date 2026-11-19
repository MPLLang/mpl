(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Access to the operands of the Machine IR, for the vibe-amd64 codegen: where
 * the value of an operand is as a block is output (a register, the frame,
 * the ML stack, the heap), and the instructions that read and write it.
 *
 * A temporary is in the register of its web (see VibeAmd64Analysis), or, if
 * it has none, in the frame.  A stack slot that is promoted in a loop is in
 * the register of its web.  Within a block, a stack slot that is read more
 * than once is cached in a register that no web is using there.
 *
 * Unlike a RISC, x86-64 can use an operand where it lives: "opnd" gives a
 * register, an immediate or a memory operand.  A memory operand may need
 * registers for its address; those are taken from a list of scratch
 * registers that the caller says are free (some of rax, rcx and rdx, which
 * never hold a temporary).
 *
 * The frame is the same for all chunks (see VibeAmd64Codegen), from rsp up:
 * the stack arguments of C calls; the int and the real result of the last C
 * call; slots for shuffling the arguments of a C call; an area in which to
 * save the caller-save registers around calls; the temporaries.
 *
 * The operations are those of one chunk, and they have state: they must be
 * called in the order that the code is output.  For each block:
 * startBlock; then, for each item, the reads and writes of the item, and
 * "defined" for each of its destinations.
 *)

signature VIBE_AMD64_ACCESS_STRUCTS =
   sig
      structure Emit: VIBE_AMD64_EMIT
      structure Items: VIBE_AMD64_ITEMS
      sharing Emit.Machine = Items.Analysis.Machine
   end

signature VIBE_AMD64_ACCESS =
   sig
      include VIBE_AMD64_ACCESS_STRUCTS

      val cty: Emit.Machine.Operand.t -> Emit.Machine.CType.t

      (* what the frame must hold besides the temporaries: the bytes of
       * stack arguments, and the number of shuffle slots
       *)
      type frame = {outArgs: int, shuffle: int}
      (* the size of the frame that a chunk needs *)
      val frameNeed: {frame: frame,
                      tempsMax: Emit.Machine.CType.t -> int} -> int

      type t =
         {(* The block that is output next.  (Outputs nothing.) *)
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

          (* the register that holds the value of an operand, if one does
           * (outputs nothing)
           *)
          home: Emit.Machine.Operand.t -> int option,
          (* the registers that reading an operand reads, other than
           * scratch registers (outputs nothing)
           *)
          reads: Emit.Machine.Operand.t -> int list,
          (* does reading the operand need a scratch register? *)
          needsScratch: Emit.Machine.Operand.t -> bool,
          (* A constant that can be an immediate of an instruction on
           * values of its type.
           *)
          immOf: Emit.Machine.Operand.t -> IntInf.t option,
          (* opnd (z, scr, {imm}): an operand of an instruction for the
           * value of z, at the size of its type: a register, memory or, if
           * imm, an immediate.  May clobber the scratch registers scr, of
           * which there must be at least one.
           *)
          opnd: (Emit.Machine.Operand.t * int list * {imm: bool}
                 -> Emit.Asm.Operand.t),
          (* the address of an operand that is in memory *)
          addr: Emit.Machine.Operand.t * int list -> Emit.Asm.Addr.t,
          (* regOf (z, d, scr): a register holding the value of z: the one
           * that it lives in, if any, and otherwise d, after loading it.
           *)
          regOf: Emit.Machine.Operand.t * int * int list -> int,
          (* load (z, d, scr): put the value of z in register d.  A narrow
           * integer is zero-extended.
           *)
          load: Emit.Machine.Operand.t * int * int list -> unit,
          (* a register in which to compute the value of a destination, if
           * it lives in one
           *)
          dstReg: Emit.Machine.Operand.t -> int option,
          (* store (dst, r, {clean}): set dst to the value in register r.
           * If not clean, the bits of r above the size of dst may be
           * anything.  May clobber the scratch registers other than r, but
           * not the flags.
           *)
          store: Emit.Machine.Operand.t * int * {clean: bool} -> unit,
          move: {dst: Emit.Machine.Operand.t,
                 src: Emit.Machine.Operand.t} -> unit,
          (* setDst (dst, old, set): set dst with the instructions that set
           * outputs, given a register for dst; if old, the register holds
           * the value of dst, for an instruction that may keep it.  Also
           * does "defined dst".  Uses rax, and doesn't change the flags.
           *)
          setDst: Emit.Machine.Operand.t * bool * (int -> unit) -> unit,

          (* Forget which registers hold stack slots: StackTop is about to
           * change, or their registers are about to be written.
           *)
          clearCache: unit -> unit,
          (* Don't cache any more in this block.  (What is cached stays.) *)
          noCache: unit -> unit,

          (* in the frame: the int and the real result of a C call, and
           * the shuffle slots
           *)
          resInt: Emit.Asm.Addr.t,
          resReal: Emit.Asm.Addr.t,
          shuffle: int -> Emit.Asm.Addr.t,
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
          (* callRuntime (name, setup): call a runtime function in the
           * middle of an item, saving all of the caller-save registers
           * that hold anything around it.  setup puts the arguments in
           * place; it may read the operands of the item.  The result is
           * the caller's to take.
           *)
          callRuntime: string * (unit -> unit) -> unit}

      val new: {analysis: Items.Analysis.t,
                emit: Emit.t,
                frame: frame,
                labelIndex: Emit.Machine.Label.t -> int,
                (* the number of temporaries of each type *)
                tempsMax: Emit.Machine.CType.t -> int} -> t
   end

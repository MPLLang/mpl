(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* The analysis of a chunk that the vibe-arm codegen does before it outputs
 * any code.
 *
 *  - Speculation: cheap computations at the start of the arms of a two-way
 *    switch are hoisted into the block of the switch, so that more switches
 *    can be if-converted (output as conditional selects).  The blocks that
 *    the analysis returns are the ones to output.
 *  - Regions: the blocks are partitioned into regions connected by "near"
 *    transfers (ones that stay in the chunk function and don't go through
 *    an entry block); no temporary is live between regions.
 *  - Loops: the loop forest of the control-flow graph of near transfers.
 *  - Webs: each temporary is split into its webs, the live ranges that are
 *    connected by definitions reaching uses.  The webs are colored: each
 *    gets a register, or none (it then lives in the frame).
 *  - Promotion: the most-read stack slots of a loop get webs, and so
 *    registers, of their own, that are live throughout the loop.
 *
 * The comments in the implementation say how each is done.
 *)

signature VIBE_ARM_ANALYSIS_STRUCTS =
   sig
      structure Machine: MACHINE
   end

signature VIBE_ARM_ANALYSIS =
   sig
      include VIBE_ARM_ANALYSIS_STRUCTS

      (* Registers are numbered: 0-31 are x0-x30,sp; 32-63 are d0-d31.
       * These are the ones that temporaries and stack slots may be given,
       * in order of preference.
       *)
      val caller: int list     (* x8-x15 *)
      val callee: int list     (* x22-x28 *)
      val fcaller: int list    (* d19-d31 *)
      val fcallee: int list    (* d8-d15 *)
      val callerSave: int -> bool

      val isReal: Machine.CType.t -> bool
      (* in bytes *)
      val size: Machine.CType.t -> int

      val dstTemp: Machine.Operand.t -> Machine.Temporary.t option
      (* the temporaries that an operand reads (or, if it is one, is) *)
      val foreachTemp:
         Machine.Operand.t * (Machine.Temporary.t -> unit) -> unit
      val stripCast: Machine.Operand.t -> Machine.Operand.t
      (* View a switch as a list of cases and a default target. *)
      val switchCases: ('a * 'b) vector * 'b option -> ('a * 'b) list * 'b

      (* a stack slot: its offset and type *)
      type slot = int * Machine.CType.t
      val slotKey: Machine.StackOffset.t -> slot
      (* the same bytes, in the same kind of register *)
      val sameSlot: slot * slot -> bool

      structure Temp:
         sig
            (* a temporary of a region *)
            type t

            (* is it live across some C call? *)
            val across: t -> bool
            val ct: t -> Machine.CType.t
            val equals: t * t -> bool
            val index: t -> int
         end

      structure Region:
         sig
            type t

            (* a region with no temporaries *)
            val new: unit -> t
            val temp: t * Machine.Temporary.t -> Temp.t
            val temps: t -> Temp.t list
         end

      structure Web:
         sig
            type t

            (* its register, if it has one *)
            val home: t -> int option
         end

      type t =
         {(* the blocks to output: those of the chunk, after speculation *)
          blocks: Machine.Block.t vector,
          labelBlock: Machine.Label.t -> Machine.Block.t,
          blockRegion: Machine.Label.t -> Region.t,
          blockWebs:
          Machine.Label.t
          -> {(* the webs of the temporaries at the start of the block *)
              segIn: (Temp.t * Web.t) list,
              (* the webs of its definitions, in order *)
              defs: (Temp.t * Web.t) list,
              (* the stack slots that are kept in registers throughout the
               * loop that the block is in
               *)
              promoted: (slot * Web.t) list,
              (* which such loop; 0 if none *)
              ploop: int,
              (* Is the block entered from outside that loop?  If so, it
               * needs a preheader, to load the slots.
               *)
              pheader: bool},
          (* If a switch can be output as conditional selects: the moves of
           * each of its two arms, and the block that both go on to.
           *)
          ifConvert:
          Machine.Transfer.t
          -> {caseLabel: Machine.Label.t,
              caseMoves: {dst: Machine.Operand.t,
                          src: Machine.Operand.t} list,
              defaultLabel: Machine.Label.t,
              defaultMoves: {dst: Machine.Operand.t,
                             src: Machine.Operand.t} list,
              join: Machine.Label.t} option,
          (* the temporaries that are live at the start of a block *)
          liveTemps: Machine.Block.t -> Temp.t list,
          (* How much deeper in the loop nest is the second block than the
           * first?  NONE if either isn't in a loop, or they are in
           * unrelated loops.
           *)
          loopDistance: Machine.Label.t * Machine.Label.t -> int option,
          (* the targets of the near transfers of a transfer *)
          nearSuccessors: Machine.Transfer.t -> Machine.Label.t list,
          (* the web of a temporary that is live at the start of a block *)
          segIn: Machine.Label.t * Temp.t -> Web.t}

      val analyze: {blocks: Machine.Block.t vector,
                    labelBlock: Machine.Label.t -> Machine.Block.t} -> t
   end

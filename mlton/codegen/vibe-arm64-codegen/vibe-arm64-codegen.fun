(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* A simple native codegen for ARM64 (AArch64).
 *
 * This codegen mirrors the structure of the C codegen: each chunk is a
 * function
 *    uintptr_t Chunk (CPointer gcState, CPointer stackTop,
 *                     CPointer frontier, uintptr_t nextBlock)
 * that is driven by the trampoline in c-main.h.  Only the chunks are emitted
 * as assembly; the main file is emitted (as C) by the C codegen.
 *
 * Register conventions within a chunk:
 *    x19 = GCState, x20 = StackTop, x21 = Frontier
 *    x8-x15, x22-x28, d8-d15, d19-d31: temporaries (see "Register
 *       allocation", below)
 *    x0-x2, d16-d18: operands/results of the current statement
 *    x3-x4: address computations
 *    x5-x7: scratch for multi-instruction primitives
 *    x16-x17: constants, large offsets, C call setup
 * Integer values narrower than 64 bits are kept zero-extended in registers.
 *
 * Register allocation.  The backend has already packed variables into
 * Machine temporaries (pseudo-registers), such that temporaries that are
 * simultaneously live are distinct, and temporaries are never live across a
 * transfer that leaves the chunk function or re-enters it at an entry block.
 * So, the blocks of a chunk are partitioned into regions connected by "near"
 * transfers (Goto, Switch, C calls that don't have a frame); no temporary is
 * live between regions.  Within each region, we build an interference graph
 * (by a backward scan of each block, starting from the live temporaries that
 * the backend recorded for the successor blocks) and color it greedily, in
 * order of use count; temporaries that can't be colored live in the (native)
 * stack frame.  As in the x86/amd64 codegens, every temporary has a home in
 * memory and caller-save registers are committed to memory around C calls,
 * but only those that are live across the call.  Temporaries that are live
 * across some C call prefer callee-save registers.
 *
 * The regions, the webs of temporaries and their registers are computed for a whole chunk before any of its code is
 * output: that is VibeArm64Analysis (see vibe-arm64-analysis.sig).
 *
 * Output.  The codegen builds a list of ARM64 instructions for each chunk
 * (see arm64.sig); printing them as assembly is the business of Arm64.
 *
 * The parts: VibeArm64Emit collects the instructions; VibeArm64Access reads
 * and writes the operands of the Machine IR; VibeArm64Prim outputs the primitives; VibeArm64Items combines
 * statements; VibeArm64Layout orders the blocks.  What is here is the frame
 * of a chunk function, the transfers, and the loop over the blocks.
 *)

functor VibeArm64Codegen (S: VIBE_ARM64_CODEGEN_STRUCTS): VIBE_ARM64_CODEGEN =
struct

open S
open Machine

structure Asm = Arm64 ()
structure Addr = Asm.Addr
structure Cond = Asm.Cond
structure I = Asm.Instr
structure Lbl = Asm.Label
structure Opnd = Asm.Operand
structure R = Asm.Reg
structure Tgt = Asm.Target

structure Analysis = VibeArm64Analysis (structure Machine = Machine)
structure Web = Analysis.Web
structure Items = VibeArm64Items (structure Analysis = Analysis)
structure Layout = VibeArm64Layout (structure Analysis = Analysis)
structure Emit = VibeArm64Emit (structure Arm64 = Asm
                              structure Machine = Machine)
structure Access = VibeArm64Access (structure Emit = Emit
                                  structure Items = Items)
structure PrimApp = VibeArm64Prim (structure Access = Access)
val isReal = Analysis.isReal
val size = Analysis.size
val switchCases = Analysis.switchCases
val x = Emit.x
val reg = Emit.reg
val regOp = Emit.regOp
val immOp = Emit.immOp
val scratch = Emit.scratch
val cty = Access.cty

structure GCField = Runtime.GCField

fun implementsPrim (p: 'a Prim.t): bool =
   let
      datatype z = datatype Prim.t
      fun big ws = Bits.toInt (WordSize.bits ws) >= 32
   in
      (* Unimplemented primitives become calls to the runtime's C functions. *)
      case p of
         Real_Math_acos _ => false
       | Real_Math_asin _ => false
       | Real_Math_atan _ => false
       | Real_Math_atan2 _ => false
       | Real_Math_cos _ => false
       | Real_Math_exp _ => false
       | Real_Math_ln _ => false
       | Real_Math_log10 _ => false
       | Real_Math_sin _ => false
       | Real_Math_tan _ => false
       | Real_ldexp _ => false
       | Word_rndToReal _ => true
       | Word_rol ws => big ws
       | Word_ror ws => big ws
       | _ => CCodegen.implementsPrim p
   end

(* Each entry block has a global symbol for its entry stub, which sets up the
 * (native) stack frame of its chunk before falling into the block.  The
 * addresses of the stubs are collected in a table that is indexed by frame
 * index (that is, by "return address").
 *)
fun entryStub l = "vibeArm64Entry_" ^ Label.toString l
val entryTable = "vibeArm64Entries"

fun outputChunk (Chunk.T {blocks, chunkLabel, tempsMax},
                 {labelBlock: Label.t -> Block.t,
                  labelChunk: Label.t -> ChunkLabel.t,
                  labelIndex: Label.t -> int}): I.t list =
   let
      val int = CCodegen.C.int
      val darwin = !Control.Target.os = MLton.Platform.OS.Darwin
      val e as {emit, label, branch, ldr, str, movImm, movInt, addImm, mem,
                symAddr, instrs, ...} = Emit.new ()
      val self = ChunkLabel.toString chunkLabel
      val leave = Lbl.Plain ("Lleave_" ^ self)
      (* The label of a block, and of the preheader that loads its promoted
       * stack slots.  By default they are local to the assembler; with
       * -vibe-arm64-symbols true, each is a symbol, so that tools (a
       * profiler, a debugger) can name the block that an address is in.
       *)
      val symbols = !Control.vibeArm64Symbols
      fun blockLabel l =
         if symbols then Lbl.Sym (Label.toString l)
         else Lbl.Local (Label.toString l)
      fun preLabel l =
         if symbols then Lbl.Sym ("pre_" ^ Label.toString l)
         else Lbl.Plain ("Lpre_" ^ Label.toString l)

      (* The analysis of the chunk (see vibe-arm64-analysis.sig): speculation,
       * regions, loops and, for the graph allocator, the registers of the
       * webs of temporaries and of promoted stack slots.  The blocks that
       * it returns are the ones to output.
       *)
      val analysis as {blocks, labelBlock, blockWebs, ifConvert, ...} =
         Analysis.analyze {blocks = blocks, labelBlock = labelBlock}
      val access as {frameSize, startBlock, curBlock, enterBlock, defined,
                     value, load, dstReg, store, move, setDst, clearCache,
                     noCache, setSpAdj, saveForCall, afterCall, ...} =
         Access.new {analysis = analysis, emit = e, labelIndex = labelIndex,
                     tempsMax = tempsMax}
      val prim = {access = access, emit = e}
      val webHome = Web.home
      (* The label to branch to to get to l: the preheader of a loop with
       * promoted slots, if coming from outside the loop.
       *)
      fun target l =
         let
            val {ploop, pheader, ...} = blockWebs l
         in
            if ploop <> 0
               andalso ploop <> #ploop (blockWebs (curBlock ()))
               then if pheader
                       then preLabel l
                    else Error.bug "VibeArm64Codegen.target: no preheader"
            else blockLabel l
         end

      val saved = [(19, 20), (21, 22), (23, 24), (25, 26), (27, 28),
                   (40, 41), (42, 43), (44, 45), (46, 47)]

      fun gcField f = Operand.gcField f
      fun flushFrontier () =
         move {dst = gcField GCField.Frontier, src = Operand.Frontier}
      fun flushStackTop () =
         move {dst = gcField GCField.StackTop, src = Operand.StackTop}
      val jumpTables = ref 0
      (* the block that is laid out after the one being output *)
      val next: Label.t option ref = ref NONE
      fun isNext l =
         case !next of
            SOME l' => Label.equals (l, l')
          | NONE => false
      fun goto l =
         if isNext l andalso Lbl.equals (target l, blockLabel l)
            then ()
         else branch (target l)
      (* Conditional branches only reach +-1MB. *)
      val far =
         Vector.fold (blocks, 0, fn (Block.T {statements, ...}, n) =>
                      n + 1 + Vector.length statements) > 8000
      datatype cond =
         CC of Cond.t           (* condition code *)
       | Z of R.t * bool        (* register is (true) or isn't (false) zero *)
      val negate =
         fn CC c => CC (Cond.negate c)
          | Z (r, b) => Z (r, not b)
      fun bcond (c, l: Lbl.t) =
         let
            fun br (c, t) =
               emit (case c of
                        CC c => I.Bcond (c, t)
                      | Z (r, true) => I.Cbz (r, t)
                      | Z (r, false) => I.Cbnz (r, t))
         in
            if far
               then (br (negate c, Tgt.Forward 1)
                     ; branch l
                     ; emit (I.LocalLabel 1))
            else br (c, Tgt.Label l)
         end
      (* if c then goto l else goto l' *)
      fun twoWay (c, l, l') =
         if isNext l
            then (bcond (negate c, target l'); goto l)
         else (bcond (c, target l); goto l')
      (* Output an if-converted switch; the flags hold condition c, which
       * selects the case (rather than the default) target.
       *)
      fun selects (c, {caseLabel, caseMoves, defaultLabel, defaultMoves,
                       join}) =
         let
            (* The selects write temporaries of other blocks, whose registers
             * the cache may be using.
             *)
            val () = noCache ()
            fun select c {dst, src} =
               let
                  val ct = cty dst
                  (* Select the whole register, because it may be shared
                   * with a wider temporary that is live when c is false.
                   *)
                  val wide = if isReal ct then CType.Real64 else CType.Word64
                  val s = value (src, scratch (ct, 1), 3)
               in
                  setDst (dst, true, fn d =>
                          emit (I.Sel (if isReal ct then I.FCSEL else I.CSEL,
                                       reg (d, wide), reg (s, wide),
                                       reg (d, wide), c)))
               end
            (* the moves of an arm are output in the context of its block *)
            fun arm (l, moves, c) =
               if List.isEmpty moves then ()
               else (enterBlock l; List.foreach (moves, select c))
            val c' = Cond.negate c
            fun bool z =
               case z of
                  Operand.Const (Const.Word w) =>
                     (case WordX.toIntInf w of
                         0 => SOME false
                       | 1 => SOME true
                       | _ => NONE)
                | _ => NONE
         in
            (* a choice between 0 and 1 is the condition itself *)
            (case (caseMoves, defaultMoves) of
                ([{dst, src}], [{dst = dst', src = src'}]) =>
                   (case (Operand.equals (dst, dst'), bool src, bool src') of
                       (true, SOME b, SOME b') =>
                          if b <> b'
                             then (enterBlock caseLabel
                                   ; setDst (dst, false, fn d =>
                                             emit (I.Cset
                                                   (reg (d, CType.Word64),
                                                    if b then c else c')))
                                   ; enterBlock defaultLabel
                                   ; defined dst')
                          else (arm (caseLabel, caseMoves, c)
                                ; arm (defaultLabel, defaultMoves, c'))
                     | _ => (arm (caseLabel, caseMoves, c)
                             ; arm (defaultLabel, defaultMoves, c')))
              | _ => (arm (caseLabel, caseMoves, c)
                      ; arm (defaultLabel, defaultMoves, c')))
            ; goto join
         end
      (* return x0 to the trampoline *)
      fun leaveChunk () = (flushFrontier (); flushStackTop (); branch leave)
      (* Transfers between chunks don't go through the trampoline: a
       * transfer to a known entry block of another chunk branches to its
       * entry stub.  This relies on every chunk running in the same frame
       * of saved registers (x29) with x19-x21 set.
       *)
      fun jump l =
         if ChunkLabel.equals (labelChunk l, chunkLabel)
            then goto l
         else branch (Lbl.Sym (entryStub l))
      fun push (return, size) =
         let
            val size = Bytes.toInt size
         in
            (* only x17 is available: C arguments may already be in place *)
            clearCache ()
            ; addImm (20, 20, size)
            ; movInt (17, labelIndex return)
            ; emit (I.Store (I.STR, R.X 17, Addr.Offset (R.X 20, ~8)))
            ; (case labelBlock return of
                  Block.T {kind, ...} =>
                     (case Option.map (Kind.frameInfoOpt kind,
                                       FrameInfo.sporkInfo) of
                         SOME (SOME _) =>
                            (* push StackTop on the promotion stack *)
                            let
                               val pst = gcField GCField.PromoStackTop
                            in
                               load (pst, 17, 17)
                               ; emit (I.Store (I.STR, R.X 20,
                                                Addr.PostIndex (R.X 17, 8)))
                               ; store (pst, 17)
                            end
                       | _ => ()))
         end
      fun pop (fi, {promo}) =
         (clearCache ()
          ; addImm (20, 20, ~ (Bytes.toInt (FrameInfo.size fi)))
          ; if promo andalso Option.isSome (FrameInfo.sporkInfo fi)
               then
                  let
                     val pst = gcField GCField.PromoStackTop
                     val psb = gcField GCField.PromoStackBot
                  in
                     (* not x0: a C call's result may be in it (see
                      * directReturn)
                      *)
                     load (pst, 5, 3)
                     ; emit (I.Bin (I.SUB, R.X 5, R.X 5, immOp 8))
                     ; store (pst, 5)
                     ; load (psb, 6, 3)
                     ; emit (I.Cmp (I.CMP, R.X 6, regOp (R.X 5)))
                     ; emit (I.Bcond (Cond.LS, Tgt.Forward 1))
                     ; store (psb, 5)
                     ; emit (I.LocalLabel 1)
                  end
            else ())
      (* transfer to the entry block whose index is on top of the stack *)
      fun indJump () =
         (emit (I.Load (I.LDR, R.X 16, Addr.Offset (R.X 20, ~8)))
          ; symAddr (17, entryTable, {got = false})
          ; emit (I.Load (I.LDR, R.X 16, Addr.Index (R.X 17, R.X 16, 3)))
          ; emit (I.Br (R.X 16)))
      fun rtrans rsTo =
         case rsTo of
            l :: rsTo' =>
               if List.forall (rsTo', fn l' => Label.equals (l, l'))
                  then jump l
               else indJump ()
          | [] => indJump ()
      (* Where a C call leaves its result for the block that it returns to:
       * in x0/d0 if directReturn, else in the frame, at [sp] or [sp, #8].
       *
       * A CReturn block is reached only as the return of a C call to its
       * function (the Machine IR allows Goto and Switch only to Jump
       * blocks): directly from the call, or, if the function may switch
       * threads, through the entry table.  When neither the call nor the
       * block goes through the entry table, every predecessor arrives with
       * the result still in x0/d0.  Nothing that is output between the call
       * and the block's use of the result touches x0 or d0:
       *  - after the call: the adjustment of sp (x17), the reloads of the
       *    caller-save registers (x8-x15, d19-d31), of Frontier and of
       *    StackTop;
       *  - the preheader of the block, if it heads a loop with promoted
       *    slots: loads into allocated registers (and x17);
       *  - pop, in the header: x5, x6 and x17.
       * That a call may collect doesn't matter: it still returns to its
       * call site.
       *)
      fun directReturn (l: Label.t): bool =
         case labelBlock l of
            Block.T {kind as Kind.CReturn {func, ...}, ...} =>
               not (CFunction.maySwitchThreadsFrom func)
               andalso not (Kind.isEntry kind)
          | _ => false
      (* Runtime functions that are one instruction.  (The C codegen gets
       * them inline from the C compiler.)  The result goes where the block
       * that the call returns to expects it.
       *)
      fun inlineCCall (args, CFunction.T {prototype = (_, retTy), target, ...},
                       return) =
         case (target, Vector.toList args, retTy, return) of
            (CFunction.Target.Direct name, [z], SOME ct,
             SOME {return, size = NONE}) =>
               (case List.peek ([("_realCeil", I.FRINTP),
                                 ("_realFloor", I.FRINTM),
                                 ("_realTrunc", I.FRINTZ)], fn (f, _) =>
                                List.exists (["Real32", "Real64"], fn r =>
                                             name = r ^ f)) of
                   NONE => false
                 | SOME (_, opc) =>
                      let
                         val r = scratch (ct, 0)
                      in
                         if directReturn return
                            then emit (I.Un (opc, reg (32, ct),
                                             reg (value (z, r, 16), ct)))
                         else (load (z, r, 16)
                               ; emit (I.Un (opc, reg (r, ct), reg (r, ct)))
                               ; str (ct, r, Addr.Offset (R.SP, 8)))
                         ; goto return
                         ; true
                      end)
          | _ => false
      fun ccall (args, func as CFunction.T {prototype = (ctys, retTy),
                                             target, ...}, return) =
         if inlineCCall (args, func, return) then ()
         else
         let
            datatype z = datatype CFunction.Target.t
            val args = Vector.toList args
            val (fptr, args) =
               case (target, args) of
                  (Indirect, fptr :: args) => (SOME fptr, args)
                | _ => (NONE, args)
            (* Save the caller-save registers that are live across the
             * call.
             *)
            val {restore} = saveForCall (Option.map (return, #return))
            (* Assign argument locations: a register or 64 + stack offset. *)
            val (locs, _, _, stk) =
               List.fold2
               (args, Vector.toList ctys, ([], 0, 32, 0),
                fn (z, ct, (locs, ni, nf, stk)) =>
                if isReal ct andalso nf < 40
                   then ((z, ct, nf) :: locs, ni, nf + 1, stk)
                else if not (isReal ct) andalso ni < 8
                   then ((z, ct, ni) :: locs, ni + 1, nf, stk)
                else
                   let
                      (* Darwin packs stack arguments. *)
                      val ct = if darwin then ct
                               else if isReal ct then CType.Real64
                               else CType.Word64
                      val n = size ct
                      val stk = n * ((stk + n - 1) div n)
                   in
                      ((z, ct, 64 + stk) :: locs, ni, nf, stk + n)
                   end)
            val stk = 16 * ((stk + 15) div 16)
            val () = addImm (31, 31, ~ stk)
            val () = setSpAdj stk
            val () =
               List.foreach
               (locs, fn (z, ct, loc) =>
                let
                   val r = if loc >= 64 then scratch (ct, 0) else loc
                   val () = load (z, r, 16)
                   val () =
                      case ct of
                         CType.Int8 => emit (I.Un (I.SXTB, R.W r, R.W r))
                       | CType.Int16 => emit (I.Un (I.SXTH, R.W r, R.W r))
                       | _ => ()
                in
                   if loc >= 64
                      then str (ct, r, mem (31, loc - 64, ct))
                   else ()
                end)
            val () = Option.app (fptr, fn z => load (z, 16, 16))
            val () = setSpAdj 0
            val () =
               case return of
                  SOME {return, size = SOME size} => push (return, size)
                | _ => ()
            val () = if CFunction.modifiesFrontier func then flushFrontier () else ()
            val () = if CFunction.readsStackTop func then flushStackTop () else ()
            val () =
               case target of
                  Direct name =>
                     emit
                     (I.Bl
                      (Lbl.Sym
                       (case name of
                           (* static inline wrappers in c-chunk.h *)
                           "GC_readBarrier" => "Assignable_readBarrier"
                         | "GC_writeBarrier" => "Assignable_writeBarrier"
                         | _ => name)))
                | Indirect => emit (I.Blr (R.X 16))
            val () = afterCall ()
            val () = addImm (31, 31, stk)
            val () =
               case (retTy, return) of
                  (NONE, _) => ()
                | (SOME ct, SOME {return, ...}) =>
                     if directReturn return
                        then ()
                     else if isReal ct
                             then emit (I.Store (I.STR, R.D 0,
                                                 Addr.Offset (R.SP, 8)))
                          else emit (I.Store (I.STR, R.X 0, Addr.Base R.SP))
                | (SOME _, NONE) => ()
            val () = restore ()
            val () =
               if CFunction.modifiesFrontier func
                  then move {dst = Operand.Frontier,
                             src = gcField GCField.Frontier}
               else ()
            val () =
               if CFunction.writesStackTop func
                  then move {dst = Operand.StackTop,
                             src = gcField GCField.StackTop}
               else ()
         in
            if CFunction.maySwitchThreadsFrom func
               then indJump ()
            else (case return of
                     NONE => (movInt (0, ~2); branch leave)
                   | SOME {return, ...} => goto return)
         end
      fun outputTransfer t =
         case t of
            Transfer.CCall {func = CFunction.T {target = CFunction.Target.Direct
                                                         "Thread_returnToC", ...},
                            return = SOME {return, size = SOME size}, ...} =>
               (push (return, size)
                ; movInt (0, ~1)
                ; leaveChunk ())
          | Transfer.CCall {args, func, return} => ccall (args, func, return)
          | Transfer.Call {label, return, ...} =>
               (Option.app (return, fn {return, size, ...} => push (return, size))
                ; jump label)
          | Transfer.Goto l => goto l
          | Transfer.Raise {raisesTo} =>
               (clearCache ()
                ; emit (I.Bin
                        (I.ADD, R.X 20,
                         x (value (gcField GCField.StackBottom, 0, 3)),
                         regOp (x (value (gcField GCField.ExnStack, 1, 3)))))
                ; rtrans raisesTo)
          | Transfer.Return {returnsTo} => rtrans returnsTo
          | Transfer.Spork {cont, ...} => goto cont
          | Transfer.Switch (Switch.T {cases, default, test, ...}) =>
               let
                  val r = value (test, 0, 3)
                  val (cases, default) = switchCases (cases, default)
                  (* the condition that r = w *)
                  fun eq w =
                     let
                        val n = WordX.toIntInf w
                     in
                        if n = 0
                           then Z (x r, true)
                        else (if n < 4096
                                 then emit (I.Cmp (I.CMP, x r, Opnd.Imm n))
                              else (movImm (16, n)
                                    ; emit (I.Cmp (I.CMP, x r, regOp (R.X 16))))
                              ; CC Cond.EQ)
                     end
               in
                  case cases of
                     [(w, l)] =>
                        (case ifConvert t of
                            NONE => twoWay (eq w, l, default)
                          | SOME z =>
                               selects (case eq w of
                                           CC c => c
                                         | Z (r, b) =>
                                              (emit (I.Cmp (I.CMP, r, immOp 0))
                                               ; if b then Cond.EQ
                                                 else Cond.NE),
                                        z))
                   | _ =>
                        let
                           val cases' =
                              List.map (cases, fn (w, l) =>
                                        (WordX.toIntInf w, l))
                           val (lo, hi) =
                              List.fold
                              (cases', (#1 (hd cases'), #1 (hd cases')),
                               fn ((n, _), (lo, hi)) =>
                               (IntInf.min (lo, n), IntInf.max (hi, n)))
                           val n = List.length cases
                        in
                           if n >= 4 andalso hi - lo < IntInf.fromInt (3 * n)
                              then
                                 (* dense: use a jump table *)
                                 let
                                    val span = IntInf.toInt (hi - lo)
                                    val () = Int.inc jumpTables
                                    val table =
                                       Lbl.Plain
                                       (concat ["Ljump_", self, "_",
                                                int (!jumpTables)])
                                 in
                                    if lo < 4096
                                       then addImm (16, r, ~ (IntInf.toInt lo))
                                    else (movImm (17, lo)
                                          ; emit (I.Bin (I.SUB, R.X 16, x r,
                                                         regOp (R.X 17))))
                                    ; if span < 4096
                                         then emit (I.Cmp (I.CMP, R.X 16,
                                                           immOp span))
                                      else (movInt (17, span)
                                            ; emit (I.Cmp (I.CMP, R.X 16,
                                                           regOp (R.X 17))))
                                    ; bcond (CC Cond.HI, target default)
                                    ; emit (I.Adr (R.X 17, table))
                                    ; emit (I.Load (I.LDRSW, R.X 16,
                                                    Addr.Index (R.X 17, R.X 16,
                                                                2)))
                                    ; emit (I.Bin (I.ADD, R.X 17, R.X 17,
                                                   regOp (R.X 16)))
                                    ; emit (I.Br (R.X 17))
                                    ; label table
                                    ; Int.for
                                      (0, span + 1, fn k =>
                                       emit
                                       (I.Long
                                        (target
                                         (case List.peek
                                               (cases', fn (m, _) =>
                                                m = lo + IntInf.fromInt k) of
                                             SOME (_, l) => l
                                           | NONE => default),
                                         table)))
                                 end
                           else (List.foreach (cases, fn (w, l) =>
                                               bcond (eq w, target l))
                                 ; goto default)
                        end
               end
      (* the statements of a block, as they are output *)
      datatype item = datatype Items.t
      fun outputItem it =
         case it of
            App z => PrimApp.app (prim, z)
          | Defined _ => ()
          | Move z => move z
      (* an item has been output: so have its definitions *)
      fun afterItem it =
         (case it of
             App {dst, also, ...} =>
                (defined dst; Option.app (also, fn (z, _) => defined z))
           | Defined z => defined z
           | Move {dst, ...} => defined dst)
      fun outputItems its =
         List.foreach (its, fn it => (outputItem it; afterItem it))
      fun outputBlock (b as Block.T {kind, label = l, transfer, ...}) =
         (if Kind.isEntry kind
             then let
                     val stub = Lbl.Sym (entryStub l)
                  in
                     emit (I.Globl stub)
                     ; emit (I.Hidden stub)
                     ; label stub
                     ; addImm (31, 29, ~ frameSize)
                  end
          else ()
          ; label (blockLabel l)
          ; startBlock b
          ; (case kind of
                Kind.Cont {frameInfo, ...} => pop (frameInfo, {promo = true})
              | Kind.CReturn {dst, frameInfo, ...} =>
                   (Option.app (frameInfo, fn fi => pop (fi, {promo = true}))
                    ; Option.app
                      (dst, fn dst =>
                       let
                          val dst = Live.toOperand dst
                          val ct = cty dst
                       in
                          if directReturn l
                             (* store keeps a narrow value zero-extended *)
                             then store (dst, if isReal ct then 32 else 0)
                          else
                             let
                                val d = dstReg dst
                             in
                                ldr (ct, d,
                                     if isReal ct
                                        then Addr.Offset (R.SP, 8)
                                     else Addr.Base R.SP)
                                ; store (dst, d)
                             end
                          ; defined dst
                       end))
              | Kind.Func _ => ()
              | Kind.Handler {frameInfo, ...} => pop (frameInfo, {promo = false})
              | Kind.Jump => ()
              | Kind.SporkSpwn {frameInfo, ...} =>
                   pop (frameInfo, {promo = false}))
          ; let
               val its = Items.ofBlock (analysis, b)
            in
               case Items.fusable (analysis, b, its) of
                  NONE => (outputItems its; outputTransfer transfer)
                | SOME (its, a, l, l') =>
                     let
                        val () = outputItems its
                        val fused = PrimApp.appFlags (prim, a)
                     in
                        afterItem (App a)
                        ; (case fused of
                              NONE => outputTransfer transfer
                            | SOME c =>
                                 (case ifConvert transfer of
                                     NONE => twoWay (CC c, l, l')
                                   | SOME (z as {caseLabel, ...}) =>
                                        selects
                                        (if Label.equals (l, caseLabel)
                                            then c
                                         else Cond.negate c,
                                         z)))
                     end
            end)

      (* the blocks, in the order to output them *)
      val order =
         Layout.layout {analysis = analysis, chunkLabel = chunkLabel,
                        labelChunk = labelChunk, labelIndex = labelIndex}
      fun outputBlocks bs =
         case bs of
            [] => ()
          | b :: bs =>
               (next := (case bs of
                            Block.T {label, ...} :: _ => SOME label
                          | [] => NONE)
                ; outputBlock b
                ; outputBlocks bs)
      fun pair (opc, (r1, r2), k) =
         emit (I.Pair (opc, reg (r1, CType.Word64), reg (r2, CType.Word64),
                       Addr.Offset (R.SP, 16 * (k + 1))))
   in
      emit (I.Section Asm.Section.Text)
      ; emit (I.P2align 2)
      ; emit (I.Globl (Lbl.Sym self))
      ; emit (I.Hidden (Lbl.Sym self))
      ; label (Lbl.Sym self)
      ; emit (I.Pair (I.STP, R.X 29, R.X 30, Addr.PreIndex (R.SP, ~160)))
      ; emit (I.Un (I.MOV, R.X 29, R.SP))
      ; List.foreachi (saved, fn (k, rs) => pair (I.STP, rs, k))
      ; emit (I.Un (I.MOV, R.X 19, R.X 0))
      ; emit (I.Un (I.MOV, R.X 20, R.X 1))
      ; emit (I.Un (I.MOV, R.X 21, R.X 2))
      (* jump to the entry block with index x3 *)
      ; symAddr (17, entryTable, {got = false})
      ; emit (I.Load (I.LDR, R.X 16, Addr.Index (R.X 17, R.X 3, 3)))
      ; emit (I.Br (R.X 16))
      (* return x0 to the trampoline *)
      ; label leave
      ; emit (I.Un (I.MOV, R.SP, R.X 29))
      ; List.foreachi (saved, fn (k, rs) => pair (I.LDP, rs, k))
      ; emit (I.Pair (I.LDP, R.X 29, R.X 30, Addr.PostIndex (R.SP, 160)))
      ; emit I.Ret
      ; outputBlocks order
      (* the preheaders (out of line, so that nothing falls through them):
       * load the promoted stack slots
       *)
      ; List.foreach
        (order, fn Block.T {label = l, ...} =>
         let
            val {pheader, promoted, ...} = blockWebs l
         in
            if pheader
               then (label (preLabel l)
                     ; List.foreach
                       (promoted, fn ((off, ct), w) =>
                        case webHome w of
                           NONE => ()
                         | SOME r =>
                              ldr (ct, r, mem (20, off, ct)))
                     ; branch (blockLabel l))
            else ()
         end)
      ; instrs ()
   end

fun output {program as Program.T {chunks, frameInfos, ...}, outputC,
            outputS: unit -> {file: File.t,
                              print: string -> unit,
                              done: unit -> unit}} =
   let
      (* the table of entry stubs *)
      val () =
         let
            val {print, done, ...} = outputS ()
            val entries = Array.new (Vector.length frameInfos, NONE)
            val () =
               List.foreach
               (chunks, fn Chunk.T {blocks, ...} =>
                Vector.foreach
                (blocks, fn Block.T {kind, label, ...} =>
                 if Kind.isEntry kind
                    then Array.update
                         (entries,
                          FrameInfo.index (valOf (Kind.frameInfoOpt kind)),
                          SOME label)
                 else ()))
            val table = Lbl.Sym entryTable
            val code = ref [I.Label table, I.Hidden table, I.Globl table,
                            I.P2align 3, I.Section Asm.Section.RelRo]
            val () =
               Array.foreach
               (entries, fn lo =>
                List.push (code,
                           I.Quad (Option.map (lo, fn l =>
                                               Lbl.Sym (entryStub l)))))
         in
            Asm.output (List.rev (!code), print)
            ; done ()
         end
   in
   CCodegen.outputWith
   {program = program,
    outputC = outputC,
    outputChunks =
    SOME (fn {chunks, labelBlock, labelChunk, labelIndex} =>
          let
             val {print, done, ...} = outputS ()
          in
             List.foreach
             (chunks, fn chunk =>
              Asm.output (outputChunk (chunk, {labelBlock = labelBlock,
                                               labelChunk = labelChunk,
                                               labelIndex = labelIndex}),
                          print))
             ; done ()
          end)}
   end

end

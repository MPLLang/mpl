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
 * That is the default ("graph") register allocator.  There is a second,
 * "local", allocator, selected by `-vibe-arm-regalloc local`, that is
 * modeled on the allocator of the x86/amd64 codegens; see "The local
 * register allocator", below.
 *
 * The regions, the webs of temporaries and (for the graph allocator) their
 * registers are computed for a whole chunk before any of its code is
 * output: that is VibeArmAnalysis (see vibe-arm-analysis.sig).
 *
 * Output.  The codegen builds a list of ARM64 instructions for each chunk
 * (see arm64.sig); printing them as assembly is the business of Arm64.
 *)

functor VibeArmCodegen (S: VIBE_ARM_CODEGEN_STRUCTS): VIBE_ARM_CODEGEN =
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

structure Analysis = VibeArmAnalysis (structure Machine = Machine)
structure Region = Analysis.Region
structure Temp = Analysis.Temp
structure Web = Analysis.Web
val caller = Analysis.caller
val callee = Analysis.callee
val fcaller = Analysis.fcaller
val fcallee = Analysis.fcallee
val callerSave = Analysis.callerSave
val dstTemp = Analysis.dstTemp
val foreachTemp = Analysis.foreachTemp
val isReal = Analysis.isReal
val sameSlot = Analysis.sameSlot
val size = Analysis.size
val slotKey = Analysis.slotKey
val stripCast = Analysis.stripCast
val switchCases = Analysis.switchCases

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
fun entryStub l = "vibeArmEntry_" ^ Label.toString l
val entryTable = "vibeArmEntries"

fun outputChunk (Chunk.T {blocks, chunkLabel, tempsMax},
                 {labelBlock: Label.t -> Block.t,
                  labelChunk: Label.t -> ChunkLabel.t,
                  labelIndex: Label.t -> int}): I.t list =
   let
      val int = CCodegen.C.int
      val darwin = !Control.Target.os = MLton.Platform.OS.Darwin
      (* the instructions of the chunk, the most recent first *)
      val code: I.t list ref = ref []
      fun emit ins = List.push (code, ins)
      fun label l = emit (I.Label l)
      fun branch l = emit (I.B (Tgt.Label l))
      val self = ChunkLabel.toString chunkLabel
      val leave = Lbl.Plain ("Lleave_" ^ self)
      (* which register allocator: see "Register allocation", above *)
      val localRA = !Control.vibeArmLocalRegAlloc
      (* The label of a block, and of the preheader that loads its promoted
       * stack slots.  By default they are local to the assembler; with
       * -vibe-arm-symbols true, each is a symbol, so that tools (a
       * profiler, a debugger) can name the block that an address is in.
       *)
      val symbols = !Control.vibeArmSymbols
      fun blockLabel l =
         if symbols then Lbl.Sym (Label.toString l)
         else Lbl.Local (Label.toString l)
      fun preLabel l =
         if symbols then Lbl.Sym ("pre_" ^ Label.toString l)
         else Lbl.Plain ("Lpre_" ^ Label.toString l)

      (* Registers: 0-31 are x0-x30,sp; 32-63 are d0-d31. *)
      fun x r = if r = 31 then R.SP else R.X r
      (* register r, at the width of a value of type ct *)
      fun reg (r, ct) =
         if r >= 32
            then (if size ct = 4 then R.S (r - 32) else R.D (r - 32))
         else if size ct = 8 then x r else R.W r
      val regOp = Opnd.Reg
      fun immOp n = Opnd.Imm (IntInf.fromInt n)
      fun scratch (ct, k) = if isReal ct then 48 + k else k
      (* load/store register r, as a value of type ct *)
      fun ldr (ct, r, a) =
         emit (I.Load (case size ct of 1 => I.LDRB | 2 => I.LDRH | _ => I.LDR,
                       reg (r, ct), a))
      fun str (ct, r, a) =
         emit (I.Store (case size ct of 1 => I.STRB | 2 => I.STRH | _ => I.STR,
                        reg (r, ct), a))
      fun mov (d, s, ct) =
         if d = s then ()
         else emit (I.Un (if d >= 32 then I.FMOV else I.MOV,
                          reg (d, ct), reg (s, ct)))

      fun movImm (r, n: IntInf.t) =
         let
            val top = IntInf.<< (1, 0w64)
            val n = if n < 0 then n + top else n
            fun chunk k =
               IntInf.toInt (IntInf.andb (IntInf.~>> (n, Word.fromInt (16 * k)),
                                          0xFFFF))
            fun mk (opc, k) = emit (opc (x r, chunk k, 16 * k))
         in
            if n >= top - 0x10000
               then emit (I.MovImm (x r, n - top))
            else (case List.keepAll ([0, 1, 2, 3], fn k => chunk k <> 0) of
                     [] => emit (I.MovImm (x r, 0))
                   | k :: ks => (mk (I.Movz, k)
                                 ; List.foreach (ks, fn k => mk (I.Movk, k))))
         end
      fun movInt (r, n) = movImm (r, IntInf.fromInt n)
      (* d = s + n; may clobber x17 *)
      fun addImm (d, s, n) =
         if n = 0 then (if d = s then () else emit (I.Un (I.MOV, x d, x s)))
         else
            let
               val opc = if n > 0 then I.ADD else I.SUB
               val n = Int.abs n
            in
               if n < 4096
                  then emit (I.Bin (opc, x d, x s, immOp n))
               else (movInt (17, n)
                     ; emit (I.Bin (opc, x d, x s, regOp (R.X 17))))
            end
      (* an addressing mode for [b + off]; may clobber x17 *)
      fun mem (b, off, ct) =
         if (~256 <= off andalso off < 256)
            orelse (off >= 0 andalso off mod size ct = 0
                    andalso off div size ct < 4096)
            then Addr.Offset (x b, off)
         else (movInt (17, off); Addr.Index (x b, R.X 17, 0))
      (* the address of a symbol, directly or from the global offset table *)
      fun symAddr (r, s, {got}) =
         let
            val s = Lbl.Sym s
         in
            emit (I.Adrp (x r, s, {got = got}))
            ; emit (if got
                       then I.LoadGot (x r, x r, s)
                    else I.AddLo (x r, x r, s))
         end

      (* Frame: [sp, #0] and [sp, #8] hold the int/real result of the last
       * C call; then the temporaries that don't have a home register.
       *)
      val (slots, frameSize) =
         List.fold (CType.all, ([], 16), fn (t, (slots, off)) =>
                    ((t, off) :: slots, off + 8 * (1 + tempsMax t)))
      (* then an area in which to save the caller-save registers at C calls *)
      val saveBase = frameSize
      val frameSize = 16 * ((frameSize + 8 * 21 + 15) div 16)
      fun slot ct =
         #2 (valOf (List.peek (slots, fn (t, _) => CType.equals (t, ct))))
      (* extra adjustment of sp while passing C arguments on the stack *)
      val spAdj = ref 0

      (* The analysis of the chunk (see vibe-arm-analysis.sig): speculation,
       * regions, loops and, for the graph allocator, the registers of the
       * webs of temporaries and of promoted stack slots.  The blocks that
       * it returns are the ones to output.
       *)
      val {blocks, labelBlock, blockRegion, blockWebs, ifConvert, liveTemps,
           loopDistance, nearSuccessors, segIn} =
         Analysis.analyze {blocks = blocks, labelBlock = labelBlock,
                           localRA = localRA}
      (* the region of the block that is being output *)
      val region = ref (Region.new ())
      (* a temporary of that region *)
      fun temp t = Region.temp (!region, t)
      (* The webs of the temporaries at the point that is being output. *)
      val curWebs: (Temp.t * Web.t) list ref = ref []
      (* the definitions that are still to come in the block *)
      val pendingDefs: (Temp.t * Web.t) list ref = ref []
      (* the promoted stack slots of the block *)
      val curPromoted: (Analysis.slot * Web.t) list ref = ref []
      (* the block that is being output *)
      val curLabel = ref (Label.newNoname ())
      val webHome = Web.home
      fun enterBlock l =
         let
            val {segIn, defs, promoted, ...} = blockWebs l
         in
            curWebs := segIn
            ; pendingDefs := defs
            ; curPromoted := promoted
         end
      (* the register of a stack slot, if it is promoted here *)
      fun promotedHome so =
         case List.peek (!curPromoted, fn (k, _) => sameSlot (k, slotKey so)) of
            NONE => NONE
          | SOME (_, w) => webHome w
      (* The label to branch to to get to l: the preheader of a loop with
       * promoted slots, if coming from outside the loop.
       *)
      fun target l =
         let
            val {ploop, pheader, ...} = blockWebs l
         in
            if not localRA
               andalso ploop <> 0
               andalso ploop <> #ploop (blockWebs (!curLabel))
               then if pheader
                       then preLabel l
                    else Error.bug "VibeArmCodegen.target: no preheader"
            else blockLabel l
         end
      (* the register of a temporary that is being used *)
      fun tempHome t =
         let
            val e = temp t
         in
            case List.peek (!curWebs, fn (e', _) => Temp.equals (e, e')) of
               NONE => NONE
             | SOME (_, w) => webHome w
         end
      (* the register of a temporary that is being defined *)
      fun defHome t =
         let
            val e = temp t
         in
            case List.peek (!pendingDefs, fn (e', _) => Temp.equals (e, e')) of
               NONE => NONE
             | SOME (_, w) => webHome w
         end
      (* the definition of z has been output *)
      fun defined z =
         case dstTemp z of
            NONE => ()
          | SOME t =>
               let
                  val e = temp t
                  fun same (e', _) = Temp.equals (e, e')
               in
                  case List.peek (!pendingDefs, same) of
                     NONE => ()
                   | SOME d =>
                        (pendingDefs := List.removeFirst (!pendingDefs, same)
                         ; curWebs := d :: List.keepAll (!curWebs, not o same))
               end
      (* the caller-save registers that hold some of the webs *)
      fun callerSaves ws =
         List.fold (List.keepAllMap (ws, webHome), [], fn (r, ac) =>
                    if callerSave r andalso not (List.contains (ac, r, op =))
                       then r :: ac
                    else ac)
      (* store/load registers to/from the save area of the frame *)
      fun commit (f, rs) =
         List.foreach
         (rs, fn r =>
          let
             val ct = if r >= 32 then CType.Real64 else CType.Word64
          in
             f (ct, r,
                mem (31, saveBase + 8 * (if r >= 32 then r - 43 else r - 8),
                     ct))
          end)

      val saved = [(19, 20), (21, 22), (23, 24), (25, 26), (27, 28),
                   (40, 41), (42, 43), (44, 45), (46, 47)]

      fun cty z = Type.toCType (Operand.ty z)
      (* Stack slot caching.  Within a block, a stack slot that is read more
       * than once is kept in a register that no temporary is using in the
       * block.  Stores write through to memory, so the cache can be dropped
       * at any time; it is dropped whenever StackTop changes and at C calls.
       *)
      val cache: {off: int, ct: CType.t, reg: int} list ref = ref []
      (* the slots that are worth caching, and the registers that are free,
       * in the block being output
       *)
      val cacheable: (int * CType.t) list ref = ref []
      val freeRegs: int list ref = ref []
      fun cached so =
         List.peek (!cache, fn {off, ct, ...} =>
                    sameSlot ((off, ct), slotKey so))
      (* a free register in which to cache a slot, if it is worth it *)
      fun cacheReg (k as (_, ct)) =
         if List.exists (!cacheable, fn k' => sameSlot (k, k'))
            then List.peek (!freeRegs, fn r =>
                            (r >= 32) = isReal ct
                            andalso not (List.exists (!cache, fn {reg, ...} =>
                                                      reg = r)))
         else NONE
      (* ------------- The local register allocator ------------- *)
      (* This is the allocator of the x86/amd64 codegens, adapted.  Every
       * temporary has a home in the (native) stack frame, and the registers
       * are a cache of temporaries and stack slots that is filled on
       * demand as a block is output.  When a register is needed and none
       * is free, the value whose next use is furthest away is evicted
       * (and written back, if it is live and dirty); values that are dead
       * are dropped.  Stack slots are written through, so only temporaries
       * can be dirty.  The caller-save registers are committed and dropped
       * at C calls.
       *
       * Values cross near transfers in registers ("live transfers"): the
       * first predecessor of a block to be output fixes which temporaries
       * the block expects in which registers, and any other predecessor
       * conforms, with compensation code on the edge if it has to.
       *)
      datatype loc =
         S of int * CType.t      (* a stack slot *)
       | T of CType.t * int      (* a temporary *)
      fun sameLoc (l, l') =
         case (l, l') of
            (S k, S k') => sameSlot (k, k')
          | (T (ct, n), T (ct', n')) => n = n' andalso CType.equals (ct, ct')
          | _ => false
      fun locCt l = case l of S (_, ct) => ct | T (ct, _) => ct
      fun isT l = case l of T _ => true | _ => false
      fun tloc t = T (Type.toCType (Temporary.ty t), Temporary.index t)
      fun wideCt ct = if isReal ct then CType.Real64 else CType.Word64
      type rentry = {reg: int, loc: loc, dirty: bool ref}
      (* what the registers hold *)
      val regs: rentry list ref = ref []
      (* registers that the item being output is using *)
      val pinned: int list ref = ref []
      (* registers that are reserved for the destinations of the item *)
      val reserved: (loc * int) list ref = ref []
      (* the index of the item being output; the transfer comes last *)
      val pos = ref 0
      (* where each location occurs in the block being output *)
      val occs: (loc * int list) list ref = ref []
      (* the temporaries that are live at the end of the block *)
      val liveOut: loc list ref = ref []
      (* what a block expects in registers: (temporary, register, clean?) *)
      val {get = entryState: Label.t -> (loc * int * bool) list option ref,
           ...} =
         Property.get (Label.plist, Property.initFun (fn _ => ref NONE))
      fun clearCache () =
         (cache := []
          ; regs := List.keepAll (!regs, fn {loc, ...} => isT loc))
      fun pin r = List.push (pinned, r)
      fun inReg l = List.peek (!regs, fn {loc, ...} => sameLoc (l, loc))
      fun occsOf l =
         case List.peek (!occs, fn (l', _) => sameLoc (l, l')) of
            SOME (_, ps) => ps
          | NONE => []
      fun usedLater l = List.exists (occsOf l, fn p => p > !pos)
      (* A stack slot that is promoted in the region of the block stays in
       * its register across the blocks of the region.
       *)
      fun loopSlot l =
         case l of
            S k => List.exists (!curPromoted, fn (k', _) => sameSlot (k, k'))
          | T _ => false
      fun isLiveOut l =
         loopSlot l orelse List.exists (!liveOut, fn l' => sameLoc (l, l'))
      fun liveLater l = usedLater l orelse isLiveOut l
      val never = 1000000
      (* never: live at the end of the block; never + 1: dead *)
      fun nextUse l =
         case List.peek (occsOf l, fn p => p >= !pos) of
            SOME p => p
          | NONE => if isLiveOut l then never else never + 1
      fun homeOf (ct, n) = (31, !spAdj + slot ct + 8 * n)
      (* write a register back to its temporary's home *)
      fun commitEntry ({reg = r, loc, dirty}: rentry) =
         case loc of
            S _ => ()
          | T (ct, n) =>
               if !dirty
                  then let
                          val (b, off) = homeOf (ct, n)
                       in
                          str (wideCt ct, r, mem (b, off, wideCt ct))
                          ; dirty := false
                       end
               else ()
      fun dropReg r = regs := List.keepAll (!regs, fn {reg, ...} => reg <> r)
      fun dropLoc l =
         regs := List.keepAll (!regs, fn {loc, ...} => not (sameLoc (l, loc)))
      (* a register for l, evicting something if need be *)
      fun allocReg l =
         let
            val real = isReal (locCt l)
            val across =
               case l of
                  S _ => false
                | T (ct, n) =>
                     List.exists (Region.temps (!region), fn e =>
                                  Temp.index e = n
                                  andalso CType.equals (ct, Temp.ct e)
                                  andalso Temp.across e)
            val pool =
               case (real, across) of
                  (false, false) => caller @ callee
                | (false, true) => callee @ caller
                | (true, false) => fcaller @ fcallee
                | (true, true) => fcallee @ fcaller
            fun usable r = not (List.contains (!pinned, r, op =))
            val r =
               case List.peek (pool, fn r =>
                               usable r
                               andalso not (List.exists (!regs, fn {reg, ...} =>
                                                         reg = r))) of
                  SOME r => r
                | NONE =>
                     let
                        (* furthest next use; prefer a clean one *)
                        val victim =
                           List.fold
                           (!regs, NONE, fn (e as {reg, loc, dirty}, best) =>
                            if (reg >= 32) <> real orelse not (usable reg)
                               then best
                            else
                               let
                                  val score = 2 * nextUse loc
                                              + (if !dirty then 0 else 1)
                               in
                                  case best of
                                     NONE => SOME (e, score)
                                   | SOME (_, score') =>
                                        if score > score'
                                           then SOME (e, score)
                                        else best
                               end)
                     in
                        case victim of
                           NONE => Error.bug "VibeArmCodegen.allocReg"
                         | SOME (e as {reg, loc, ...}, _) =>
                              (if nextUse loc <= never then commitEntry e else ()
                               ; dropReg reg
                               ; reg)
                     end
         in
            pin r; r
         end
      (* Load a location into a register (of the cache), if it is worth it. *)
      fun cacheIn z =
         if localRA
            then
               let
                  fun go (l, ct, (b, off)) =
                     if usedLater l orelse loopSlot l
                        then
                           let
                              val r = allocReg l
                           in
                              ldr (ct, r, mem (b, off, ct))
                              ; List.push (regs, {reg = r, loc = l,
                                                  dirty = ref false})
                              ; SOME r
                           end
                     else NONE
               in
                  case z of
                     Operand.Cast (z, _) => cacheIn z
                   | Operand.StackOffset so =>
                        let
                           val k as (off, ct) = slotKey so
                        in
                           go (S k, ct, (20, off))
                        end
                   | Operand.Temporary t =>
                        let
                           val ct = Type.toCType (Temporary.ty t)
                           val n = Temporary.index t
                        in
                           go (T (ct, n), ct, homeOf (ct, n))
                        end
                   | _ => NONE
               end
         else
         case z of
            Operand.Cast (z, _) => cacheIn z
          | Operand.StackOffset so =>
               let
                  val k as (off, ct) = slotKey so
               in
                  case cacheReg k of
                     NONE => NONE
                   | SOME r =>
                        (ldr (ct, r, mem (20, off, ct))
                         ; List.push (cache, {off = off, ct = ct, reg = r})
                         ; SOME r)
               end
          | _ => NONE
      fun home z =
         case z of
            Operand.Cast (z, _) => home z
          | Operand.Frontier => SOME 21
          | Operand.GCState => SOME 19
          | Operand.StackOffset so =>
               if localRA
                  then pinnedReg (inReg (S (slotKey so)))
               else (case promotedHome so of
                        SOME r => SOME r
                      | NONE => Option.map (cached so, #reg))
          | Operand.StackTop => SOME 20
          | Operand.Temporary t =>
               if localRA then pinnedReg (inReg (tloc t)) else tempHome t
          | _ => NONE
      and pinnedReg (e: rentry option) =
         case e of
            NONE => NONE
          | SOME {reg, ...} => (pin reg; SOME reg)
      (* The address of a memory operand, as a base register and offset.
       * May clobber x<t> and up.
       *)
      fun addr (z, t): int * int =
         case z of
            Operand.Cast (z, _) => addr (z, t)
          | Operand.Global g =>
               let
                  val ct = Type.toCType (Global.ty g)
               in
                  symAddr (t, "global" ^ CType.toString ct, {got = false})
                  ; (t, size ct * Global.index g)
               end
          | Operand.Offset {base, offset, ...} =>
               (value (base, t, t), Bytes.toInt offset)
          | Operand.SequenceOffset {base, index, offset, scale, ...} =>
               let
                  val () = if t >= 17
                              then Error.bug "VibeArmCodegen.addr: nesting"
                           else ()
                  val b = value (base, t, t)
                  val ix = value (index, t + 1, t + 1)
                  val lg = case scale of
                              Scale.One => 0 | Scale.Two => 1
                            | Scale.Four => 2 | Scale.Eight => 3
               in
                  emit (I.Bin (I.ADD, x t, x b,
                               Opnd.Shifted (x ix, Opnd.LSL, lg)))
                  ; (t, Bytes.toInt offset)
               end
          | Operand.StackOffset (StackOffset.T {offset, ...}) =>
               (20, Bytes.toInt offset)
          | Operand.Temporary tmp =>
               (31, !spAdj + slot (cty z) + 8 * Temporary.index tmp)
          | _ => Error.bug (concat ["VibeArmCodegen.addr: ", Operand.toString z])
      (* Put the value of z in register d. May clobber x<t> and up. *)
      and load (z, d, t): unit =
         let
            val ct = cty z
         in
            case z of
               Operand.Address z =>
                  let val (b, off) = addr (z, t) in addImm (d, b, off) end
             | Operand.Cast (z, _) => load (z, d, t)
             | Operand.Const c =>
                  (case c of
                      Const.CSymbol (CSymbol.T {name, ...}) =>
                         symAddr (d, name, {got = true})
                    | Const.Null => movInt (d, 0)
                    | Const.Real r =>
                         (case RealX.castToWord r of
                             NONE => Error.bug "VibeArmCodegen.load: Real"
                           | SOME w =>
                                (movImm (17, WordX.toIntInf w)
                                 ; emit (I.Un (I.FMOV, reg (d, ct),
                                               reg (17, ct)))))
                    | Const.Word w => movImm (d, WordX.toIntInf w)
                    | _ => Error.bug "VibeArmCodegen.load: Const")
             | Operand.Label l => movInt (d, labelIndex l)
             | Operand.StaticHeapRef (StaticHeap.Ref.T {kind, offset, ...}) =>
                  (symAddr (d, "staticHeap" ^ StaticHeap.Kind.name kind,
                            {got = false})
                   ; addImm (d, d, Bytes.toInt offset))
             | _ =>
                  (case home z of
                      SOME h => mov (d, h, ct)
                    | NONE =>
                         (case cacheIn z of
                             SOME r => mov (d, r, ct)
                           | NONE =>
                                ldr (ct, d, memOf (z, t, ct))))
         end
      (* An addressing mode for a memory operand that is accessed at type ct.
       * A sequence element is addressed as [base, index, lsl #s] when it can
       * be: no offset, and a scale that is 1 or the size of the access.
       *)
      and memOf (z, t, ct): Addr.t =
         let
            fun default () =
               let val (b, off) = addr (z, t) in mem (b, off, ct) end
         in
            case z of
               Operand.Cast (z, _) => memOf (z, t, ct)
             | Operand.SequenceOffset {base, index, offset, scale, ...} =>
                  let
                     val n = Bytes.toInt (Scale.toBytes scale)
                  in
                     if Bytes.toInt offset = 0
                        andalso (n = 1 orelse n = size ct)
                        andalso t < 17
                        then
                           let
                              val b = value (base, t, t)
                              val ix = value (index, t + 1, t + 1)
                           in
                              Addr.Index (x b, x ix,
                                          case n of
                                             1 => 0
                                           | 2 => 1
                                           | 4 => 2
                                           | _ => 3)
                           end
                     else default ()
                  end
             | _ => default ()
         end
      (* A register holding the value of z: either its home or d. *)
      and value (z, d, t): int =
         case home z of
            SOME h => h
          | NONE => (case cacheIn z of
                        SOME r => r
                      | NONE => (load (z, d, t); d))
      fun store (dst, r) =
         let
            val ct = cty dst
            (* set a register home, keeping narrow values zero-extended *)
            fun set h =
               case size ct of
                  1 => emit (I.Bin (I.AND, R.W h, R.W r, immOp 0xff))
                | 2 => emit (I.Bin (I.AND, R.W h, R.W r, immOp 0xffff))
                | _ => mov (h, r, ct)
            fun strip z =
               case z of
                  Operand.Cast (z, _) => strip z
                | _ => z
         in
            case (strip dst, home dst) of
               (Operand.StackOffset so, _) =>
                  let
                     val k as (off, _) = slotKey so
                     val () =
                        str (ct, r, mem (20, off, ct))
                     (* drop other views of the bytes that were written *)
                     fun other (off', ct') =
                        not (sameSlot (k, (off', ct')))
                        andalso off' + size ct' > off
                        andalso off + size ct > off'
                     val () =
                        cache := List.keepAll (!cache, fn {off, ct, ...} =>
                                               not (other (off, ct)))
                     val () =
                        regs := List.keepAll (!regs, fn {loc, ...} =>
                                              case loc of
                                                 S k' => not (other k')
                                               | T _ => true)
                  in
                     if localRA
                        then
                           (case inReg (S k) of
                               SOME {reg, ...} => set reg
                             | NONE =>
                                  if usedLater (S k) orelse loopSlot (S k)
                                     then
                                        let
                                           val h = allocReg (S k)
                                        in
                                           set h
                                           ; List.push (regs, {reg = h, loc = S k,
                                                               dirty = ref false})
                                        end
                                  else ())
                     else
                     case (promotedHome so, cached so) of
                        (SOME r, _) => set r
                      | (_, SOME {reg, ...}) => set reg
                      | _ =>
                           (case cacheReg k of
                               NONE => ()
                             | SOME h =>
                                  (set h
                                   ; List.push (cache, {off = off, ct = ct,
                                                        reg = h})))
                  end
             | (Operand.Temporary t, _) =>
                  if localRA
                     then
                        let
                           val l = tloc t
                           fun enter h =
                              (dropLoc l
                               ; set h
                               ; List.push (regs, {reg = h, loc = l,
                                                   dirty = ref true}))
                        in
                           case (List.peek (!reserved, fn (l', _) =>
                                            sameLoc (l, l')),
                                 inReg l) of
                              (SOME (_, h), _) =>
                                 (reserved := List.keepAll
                                              (!reserved, fn (l', _) =>
                                               not (sameLoc (l, l')))
                                  ; enter h)
                            | (NONE, SOME {reg, dirty, ...}) =>
                                 (set reg; dirty := true)
                            | (NONE, NONE) =>
                                 (* a dead destination is not stored at all *)
                                 if liveLater l then enter (allocReg l) else ()
                        end
                  else (case defHome t of
                           SOME h => set h
                         | NONE =>
                              str (ct, r, memOf (dst, 3, ct)))
             | (_, SOME h) => set h
             | (_, NONE) =>
                  str (ct, r, memOf (dst, 3, ct))
         end
      (* A register in which to compute the value of dst. *)
      fun dstReg dst =
         let
            fun strip z =
               case z of
                  Operand.Cast (z, _) => strip z
                | _ => z
            val narrow = size (cty dst) < 4
         in
            case (localRA, strip dst) of
               (true, Operand.Temporary t) =>
                  let
                     val l = tloc t
                  in
                     if narrow orelse not (liveLater l)
                        then scratch (cty dst, 0)
                     else
                        (* The entry for l isn't made until the store, because
                         * the arguments may still read the old value of l.
                         *)
                        (case List.peek (!reserved, fn (l', _) =>
                                         sameLoc (l, l')) of
                            SOME (_, h) => h
                          | NONE =>
                               (case inReg l of
                                   SOME {reg, ...} => (pin reg; reg)
                                 | NONE =>
                                      let
                                         val h = allocReg l
                                      in
                                         List.push (reserved, (l, h)); h
                                      end))
                  end
             | (false, Operand.Temporary t) =>
                  (case defHome t of
                      SOME h => if narrow then scratch (cty dst, 0) else h
                    | NONE => scratch (cty dst, 0))
             | _ =>
                  (case home dst of
                      SOME h => if narrow then scratch (cty dst, 0) else h
                    | NONE => scratch (cty dst, 0))
         end
      fun move {dst, src} =
         let
            fun strip z =
               case z of
                  Operand.Cast (z, _) => strip z
                | _ => z
            fun default () = store (dst, value (src, dstReg dst, 3))
            fun inMemory z =
               case z of
                  Operand.Global _ => true
                | Operand.Offset _ => true
                | Operand.SequenceOffset _ => true
                | Operand.StackOffset _ => true
                | _ => false
         in
            (* A load of a narrow value zero-extends it, so it can go
             * straight to the home register of a temporary.
             *)
            case (localRA,
                  case strip dst of
                     Operand.Temporary t => defHome t
                   | _ => NONE) of
               (false, SOME h) =>
                  if size (cty dst) < 4
                     andalso size (cty src) = size (cty dst)
                     andalso inMemory (strip src)
                     andalso not (isSome (home src))
                     andalso not (isSome (cacheIn src))
                     then load (src, h, 3)
                  else default ()
             | _ =>
            case (localRA, strip dst, strip src) of
               (true, Operand.Temporary td, Operand.Temporary ts) =>
                  let
                     val ld = tloc td
                     val ls = tloc ts
                  in
                     (* A copy from a temporary that is dead afterwards just
                      * renames what its register holds.
                      *)
                     case inReg ls of
                        SOME {reg, ...} =>
                           if not (liveLater ls)
                              andalso not (sameLoc (ld, ls))
                              andalso size (cty dst) >= 4
                              andalso size (cty dst) = size (cty src)
                              then (dropLoc ld
                                    ; dropReg reg
                                    ; if liveLater ld
                                         then List.push (regs, {reg = reg,
                                                                loc = ld,
                                                                dirty = ref true})
                                      else ())
                           else default ()
                      | NONE => default ()
                  end
             | _ => default ()
         end

      (* (Local allocator.)  Before a C call: write back the caller-save
       * registers that hold live temporaries, and forget what the
       * caller-save registers hold.
       *)
      fun commitCallerSaves live =
         if localRA
            then List.foreach (!regs, fn e as {reg, loc, ...} =>
                               if callerSave reg andalso live loc
                                  then commitEntry e
                               else ())
         else ()
      fun dropCallerSaves () =
         if localRA
            then regs := List.keepAll (!regs, fn {reg, ...} =>
                                       not (callerSave reg))
         else ()
      fun flushCallerSaves live = (commitCallerSaves live; dropCallerSaves ())

      (* When fusing, a primitive that computes a condition leaves it in the
       * flags (and reports it in fused) rather than computing dst.
       *)
      val fusing = ref false
      val fused: Cond.t option ref = ref NONE
      (* When set, an overflow-checking primitive also stores the result of
       * the operation itself to the given operand; the bool is true if the
       * check originally came before the operation.
       *)
      val also: (Operand.t * bool) option ref = ref NONE
      fun primApp (prim: Type.t Prim.t, args, dst) =
         let
            datatype z = datatype Prim.t
            val dct = cty dst
            val d = dstReg dst
            fun a k = Vector.sub (args, k)
            (* a register holding argument k *)
            fun v k = value (a k, scratch (cty (a k), k), 3)
            fun opnd (k, ct, immOk) =
               case (immOk, a k) of
                  (true, Operand.Const (Const.Word w)) =>
                     if WordX.toIntInf w < 4096
                        then Opnd.Imm (WordX.toIntInf w)
                     else regOp (reg (v k, ct))
                | _ => regOp (reg (v k, ct))
            fun done () = store (dst, d)
            fun un (opc, ct) =
               (emit (I.Un (opc, reg (d, dct), reg (v 0, ct))); done ())
            fun bin' (opc, ct, r0, immOk) =
               (emit (I.Bin (opc, reg (d, ct), reg (r0, ct),
                             opnd (1, ct, immOk)))
                ; done ())
            fun bin (opc, ct, immOk) = bin' (opc, ct, v 0, immOk)
            fun tern (opc, ct) =
               (emit (I.Tern (opc, reg (d, ct), reg (v 0, ct), reg (v 1, ct),
                              reg (v 2, ct)))
                ; done ())
            fun cset cond =
               if !fusing
                  then fused := SOME cond
               else (emit (I.Cset (reg (d, dct), cond)); done ())
            fun cmp' (opc, ct, r0, cond) =
               (emit (I.Cmp (opc, reg (r0, ct), opnd (1, ct, opc = I.CMP)))
                ; cset cond)
            fun cmp (opc, ct, cond) = cmp' (opc, ct, v 0, cond)
            fun addsub (isAdd, ct) =
               let
                  fun opc b = if b then I.ADD else I.SUB
                  fun default () = bin (opc isAdd, ct, true)
               in
                  case a 1 of
                     Operand.Const (Const.Word w) =>
                        let
                           val n = WordX.toIntInfX w
                        in
                           if size ct = 8 andalso ~4096 < n andalso n < 0
                              then (emit (I.Bin (opc (not isAdd), reg (d, ct),
                                                 reg (v 0, ct), Opnd.Imm (~ n)))
                                    ; done ())
                           else default ()
                        end
                   | _ => default ()
               end
            val ptr = CType.CPointer
            val rct = CType.real
            fun wct ws = CType.word (ws, {signed = false})
            fun bits ws = Bits.toInt (WordSize.bits ws)
            (* sign extension from n bits *)
            fun sxt n = case n of 8 => I.SXTB | 16 => I.SXTH | _ => I.SXTW
            (* A register holding argument k (of size ws), sign extended (if
             * signed) to 32 or 64 bits.
             *)
            fun ext (k, ws, signed, to64) =
               let
                  val n = bits ws
                  val r = v k
                  val s = k
               in
                  if signed andalso (n < 32 orelse (to64 andalso n < 64))
                     then (emit (I.Un (sxt n, if to64 then x s else R.W s,
                                       R.W r))
                           ; s)
                  else r
               end
            datatype arith = Add | Mul | Neg | Sub
            (* an overflow check for the operation opc on arguments ks *)
            fun chk (opc: arith, (ws, {signed}), ks) =
               let
                  val n = bits ws
                  (* the operands of the operation *)
                  val (r0, z1) =
                     if n = 64 andalso (opc = Add orelse opc = Sub)
                        then
                           let
                              val r0 = x (v 0)
                           in
                              (r0, SOME (opnd (1, wct ws, true)))
                           end
                     else (case List.map (ks, fn k =>
                                          x (ext (k, ws, signed, true))) of
                              [r0] => (r0, NONE)
                            | [r0, r1] => (r0, SOME (regOp r1))
                            | _ => Error.bug "VibeArmCodegen.chk: operands")
                  (* a binary instruction on the operands *)
                  fun binary (opc, d) =
                     case z1 of
                        SOME z => emit (I.Bin (opc, d, r0, z))
                      | NONE => Error.bug "VibeArmCodegen.chk: binary"
                  (* the operation itself, which may also set the flags *)
                  fun arith (d, {flags}) =
                     case opc of
                        Add => binary (if flags then I.ADDS else I.ADD, d)
                      | Mul => binary (I.MUL, d)
                      | Neg => emit (I.Un (if flags then I.NEGS else I.NEG,
                                           d, r0))
                      | Sub => binary (if flags then I.SUBS else I.SUB, d)
                  (* r holds the result of the operation itself *)
                  fun finish (r, cond) =
                     case !also of
                        NONE => cset cond
                      | SOME (z, chkFirst) =>
                           if chkFirst
                              then (cset cond; store (z, r))
                           else (store (z, r); cset cond)
               in
                  if n < 64
                     then (arith (R.X 5, {flags = false})
                           ; emit (I.Cmp
                                   (I.CMP, R.X 5,
                                    Opnd.Extended
                                    (R.W 5,
                                     case (signed, n) of
                                        (true, 8) => Opnd.SXTB
                                      | (true, 16) => Opnd.SXTH
                                      | (true, _) => Opnd.SXTW
                                      | (false, 8) => Opnd.UXTB
                                      | (false, 16) => Opnd.UXTH
                                      | (false, _) => Opnd.UXTW)))
                           ; finish (5, Cond.NE))
                  else if opc = Mul
                     then (binary (if signed then I.SMULH else I.UMULH, R.X 5)
                           ; if signed orelse Option.isSome (!also)
                                then arith (R.X 6, {flags = false})
                             else ()
                           ; if signed
                                then emit (I.Cmp (I.CMP, R.X 5,
                                                  Opnd.Shifted (R.X 6, Opnd.ASR,
                                                                63)))
                             else emit (I.Cmp (I.CMP, R.X 5, immOp 0))
                           ; finish (6, Cond.NE))
                  else
                     let
                        val r = case !also of
                                   SOME (z, false) => dstReg z
                                 | _ => 5
                     in
                        arith (x r, {flags = true})
                        ; finish (r, case (signed, opc) of
                                        (true, _) => Cond.VS
                                      | (false, Add) => Cond.CS
                                      | (false, _) => Cond.CC)
                     end
               end
            fun shift (opc, ws, r0) = bin' (opc, wct ws, r0, true)
            fun pow2 k = IntInf.<< (1, Word.fromInt k)
            (* k such that n = 2^k *)
            fun log2 (n: IntInf.t): int option =
               let
                  fun loop k =
                     if k > 64 then NONE
                     else if pow2 k = n then SOME k
                     else loop (k + 1)
               in
                  loop 0
               end
            (* Can n be the immediate of a logical instruction on N-bit
             * registers?  It must be a repetition of an e-bit pattern that
             * is a rotation of a run of ones.
             *)
            fun bitmask (n: IntInf.t, N: int): bool =
               List.exists
               ([2, 4, 8, 16, 32, 64], fn e =>
                e <= N
                andalso
                let
                   val m = pow2 e - 1
                   val p = IntInf.andb (n, m)
                   fun rep (k, ac) =
                      if k >= N then ac
                      else rep (k + e,
                                IntInf.orb (ac, IntInf.<< (p, Word.fromInt k)))
                   fun strip q =
                      if IntInf.andb (q, 1) = 0
                         then strip (IntInf.~>> (q, 0w1))
                      else q
                   fun run q = IntInf.andb (strip q, strip q + 1) = 0
                in
                   rep (0, 0) = n andalso p <> 0 andalso p <> m
                   andalso (run p orelse run (m - p))
                end)
            fun logic (opc, ws) =
               let
                  val ct = wct ws
                  fun default () = bin (opc, ct, false)
               in
                  case a 1 of
                     Operand.Const (Const.Word w) =>
                        let
                           val n = WordX.toIntInf w
                        in
                           if bitmask (n, if size ct = 8 then 64 else 32)
                              then (emit (I.Bin (opc, reg (d, ct),
                                                 reg (v 0, ct), Opnd.Imm n))
                                    ; done ())
                           else default ()
                        end
                   | _ => default ()
               end
            (* Division (quotient or remainder) by a constant: multiply by a
             * precomputed reciprocal, or shift for a power of two.  The
             * arithmetic is N = 32 or 64 bits wide (narrower values are
             * extended to 32 bits).  With m = ceil (2^(N+s) / d) such that
             * m*d - 2^(N+s) <= 2^s, floor (n/d) = floor (m*n / 2^(N+s)) for
             * all N-bit n; the signed case is similar, with one bit less.
             * Returns false if it doesn't apply.
             *)
            fun divConst (ws, signed, isRem): bool =
               case a 1 of
                  Operand.Const (Const.Word w) =>
                     let
                        val N = if bits ws = 64 then 64 else 32
                        val dv = if signed then WordX.toIntInfX w
                                 else WordX.toIntInf w
                        fun r k = if N = 64 then x k else R.W k
                        fun ins (opc, d, s, z) = emit (I.Bin (opc, d, s, z))
                        fun lsr (r, k) = Opnd.Shifted (r, Opnd.LSR, k)
                     in
                        if dv < 2 then false
                        else
                           let
                              val n = ext (0, ws, signed, false)
                              (* the quotient is in register q *)
                              fun finish q =
                                 (if isRem
                                     then (movImm (16, dv)
                                           ; emit (I.Tern (I.MSUB, r d, r q,
                                                           r 16, r n)))
                                  else if q = d then ()
                                  else emit (I.Un (I.MOV, r d, r q))
                                  ; done ())
                              fun shiftBy (opc, k) =
                                 if k > 0
                                    then ins (opc, r 5, r 5, immOp k)
                                 else ()
                              (* x5 = the high half of n * x16 *)
                              fun umulh () =
                                 ins (I.UMULH, R.X 5, x n, regOp (R.X 16))
                           in
                              (case (log2 dv, signed) of
                                  (SOME k, false) =>
                                     (if isRem
                                         then ins (I.AND, r d, r n,
                                                   Opnd.Imm (dv - 1))
                                      else ins (I.LSR, r d, r n, immOp k)
                                      ; done ())
                                | (SOME k, true) =>
                                     (* round toward zero: add 2^k - 1 to a
                                      * negative dividend
                                      *)
                                     (ins (I.ASR, r 5, r n, immOp (N - 1))
                                      ; ins (I.ADD, r 5, r n, lsr (r 5, N - k))
                                      ; if isRem
                                           then (ins (I.AND, r 5, r 5,
                                                      Opnd.Imm (pow2 N - dv))
                                                 ; ins (I.SUB, r d, r n,
                                                        regOp (r 5)))
                                        else ins (I.ASR, r d, r 5, immOp k)
                                      ; done ())
                                | (NONE, true) =>
                                     let
                                        fun search s =
                                           let
                                              val p = pow2 (N - 1 + s)
                                              val m = p div dv + 1
                                           in
                                              if m * dv - p <= pow2 s
                                                 then (m, s)
                                              else if s >= N
                                                 then Error.bug
                                                      "VibeArmCodegen.divConst"
                                              else search (s + 1)
                                           end
                                        val (m, s) = search 1
                                     in
                                        movImm (16, m)
                                        ; if N = 64
                                             then ins (I.SMULH, R.X 5, x n,
                                                       regOp (R.X 16))
                                          else (ins (I.SMULL, R.X 5, R.W n,
                                                     regOp (R.W 16))
                                                ; ins (I.ASR, R.X 5, R.X 5,
                                                       immOp 32))
                                        ; if m >= pow2 (N - 1)
                                             then ins (I.ADD, r 5, r 5,
                                                       regOp (r n))
                                          else ()
                                        ; shiftBy (I.ASR, s - 1)
                                        ; ins (I.ADD, r 5, r 5, lsr (r n, N - 1))
                                        ; finish 5
                                     end
                                | (NONE, false) =>
                                     if N = 32
                                        then
                                           (* n < 2^32: one 64-bit multiply is
                                            * exact
                                            *)
                                           (movImm (16, pow2 64 div dv + 1)
                                            ; umulh ()
                                            ; finish 5)
                                     else
                                        let
                                           fun search s =
                                              let
                                                 val p = pow2 (64 + s)
                                                 val m = (p + dv - 1) div dv
                                              in
                                                 if m * dv - p <= pow2 s
                                                    then (m, s)
                                                 else if s >= 64
                                                    then Error.bug
                                                         "VibeArmCodegen.divConst"
                                                 else search (s + 1)
                                              end
                                           val (m, s) = search 0
                                        in
                                           if m < pow2 64
                                              then (movImm (16, m)
                                                    ; umulh ()
                                                    ; shiftBy (I.LSR, s))
                                           else
                                              (* m needs 65 bits *)
                                              (movImm (16, m - pow2 64)
                                               ; umulh ()
                                               ; ins (I.SUB, R.X 6, x n,
                                                      regOp (R.X 5))
                                               ; ins (I.ADD, R.X 5, R.X 5,
                                                      lsr (R.X 6, 1))
                                               ; shiftBy (I.LSR, s - 1))
                                           ; finish 5
                                        end)
                              ; true
                           end
                     end
                | _ => false
            (* compare-and-swap; setAddr puts the address in x7 *)
            fun cas (ct, kOld, kNew, obj, setAddr) =
               let
                  fun iv k =
                     let
                        (* A real is moved to x<k> at once, so d16 will do
                         * for each (scratch 3 would be d19, which isn't a
                         * scratch register).
                         *)
                        val r = if isReal (cty (a k))
                                   then value (a k, scratch (cty (a k), 0), 3)
                                else v k
                     in
                        if r >= 32
                           then (emit (I.Un (I.FMOV, reg (k, ct), reg (r, ct)))
                                 ; k)
                        else r
                     end
                  val old = iv kOld
                  val new = iv kNew
                  val () = setAddr ()
                  val (ldaxr, stlxr) =
                     case size ct of
                        1 => (I.LDAXRB, I.STLXRB)
                      | 2 => (I.LDAXRH, I.STLXRH)
                      | _ => (I.LDAXR, I.STLXR)
               in
                  emit (I.LocalLabel 1)
                  ; emit (I.Load (ldaxr, reg (5, ct), Addr.Base (R.X 7)))
                  ; emit (I.Cmp (I.CMP, reg (5, ct), regOp (reg (old, ct))))
                  ; emit (I.Bcond (Cond.NE, Tgt.Forward 2))
                  ; emit (I.StoreX (stlxr, R.W 6, reg (new, ct),
                                    Addr.Base (R.X 7)))
                  ; emit (I.Cbnz (R.W 6, Tgt.Backward 1))
                  ; emit (I.LocalLabel 2)
                  ; (case ct of
                        CType.Objptr =>
                           let
                              (* We don't know what is live here, so commit all
                               * of the caller-save temporaries.
                               *)
                              val saves =
                                 if localRA then []
                                 else callerSaves (List.map (!curWebs, #2)
                                                   @ List.map (!curPromoted, #2))
                           in
                              mov (0, obj, ct)
                              ; emit (I.Un (I.MOV, R.X 1, R.X 5))
                              ; commit (str, saves)
                              ; flushCallerSaves (fn _ => true)
                              ; emit (I.Bl (Lbl.Sym "Assignable_decheckObjptr"))
                              ; clearCache ()
                              ; commit (ldr, saves)
                              ; mov (d, 0, ct)
                           end
                      | _ => if isReal ct
                                then emit (I.Un (I.FMOV, reg (d, ct),
                                                 reg (5, ct)))
                             else mov (d, 5, ct))
                  ; done ()
               end
         in
            case prim of
               Array_cas (SOME ct) =>
                  let
                     val obj = v 0
                     val ix = v 1
                     val lg = case size ct of 1 => 0 | 2 => 1 | 4 => 2 | _ => 3
                  in
                     cas (ct, 2, 3, obj, fn () =>
                          emit (I.Bin (I.ADD, R.X 7, x obj,
                                       Opnd.Shifted (x ix, Opnd.LSL, lg))))
                  end
             | CPointer_add => addsub (true, ptr)
             | CPointer_diff => bin (I.SUB, ptr, false)
             | CPointer_equal => cmp (I.CMP, ptr, Cond.EQ)
             | CPointer_fromWord => move {dst = dst, src = a 0}
             | CPointer_lt => cmp (I.CMP, ptr, Cond.LO)
             | CPointer_sub => addsub (false, ptr)
             | CPointer_toWord => move {dst = dst, src = a 0}
             | Real_Math_sqrt rs => un (I.FSQRT, rct rs)
             | Real_abs rs => un (I.FABS, rct rs)
             | Real_add rs => bin (I.FADD, rct rs, false)
             | Real_castToWord (rs, _) => un (I.FMOV, rct rs)
             | Real_div rs => bin (I.FDIV, rct rs, false)
             | Real_equal rs => cmp (I.FCMP, rct rs, Cond.EQ)
             | Real_le rs => cmp (I.FCMP, rct rs, Cond.LS)
             | Real_lt rs => cmp (I.FCMP, rct rs, Cond.MI)
             | Real_mul rs => bin (I.FMUL, rct rs, false)
             | Real_muladd rs => tern (I.FMADD, rct rs)
             | Real_mulsub rs => tern (I.FNMSUB, rct rs)
             | Real_neg rs => un (I.FNEG, rct rs)
             | Real_qequal rs =>
                  (emit (I.Cmp (I.FCMP, reg (v 0, rct rs),
                                regOp (reg (v 1, rct rs))))
                   ; emit (I.Cset (R.W 5, Cond.EQ))
                   ; emit (I.Sel (I.CSINC, reg (d, dct), R.W 5, R.WZR, Cond.VC))
                   ; done ())
             | Real_rndToReal (rs, _) => un (I.FCVT, rct rs)
             | Real_rndToWord (rs, _, {signed}) =>
                  un (if signed then I.FCVTZS else I.FCVTZU, rct rs)
             | Real_round rs => un (I.FRINTX, rct rs)
             | Real_sub rs => bin (I.FSUB, rct rs, false)
             | Ref_cas (SOME ct) =>
                  let
                     val obj = v 0
                  in
                     cas (ct, 1, 2, obj, fn () => mov (7, obj, ptr))
                  end
             | Word_add ws => addsub (true, wct ws)
             | Word_addCheckP z => chk (Add, z, [0, 1])
             | Word_andb ws => logic (I.AND, ws)
             | Word_castToReal (ws, _) => un (I.FMOV, wct ws)
             | Word_equal ws => cmp (I.CMP, wct ws, Cond.EQ)
             | Word_extdToWord (ws1, ws2, {signed}) =>
                  let
                     val r = v 0
                  in
                     if signed andalso bits ws1 < bits ws2
                        then emit (I.Un (sxt (bits ws1), reg (d, wct ws2),
                                         R.W r))
                     else if bits ws1 = 64 andalso bits ws2 = 32
                        (* A truncation must clear the upper half of the
                         * register, even when d = r.
                         *)
                        then emit (I.Un (I.MOV, R.W d, R.W r))
                     else mov (d, r, wct ws2)
                     ; done ()
                  end
             | Word_lshift ws => shift (I.LSL, ws, v 0)
             | Word_lt (ws, {signed}) =>
                  let
                     (* a constant that can be an immediate *)
                     fun imm k =
                        case a k of
                           Operand.Const (Const.Word w) =>
                              let
                                 val n = if signed then WordX.toIntInfX w
                                         else WordX.toIntInf w
                              in
                                 if ~4096 < n andalso n < 4096 then SOME n
                                 else NONE
                              end
                         | _ => NONE
                     fun cmpImm (k, n, cond) =
                        let
                           val r = reg (ext (k, ws, signed, false), wct ws)
                        in
                           if n >= 0
                              then emit (I.Cmp (I.CMP, r, Opnd.Imm n))
                           else emit (I.Cmp (I.CMN, r, Opnd.Imm (~ n)))
                           ; cset cond
                        end
                  in
                     case (imm 0, imm 1) of
                        (_, SOME n) =>
                           cmpImm (0, n, if signed then Cond.LT else Cond.LO)
                      | (SOME n, NONE) =>
                           cmpImm (1, n, if signed then Cond.GT else Cond.HI)
                      | _ =>
                           let
                              val r0 = ext (0, ws, signed, false)
                              val r1 = ext (1, ws, signed, false)
                           in
                              emit (I.Cmp (I.CMP, reg (r0, wct ws),
                                           regOp (reg (r1, wct ws))))
                              ; cset (if signed then Cond.LT else Cond.LO)
                           end
                  end
             | Word_mul (ws, _) =>
                  let
                     val ct = wct ws
                     fun default () = bin (I.MUL, ct, false)
                  in
                     (* by 2^k or 2^k + 1: shift, or shift and add *)
                     case (a 1, bits ws >= 32) of
                        (Operand.Const (Const.Word w), true) =>
                           let
                              val n = WordX.toIntInf w
                              val r = reg (v 0, ct)
                           in
                              case (log2 n, log2 (n - 1)) of
                                 (SOME k, _) =>
                                    (emit (I.Bin (I.LSL, reg (d, ct), r,
                                                  immOp k))
                                     ; done ())
                               | (_, SOME k) =>
                                    (emit (I.Bin (I.ADD, reg (d, ct), r,
                                                  Opnd.Shifted (r, Opnd.LSL,
                                                                k)))
                                     ; done ())
                               | _ => default ()
                           end
                      | _ => default ()
                  end
             | Word_mulCheckP z => chk (Mul, z, [0, 1])
             | Word_neg ws => un (I.NEG, wct ws)
             | Word_negCheckP z => chk (Neg, z, [0])
             | Word_notb ws => un (I.MVN, wct ws)
             | Word_orb ws => logic (I.ORR, ws)
             | Word_quot (ws, {signed}) =>
                  if divConst (ws, signed, false) then ()
                  else
                  let
                     val r0 = ext (0, ws, signed, false)
                     val r1 = ext (1, ws, signed, false)
                  in
                     emit (I.Bin (if signed then I.SDIV else I.UDIV,
                                  reg (d, wct ws), reg (r0, wct ws),
                                  regOp (reg (r1, wct ws))))
                     ; done ()
                  end
             | Word_rem (ws, {signed}) =>
                  if divConst (ws, signed, true) then ()
                  else
                  let
                     val ct = wct ws
                     val r0 = ext (0, ws, signed, false)
                     val r1 = ext (1, ws, signed, false)
                  in
                     emit (I.Bin (if signed then I.SDIV else I.UDIV,
                                  reg (5, ct), reg (r0, ct),
                                  regOp (reg (r1, ct))))
                     ; emit (I.Tern (I.MSUB, reg (d, ct), reg (5, ct),
                                     reg (r1, ct), reg (r0, ct)))
                     ; done ()
                  end
             | Word_rndToReal (ws, _, {signed}) =>
                  (emit (I.Un (if signed then I.SCVTF else I.UCVTF,
                               reg (d, dct),
                               reg (ext (0, ws, signed, false), wct ws)))
                   ; done ())
             | Word_rol ws =>
                  let
                     val ct = wct ws
                     val r0 = v 0
                  in
                     emit (I.Un (I.NEG, reg (5, ct), reg (v 1, ct)))
                     ; emit (I.Bin (I.ROR, reg (d, ct), reg (r0, ct),
                                    regOp (reg (5, ct))))
                     ; done ()
                  end
             | Word_ror ws =>
                  (case a 1 of
                      Operand.Const (Const.Word w) =>
                         (emit (I.Bin (I.ROR, reg (d, wct ws),
                                       reg (v 0, wct ws),
                                       Opnd.Imm
                                       (WordX.toIntInf w
                                        mod IntInf.fromInt (bits ws))))
                          ; done ())
                    | _ => shift (I.ROR, ws, v 0))
             | Word_rshift (ws, {signed}) =>
                  if Vector.length args = 3
                     then
                        (* a bitfield extract: see "direct", below *)
                        let
                           fun const k =
                              case a k of
                                 Operand.Const (Const.Word w) => WordX.toIntInf w
                               | _ => Error.bug "VibeArmCodegen.primApp: extract"
                           val ct = wct ws
                        in
                           emit (I.Ubfx (reg (d, ct), reg (v 0, ct),
                                         IntInf.toInt (const 1),
                                         valOf (log2 (const 2 + 1))))
                           ; done ()
                        end
                  else
                  shift (if signed then I.ASR else I.LSR, ws,
                         ext (0, ws, signed, false))
             | Word_sub ws => addsub (false, wct ws)
             | Word_subCheckP z => chk (Sub, z, [0, 1])
             | Word_xorb ws => logic (I.EOR, ws)
             | _ => Error.bug (concat ["VibeArmCodegen.primApp: ",
                                       Prim.toString prim])
         end

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
      (* (Local allocator.)  Live transfers: what registers hold at the start
       * of a block is fixed by the first of its predecessors to be output;
       * the others conform.
       *)
      fun liveIn l =
         List.map (liveTemps (labelBlock l), fn e =>
                   T (Temp.ct e, Temp.index e))
      fun member (ls, l) = List.exists (ls, fn l' => sameLoc (l, l'))
      (* Does the edge to l need code?  (Fixes what l expects, if nothing
       * has yet.)
       *)
      fun edgeNeedsCode l =
         let
            val lin = liveIn l
            val state = entryState l
         in
            case !state of
               NONE =>
                  (state := SOME (List.keepAllMap
                                  (!regs, fn {reg, loc, dirty} =>
                                   if (case loc of
                                          S k =>
                                             List.exists
                                             (#promoted (blockWebs l),
                                              fn (k', _) => sameSlot (k, k'))
                                        | T _ => member (lin, loc))
                                      then SOME (loc, reg, not (!dirty))
                                   else NONE))
                   ; false)
             | SOME m =>
                  List.exists
                  (m, fn (l, r, _) =>
                   case l of
                      S _ => (case inReg l of
                                 NONE => true
                               | SOME {reg, ...} => reg <> r)
                    | T _ => false)
                  orelse
                  List.exists
                  (lin, fn l =>
                   case (List.peek (m, fn (l', _, _) => sameLoc (l, l')),
                         inReg l) of
                      (NONE, NONE) => false
                    | (NONE, SOME {dirty, ...}) => !dirty
                    | (SOME _, NONE) => true
                    | (SOME (_, r, clean), SOME {reg, dirty, ...}) =>
                         r <> reg orelse (clean andalso !dirty))
         end
      fun parallelMove ms =
         case ms of
            [] => ()
          | (d, s) :: _ =>
               let
                  fun mv (d, s) =
                     if d >= 32
                        then emit (I.Un (I.FMOV, R.D (d - 32), R.D (s - 32)))
                     else emit (I.Un (I.MOV, x d, x s))
               in
                  case List.peek (ms, fn (d, _) =>
                                  not (List.exists (ms, fn (_, s) => s = d))) of
                     SOME (d, s) =>
                        (mv (d, s)
                         ; parallelMove (List.keepAll (ms, fn (d', _) =>
                                                       d' <> d)))
                   | NONE =>
                        (* a cycle: break it with a scratch register *)
                        let
                           val t = if d >= 32 then 48 else 16
                        in
                           mv (t, s)
                           ; parallelMove (List.map (ms, fn (d', s') =>
                                                     (d', if s' = s then t
                                                          else s')))
                        end
               end
      (* make the registers be what l expects *)
      fun conform l =
         let
            val lin = liveIn l
            val m = valOf (! (entryState l))
            fun expects l = List.peek (m, fn (l', _, _) => sameLoc (l, l'))
            val () =
               List.foreach
               (!regs, fn e as {loc, dirty, ...} =>
                if isT loc andalso member (lin, loc) andalso !dirty
                   andalso (case expects loc of
                               NONE => true
                             | SOME (_, _, clean) => clean)
                   then commitEntry e
                else ())
            val (moves, loads) =
               List.fold
               (m, ([], []), fn ((l, r, _), (moves, loads)) =>
                case inReg l of
                   NONE => (moves, (l, r) :: loads)
                 | SOME {reg, ...} =>
                      if reg = r then (moves, loads)
                      else ((r, reg) :: moves, loads))
         in
            parallelMove moves
            ; List.foreach
              (loads, fn (l, r) =>
               case l of
                  S (off, ct) =>
                     ldr (ct, r, mem (20, off, ct))
                | T (ct, n) =>
                     let
                        val (b, off) = homeOf (ct, n)
                     in
                        ldr (ct, r, mem (b, off, ct))
                     end)
         end
      (* compensation blocks, for edges that need code: output at the end of
       * the chunk
       *)
      val comps: (Lbl.t * Label.t * rentry list) list ref = ref []
      (* the label to branch to in order to get to l *)
      fun edge l =
         if localRA andalso edgeNeedsCode l
            then
               let
                  val name =
                     (if symbols then Lbl.Sym else Lbl.Plain)
                     (concat [if symbols then "edge_" else "Ledge_",
                              self, "_", int (List.length (!comps))])
               in
                  List.push
                  (comps, (name, l,
                           List.map (!regs, fn {reg, loc, dirty} =>
                                     {reg = reg, loc = loc, dirty = ref (!dirty)})))
                  ; name
               end
         else target l
      (* go to l at the end of a block *)
      fun gotoL l =
         (if localRA
             then
                let
                   val {promoted, ploop, ...} = blockWebs l
                   (* On entering a region whose blocks haven't been output:
                    * load its promoted stack slots into registers that are
                    * free, so that they are in registers around the loop.
                    *)
                   val () =
                      if ploop <> 0
                         andalso ploop <> #ploop (blockWebs (!curLabel))
                         andalso not (Option.isSome (! (entryState l)))
                         then
                            List.foreach
                            (promoted, fn (k as (off, ct), _) =>
                             if Option.isSome (inReg (S k)) then ()
                             else
                                case List.peek
                                     (if isReal ct then fcaller @ fcallee
                                      else caller @ callee,
                                      fn r =>
                                      not (List.exists (!regs, fn {reg, ...} =>
                                                        reg = r))) of
                                   NONE => ()
                                 | SOME r =>
                                      (ldr (ct, r, mem (20, off, ct))
                                       ; List.push (regs, {reg = r, loc = S k,
                                                           dirty = ref false})))
                      else ()
                in
                   if edgeNeedsCode l then conform l else ()
                end
          else ()
          ; goto l)
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
            then (bcond (negate c, edge l'); gotoL l)
         else (bcond (c, edge l); gotoL l')
      (* Output an if-converted switch; the flags hold condition c, which
       * selects the case (rather than the default) target.
       *)
      fun selects (c, {caseLabel, caseMoves, defaultLabel, defaultMoves,
                       join}) =
         let
            (* The selects write temporaries of other blocks, whose registers
             * the cache may be using.
             *)
            val () = clearCache ()
            val () = cacheable := []
            (* Set dst with the instruction that set outputs, given dst's
             * register (which holds dst's value, if old).
             *)
            fun setDst (dst, old, set) =
               let
                  val ct = cty dst
                  val (d, spilled) =
                     if localRA
                        then (value (dst, scratch (ct, 0), 3), true)
                     else
                     case Option.fold (dstTemp dst, NONE, fn (t, _) =>
                                       defHome t) of
                        SOME h => (h, false)
                      | NONE => (scratch (ct, 0), true)
               in
                  (* The old value of a spilled destination is in its slot
                   * (not where the block finds the temporary on entry,
                   * which is what load would read).
                   *)
                  if spilled andalso not localRA andalso old
                     then ldr (ct, d, memOf (dst, 3, ct))
                  else ()
                  ; set d
                  ; if spilled then store (dst, d) else ()
                  ; defined dst
               end
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
            ; gotoL join
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
            then gotoL l
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
       *
       * The local register allocator outputs code on edges and at the start
       * of a block that hasn't been checked against this, so it keeps to the
       * frame.
       *)
      fun directReturn (l: Label.t): bool =
         not localRA
         andalso (case labelBlock l of
                     Block.T {kind as Kind.CReturn {func, ...}, ...} =>
                        not (CFunction.maySwitchThreadsFrom func)
                        andalso not (Kind.isEntry kind)
                   | _ => false)
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
                         ; gotoL return
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
            (* Commit the caller-save temporaries that are live across the
             * call.
             *)
            val saves =
               case (localRA, return) of
                  (false, SOME {return, ...}) =>
                     callerSaves (List.map (liveTemps (labelBlock return),
                                            fn e => segIn (return, e))
                                  @ List.map (!curPromoted, #2))
                | _ => []
            val () = commit (str, saves)
            val () =
               let
                  val live = case return of
                                NONE => []
                              | SOME {return, ...} => liveIn return
               in
                  commitCallerSaves (fn l => member (live, l))
               end
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
            val () = spAdj := stk
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
            val () = spAdj := 0
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
            val () = clearCache ()
            val () = dropCallerSaves ()
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
            val () = commit (ldr, saves)
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
                   | SOME {return, ...} => gotoL return)
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
          | Transfer.Goto l => gotoL l
          | Transfer.Raise {raisesTo} =>
               (clearCache ()
                ; emit (I.Bin
                        (I.ADD, R.X 20,
                         x (value (gcField GCField.StackBottom, 0, 3)),
                         regOp (x (value (gcField GCField.ExnStack, 1, 3)))))
                ; rtrans raisesTo)
          | Transfer.Return {returnsTo} => rtrans returnsTo
          | Transfer.Spork {cont, ...} => gotoL cont
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
                                    ; bcond (CC Cond.HI, edge default)
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
                                        (edge
                                         (case List.peek
                                               (cases', fn (m, _) =>
                                                m = lo + IntInf.fromInt k) of
                                             SOME (_, l) => l
                                           | NONE => default),
                                         table)))
                                 end
                           else (List.foreach (cases, fn (w, l) =>
                                               bcond (eq w, edge l))
                                 ; gotoL default)
                        end
               end
      (* The statements of a block, with each adjacent pair of an operation
       * and its overflow check (on the same arguments) combined, so that
       * the arithmetic is only done once.
       *)
      datatype item =
         App of {args: Operand.t vector,
                 dst: Operand.t,
                 prim: Type.t Prim.t,
                 also: (Operand.t * bool) option}
       | Move of {dst: Operand.t, src: Operand.t}
         (* a definition that was folded into the previous item *)
       | Defined of Operand.t
      fun items (ss: Statement.t list): item list =
         case ss of
            [] => []
          | Statement.Move z :: ss => Move z :: items ss
          | Statement.PrimApp {dst = NONE, ...} :: ss =>
               (* all primitives are pure *)
               items ss
          | Statement.PrimApp {args, dst = SOME dst, prim} :: ss =>
               let
                  datatype z = datatype Prim.t
                  fun single () =
                     App {args = args, dst = dst, prim = prim, also = NONE}
                     :: items ss
                  (* is q the overflow check for p? *)
                  fun checks (p, q) =
                     case (p, q) of
                        (Word_add s, Word_addCheckP (s', _)) =>
                           WordSize.equals (s, s')
                      | (Word_mul (s, _), Word_mulCheckP (s', _)) =>
                           WordSize.equals (s, s')
                      | (Word_neg s, Word_negCheckP (s', _)) =>
                           WordSize.equals (s, s')
                      | (Word_sub s, Word_subCheckP (s', _)) =>
                           WordSize.equals (s, s')
                      | _ => false
                  fun simple z =
                     case z of
                        Operand.StackOffset _ => true
                      | Operand.Temporary _ => true
                      | _ => false
               in
                  case ss of
                     Statement.PrimApp {args = args', dst = SOME dst',
                                        prim = prim'} :: ss' =>
                        if Vector.equals (args, args', Operand.equals)
                           andalso simple dst andalso simple dst'
                           andalso not (Operand.interfere (dst, dst'))
                           andalso not (Vector.exists
                                        (args, fn z =>
                                         Operand.interfere (dst, z)
                                         orelse Operand.interfere (dst', z)))
                           then if checks (prim, prim')
                                   then App {args = args, dst = dst',
                                             prim = prim',
                                             also = SOME (dst, false)}
                                        :: items ss'
                                else if checks (prim', prim)
                                   then App {args = args, dst = dst,
                                             prim = prim,
                                             also = SOME (dst', true)}
                                        :: items ss'
                                else single ()
                        else single ()
                   | _ => single ()
               end
      fun outputApp {args, dst, prim, also = a} =
         (also := a; primApp (prim, args, dst); also := NONE)
      (* When the result of a primitive is only copied to a stack slot,
       * compute it into the slot (which may be in a register).
       *)
      fun direct (its: item list, transfer): item list =
         let
            fun same (t, t') =
               Temp.equals (temp t, temp t')
            fun mentions (z, t) =
               let
                  val r = ref false
               in
                  foreachTemp (z, fn t' => if same (t, t') then r := true else ())
                  ; !r
               end
            fun dead (t, rest) =
               not (List.exists
                    (rest, fn it =>
                     case it of
                        App {args, dst, also, ...} =>
                           mentions (dst, t)
                           orelse (case also of
                                      NONE => false
                                    | SOME (z, _) => mentions (z, t))
                           orelse Vector.exists (args, fn z => mentions (z, t))
                      | Defined _ => false
                      | Move {dst, src} =>
                           mentions (dst, t) orelse mentions (src, t)))
               andalso not (Transfer.foldOperands
                            (transfer, false, fn (z, b) =>
                             b orelse mentions (z, t)))
               andalso not (List.exists
                            (nearSuccessors transfer
                             @ (case ifConvert transfer of
                                   NONE => []
                                 | SOME {join, ...} => [join]),
                             fn l =>
                             List.exists (liveTemps (labelBlock l), fn e =>
                                          Temp.equals (e, temp t))))
            (* A shift right by k and then a mask of the low w bits is a
             * bitfield extract; the mask becomes a third argument of the
             * shift (see Word_rshift in primApp).
             *)
            fun extract (it, it', rest) =
               case (it, it') of
                  (App {args, dst = d, prim = Prim.Word_rshift (ws, {signed = false}),
                        also = NONE},
                   App {args = args', dst = d', prim = Prim.Word_andb ws',
                        also = NONE}) =>
                     (case (dstTemp d, Vector.sub (args, 1),
                            dstTemp (Vector.sub (args', 0)),
                            Vector.sub (args', 1)) of
                         (SOME t, Operand.Const (Const.Word k), SOME t',
                          mask as Operand.Const (Const.Word m)) =>
                            let
                               val n = Bits.toInt (WordSize.bits ws)
                               val k = WordX.toIntInf k
                               val m = WordX.toIntInf m
                               fun width (w: int) =
                                  if w > 64 then NONE
                                  else if IntInf.<< (1, Word.fromInt w) - 1 = m
                                     then SOME w
                                  else width (w + 1)
                               val keep =
                                  case dstTemp d' of
                                     SOME t'' => same (t, t'')
                                   | NONE => false
                            in
                               case width 1 of
                                  SOME w =>
                                     if n >= 32
                                        andalso WordSize.equals (ws, ws')
                                        andalso same (t, t')
                                        andalso k + IntInf.fromInt w
                                                <= IntInf.fromInt n
                                        andalso not (mentions (Vector.sub (args, 0), t))
                                        andalso (keep orelse dead (t, rest))
                                        then SOME ((if keep then [Defined d]
                                                    else [])
                                                   @ App {args = Vector.new3
                                                               (Vector.sub (args, 0),
                                                                Vector.sub (args, 1),
                                                                mask),
                                                        dst = d',
                                                        prim = Prim.Word_rshift
                                                               (ws, {signed = false}),
                                                        also = NONE}
                                                   :: (if keep then []
                                                       else [Defined d]))
                                     else NONE
                                | NONE => NONE
                            end
                       | _ => NONE)
                | _ => NONE
         in
            case its of
               (it as App {prim = Prim.Word_rshift _, ...})
               :: (it' as App _) :: rest =>
                  (case extract (it, it', rest) of
                      SOME its => its @ direct (rest, transfer)
                    | NONE => it :: direct (it' :: rest, transfer))
             | (it as App {args, dst = d, prim, also = NONE})
               :: (it' as Move {dst = d', src}) :: rest =>
                  (case (dstTemp d, stripCast d', dstTemp src) of
                      (SOME t, Operand.StackOffset _, SOME t') =>
                         if same (t, t')
                            andalso CType.equals (cty d, cty d')
                            andalso dead (t, rest)
                            then App {args = args, dst = d', prim = prim,
                                      also = NONE}
                                 :: Defined d :: direct (rest, transfer)
                         else it :: direct (it' :: rest, transfer)
                    | _ => it :: direct (it' :: rest, transfer))
             | it :: rest => it :: direct (rest, transfer)
             | [] => []
         end
      fun outputItem it =
         case it of
            App z => outputApp z
          | Defined _ => ()
          | Move z => move z
      (* Is this a block that ends by computing a boolean temporary that is
       * only used to choose between two successors?  If so, return the
       * other items, the final one, and the targets for true and false.
       *)
      fun fusable (its, transfer) =
         case (transfer, its) of
            (_, []) => NONE
          | (Transfer.Switch (Switch.T {cases, default,
                                        test = Operand.Temporary t, ...}),
             _) =>
               (case (List.splitLast its, switchCases (cases, default)) of
                   ((its, App (a as {dst = Operand.Temporary t', ...})),
                    ([(w, l)], l')) =>
                      let
                         val e = temp t
                         fun dead l =
                            not (List.exists (liveTemps (labelBlock l), fn e' =>
                                              Temp.equals (e, e')))
                      in
                         if Temporary.equals (t, t')
                            andalso dead l andalso dead l'
                            then if WordX.isOne w
                                    then SOME (its, a, l, l')
                                 else if WordX.isZero w
                                    then SOME (its, a, l', l)
                                 else NONE
                         else NONE
                      end
                 | _ => NONE)
          | _ => NONE
      (* Find the stack slots that are read more than once in a block, and
       * the registers that no temporary uses in the block.
       *)
      fun setupCache (Block.T {kind, label, statements, transfer, ...}) =
         let
            (* the registers of the webs that are live somewhere in the block *)
            val busy =
               let
                  val {segIn, defs, promoted, ...} = blockWebs label
               in
                  List.keepAllMap (segIn @ defs, fn (_, w) => webHome w)
                  @ List.keepAllMap (promoted, fn (_, w) => webHome w)
               end
            fun temp (_: Temporary.t) = ()
            val reads: ((int * CType.t) * int ref) list ref = ref []
            fun read so =
               let
                  val k = slotKey so
               in
                  case List.peek (!reads, fn (k', _) => sameSlot (k, k')) of
                     SOME (_, n) => Int.inc n
                   | NONE => List.push (reads, (k, ref 1))
               end
            fun visit (z, isDst) =
               case z of
                  Operand.Address z => visit (z, true)
                | Operand.Cast (z, _) => visit (z, isDst)
                | Operand.Offset {base, ...} => visit (base, false)
                | Operand.SequenceOffset {base, index, ...} =>
                     (visit (base, false); visit (index, false))
                | Operand.StackOffset so => if isDst then () else read so
                | Operand.Temporary t => temp t
                | _ => ()
            val () =
               case kind of
                  Kind.CReturn {dst = SOME z, ...} =>
                     visit (Live.toOperand z, true)
                | _ => ()
            val () =
               Vector.foreach
               (statements, fn s =>
                case s of
                   Statement.Move {dst, src} =>
                      (visit (dst, true); visit (src, false))
                 | Statement.PrimApp {args, dst, ...} =>
                      (Option.app (dst, fn z => visit (z, true))
                       ; Vector.foreach (args, fn z => visit (z, false))))
            val () =
               Transfer.foldOperands (transfer, (), fn (z, ()) =>
                                      visit (z, false))
         in
            clearCache ()
            ; cacheable := List.keepAllMap (!reads, fn (k, n) =>
                                            if !n >= 2 then SOME k else NONE)
            ; freeRegs := List.keepAll
                          (caller @ callee @ fcaller @ fcallee, fn r =>
                           not (List.contains (busy, r, op =)))
         end
      (* (Local allocator.)  Find where each temporary and stack slot occurs
       * in a block, and start with the registers that the block expects.
       *)
      fun setupLocal (Block.T {label, transfer, ...}, its) =
         let
            val table: (loc * int list ref) list ref = ref []
            fun add (l, k) =
               case List.peek (!table, fn (l', _) => sameLoc (l, l')) of
                  SOME (_, ps) => List.push (ps, k)
                | NONE => List.push (table, (l, ref [k]))
            fun visit (z, isDst, k) =
               case z of
                  Operand.Address z => visit (z, true, k)
                | Operand.Cast (z, _) => visit (z, isDst, k)
                | Operand.Offset {base, ...} => visit (base, false, k)
                | Operand.SequenceOffset {base, index, ...} =>
                     (visit (base, false, k); visit (index, false, k))
                | Operand.StackOffset so =>
                     if isDst then () else add (S (slotKey so), k)
                | Operand.Temporary t => add (tloc t, k)
                | _ => ()
            val n = List.length its
            val () =
               List.foreachi
               (its, fn (k, it) =>
                case it of
                   App {args, dst, also, ...} =>
                      (visit (dst, true, k)
                       ; Option.app (also, fn (z, _) => visit (z, true, k))
                       ; Vector.foreach (args, fn z => visit (z, false, k)))
                 | Defined _ => ()
                 | Move {dst, src} =>
                      (visit (dst, true, k); visit (src, false, k)))
            val () =
               Transfer.foldOperands (transfer, (), fn (z, ()) =>
                                      visit (z, false, n))
            val conv = ifConvert transfer
            val () =
               Option.app
               (conv, fn {caseMoves, defaultMoves, ...} =>
                List.foreach (caseMoves @ defaultMoves, fn {dst, src} =>
                              (visit (dst, true, n); visit (src, false, n))))
            val state = entryState label
         in
            occs := List.map (!table, fn (l, ps) => (l, List.rev (!ps)))
            ; liveOut := List.fold
                         (nearSuccessors transfer
                          @ (case conv of
                                NONE => []
                              | SOME {join, ...} => [join]),
                          [], fn (l, ac) => liveIn l @ ac)
            ; regs := (case !state of
                          NONE => (state := SOME []; [])
                        | SOME m =>
                             List.map (m, fn (l, r, clean) =>
                                       {reg = r, loc = l,
                                        dirty = ref (not clean)}))
            ; pinned := []
            ; reserved := []
            ; pos := ~1
         end
      fun beginItem k =
         if localRA then (pos := k; pinned := []; reserved := []) else ()
      (* after an item, drop what is dead *)
      fun endItem () =
         if localRA
            then (pos := !pos + 1
                  ; pinned := []
                  ; reserved := []
                  ; regs := List.keepAll
                            (!regs, fn {loc, ...} =>
                             isLiveOut loc
                             orelse List.exists (occsOf loc, fn p =>
                                                 p >= !pos)))
         else ()
      (* an item has been output: so have its definitions *)
      fun afterItem it =
         (case it of
             App {dst, also, ...} =>
                (defined dst; Option.app (also, fn (z, _) => defined z))
           | Defined z => defined z
           | Move {dst, ...} => defined dst
          ; endItem ())
      fun outputItems (its, k0) =
         List.foreachi (its, fn (k, it) =>
                        (beginItem (k0 + k); outputItem it; afterItem it))
      fun outputBlock (b as Block.T {kind, label = l, statements, transfer,
                                     ...}) =
         (curLabel := l
          ; if Kind.isEntry kind
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
          ; region := blockRegion l
          ; enterBlock l
          ; setupCache b
          ; if localRA
               then setupLocal (b, direct (items (Vector.toList statements),
                                           transfer))
            else ()
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
               val its = direct (items (Vector.toList statements), transfer)
               val n = List.length its
               val () = endItem ()
            in
               case fusable (its, transfer) of
                  NONE => (outputItems (its, 0)
                           ; beginItem n
                           ; outputTransfer transfer)
                | SOME (its, a, l, l') =>
                     (outputItems (its, 0)
                      ; beginItem (n - 1)
                      ; fused := NONE
                      ; fusing := true
                      ; outputApp a
                      ; fusing := false
                      ; afterItem (App a)
                      ; beginItem n
                      ; (case !fused of
                            NONE => outputTransfer transfer
                          | SOME c =>
                               (case ifConvert transfer of
                                   NONE => twoWay (CC c, l, l')
                                 | SOME (z as {caseLabel, ...}) =>
                                      selects
                                      (if Label.equals (l, caseLabel)
                                          then c
                                       else Cond.negate c,
                                       z))))
            end)

      (* Block layout: greedily extend a trace with the successor that is
       * most likely to be next.  For a two-way switch, that is the target
       * that the backend expects, else the one that is deeper in the loop
       * nest, else the one that isn't cold, else the default.
       *)
      val order: Block.t list ref = ref []
      val pending: Label.t list ref = ref []
      val {get = placedRef: Label.t -> bool ref, ...} =
         Property.get (Label.plist, Property.initFun (fn _ => ref false))
      fun placed l = ! (placedRef l)
      fun cold (l, depth) =
         case labelBlock l of
            Block.T {transfer, ...} =>
               (case transfer of
                   Transfer.CCall {return = NONE, ...} => true
                 | Transfer.CCall {return = SOME {size = SOME _, ...}, ...} =>
                      true
                 | Transfer.Goto l => depth > 0 andalso cold (l, depth - 1)
                 | Transfer.Raise _ => true
                 | _ => false)
      fun trace from =
         if placed from then ()
         else
            let
               val l = from
               val b as Block.T {transfer, ...} = labelBlock l
               val () = placedRef l := true
               val () = List.push (order, b)
               (* the successors that could fall through, best first *)
               val succs =
                  case transfer of
                     Transfer.CCall {return = SOME {return, ...}, ...} =>
                        [return]
                   | Transfer.Call {label, ...} =>
                        if ChunkLabel.equals (labelChunk label, chunkLabel)
                           then [label]
                        else []
                   | Transfer.Goto l => [l]
                   | Transfer.Spork {cont, ...} => [cont]
                   | Transfer.Switch (Switch.T {cases, default, expect, ...}) =>
                        (case (ifConvert transfer,
                               switchCases (cases, default)) of
                            (SOME {join, ...}, _) => [join]
                          | (NONE, ([(w, l)], l')) =>
                               let
                                  fun byCold () =
                                     cold (l', 2) andalso not (cold (l, 2))
                                  (* prefer to stay in, or go deeper into,
                                   * a loop
                                   *)
                                  fun byLoop () =
                                     case (loopDistance (from, l),
                                           loopDistance (from, l')) of
                                        (SOME _, NONE) => true
                                      | (NONE, SOME _) => false
                                      | (SOME d, SOME d') =>
                                           if d = d' then byCold () else d > d'
                                      | (NONE, NONE) => byCold ()
                               in
                                  if (case expect of
                                         SOME w' => WordX.equals (w, w')
                                       | NONE => byLoop ())
                                     then [l, l']
                                  else [l', l]
                               end
                          | (NONE, (cases, l)) => l :: List.map (cases, #2))
                   | _ => []
            in
               case List.peek (succs, not o placed) of
                  NONE => ()
                | SOME l => (List.foreach (succs, fn l =>
                                           List.push (pending, l))
                             ; trace l)
            end
      fun drain () =
         case !pending of
            [] => ()
          | l :: ls => (pending := ls; trace l; drain ())
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
      val entries =
         List.insertionSort
         (Vector.fold (blocks, [], fn (Block.T {kind, label, ...}, es) =>
                       if Kind.isEntry kind
                          then (labelIndex label, label) :: es
                       else es),
          fn ((i1, _), (i2, _)) => i1 <= i2)
      (* Blocks that aren't reachable from an entry (such as the arms of
       * if-converted switches) are not output.
       *)
      val () = List.foreach (entries, fn (_, l) => (trace l; drain ()))
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
      ; outputBlocks (List.rev (!order))
      (* the preheaders (out of line, so that nothing falls through them):
       * load the promoted stack slots
       *)
      ; List.foreach
        (List.rev (!order), fn Block.T {label = l, ...} =>
         let
            val {pheader, promoted, ...} = blockWebs l
         in
            if pheader andalso not localRA
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
      (* the compensation blocks of the local register allocator *)
      ; List.foreach
        (List.rev (!comps), fn (name, l, state) =>
         (label name
          ; regs := state
          ; pinned := []
          ; conform l
          ; branch (blockLabel l)))
      ; List.rev (!code)
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

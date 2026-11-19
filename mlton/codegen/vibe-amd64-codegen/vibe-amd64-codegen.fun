(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* A simple native codegen for x86-64 (AMD64).
 *
 * This codegen mirrors the structure of the C codegen: each chunk is a
 * function
 *    uintptr_t Chunk (CPointer gcState, CPointer stackTop,
 *                     CPointer frontier, uintptr_t nextBlock)
 * that is driven by the trampoline in c-main.h.  Only the chunks are emitted
 * as assembly; the main file is emitted (as C) by the C codegen.
 *
 * Register conventions within a chunk:
 *    r13 = GCState, rbp = StackTop, r12 = Frontier
 *    rsi, rdi, r8-r11, rbx, r14, r15, xmm2-xmm15: temporaries (see
 *       "Register allocation", below)
 *    rax, rcx, rdx, xmm0, xmm1: scratch: the operands and results of the
 *       current statement, address computations, and the instructions
 *       that need them (shifts, multiplies, divisions, compare-and-swap)
 * Integer values narrower than 64 bits are kept zero-extended in registers.
 *
 * The frame.  rsp doesn't move after the prologue of a chunk function, and
 * the frame is the same for every chunk: its size is fixed for the whole
 * program before any chunk is output.  So, all chunks run in the same
 * frame, and a transfer to an entry block of another chunk is a jump to it.
 * See VibeAmd64Access for what is in the frame.
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
 * The regions, the webs of temporaries and their registers are computed for
 * a whole chunk before any of its code is output: that is VibeAmd64Analysis
 * (see vibe-amd64-analysis.sig).
 *
 * Output.  The codegen builds a list of instructions for each chunk (see
 * vibe-amd64-asm.sig); printing them as assembly is the business of
 * VibeAmd64Asm.
 *
 * The parts: VibeAmd64Emit collects the instructions; VibeAmd64Access reads
 * and writes the operands of the Machine IR; VibeAmd64Prim outputs the
 * primitives; VibeAmd64Items combines statements; VibeAmd64Layout orders
 * the blocks.  What is here is the frame of a chunk function, the
 * transfers, and the loop over the blocks.
 *)

functor VibeAmd64Codegen (S: VIBE_AMD64_CODEGEN_STRUCTS): VIBE_AMD64_CODEGEN =
struct

open S
open Machine

structure Asm = VibeAmd64Asm ()
structure Addr = Asm.Addr
structure Cond = Asm.Cond
structure I = Asm.Instr
structure Lbl = Asm.Label
structure Opnd = Asm.Operand
structure Size = Asm.Size
structure Tgt = Asm.Target

structure Analysis = VibeAmd64Analysis (structure Machine = Machine)
structure Web = Analysis.Web
structure Items = VibeAmd64Items (structure Analysis = Analysis)
structure Layout = VibeAmd64Layout (structure Analysis = Analysis)
structure Emit = VibeAmd64Emit (structure Asm = Asm
                                structure Machine = Machine)
structure Access = VibeAmd64Access (structure Emit = Emit
                                    structure Items = Items)
structure PrimApp = VibeAmd64Prim (structure Access = Access)
val switchCases = Analysis.switchCases
val isReal = Emit.isReal
val size = Emit.size
val sz = Emit.sz
val based = Emit.based
val simm32 = Emit.simm32
val rax = Emit.rax
val rcx = Emit.rcx
val rdx = Emit.rdx
val rsp = Emit.rsp
val r10 = Emit.r10
val r11 = Emit.r11
val xmm0 = Emit.xmm0
val cty = Access.cty

val R = Opnd.Reg
fun Q r = Opnd.Reg (r, Size.Q)
fun imm (n: int) = Opnd.Imm (IntInf.fromInt n)

structure GCField = Runtime.GCField

fun implementsPrim (p: 'a Prim.t): bool =
   let
      datatype z = datatype Prim.t
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
       (* The baseline of x86-64 has no fused multiply-add, and can't round
        * to an integral value.
        *)
       | Real_muladd _ => false
       | Real_mulsub _ => false
       | Real_round _ => false
       | Word_rndToReal _ => true
       | _ => CCodegen.implementsPrim p
   end

(* The runtime's functions on reals that only call a function of the C
 * math library with the same arguments: call that function itself.
 *)
fun libm (name: string): string =
   let
      fun try (prefix, suffix) =
         if String.hasPrefix (name, {prefix = prefix})
            then
               let
                  val f = String.dropPrefix (name, String.size prefix)
                  val f' =
                     case f of
                        "realCeil" => SOME "ceil"
                      | "realFloor" => SOME "floor"
                      | "realTrunc" => SOME "trunc"
                      | "round" => SOME "rint"
                      | "Math_ln" => SOME "log"
                      | _ =>
                           if List.contains
                              (["acos", "asin", "atan", "atan2", "cos", "cosh",
                                "exp", "log10", "pow", "sin", "sinh", "tan",
                                "tanh"],
                               String.dropPrefix (f, Int.min (5, String.size f)),
                               op =)
                              andalso String.hasPrefix (f, {prefix = "Math_"})
                              then SOME (String.dropPrefix (f, 5))
                           else NONE
               in
                  Option.map (f', fn f => f ^ suffix)
               end
         else NONE
   in
      case try ("Real64_", "") of
         SOME f => f
       | NONE => (case try ("Real32_", "f") of
                     SOME f => f
                   | NONE => name)
   end

(* Each entry block has a global symbol.  Their addresses are collected in a
 * table that is indexed by frame index (that is, by "return address").
 *)
fun entryLabel l = Lbl.Sym ("vibeAmd64Entry_" ^ Label.toString l)
val entryTable = Lbl.Sym "vibeAmd64Entries"

(* The C calling convention: where each argument goes. *)
datatype loc =
   InReg of int
 | OnStack of int   (* its offset from rsp *)
val intArgRegs = [7, 6, 2, 1, 8, 9]
(* The locations, and the size of the stack arguments. *)
fun argLocs (ctys: CType.t list): loc list * int =
   let
      val (locs, _, _, stk) =
         List.fold
         (ctys, ([], intArgRegs, 0, 0), fn (ct, (locs, ints, nf, stk)) =>
          if isReal ct
             then if nf < 8
                     then (InReg (16 + nf) :: locs, ints, nf + 1, stk)
                  else (OnStack stk :: locs, ints, nf, stk + 8)
          else
             case ints of
                r :: ints => (InReg r :: locs, ints, nf, stk)
              | [] => (OnStack stk :: locs, ints, nf, stk + 8))
   in
      (List.rev locs, stk)
   end

fun outputChunk (Chunk.T {blocks, chunkLabel, tempsMax},
                 {labelBlock: Label.t -> Block.t,
                  labelChunk: Label.t -> ChunkLabel.t,
                  labelIndex: Label.t -> int},
                 {frame: Access.frame, frameSize: int}): I.t list =
   let
      val int = CCodegen.C.int
      val self = ChunkLabel.toString chunkLabel
      val e as {emit, ins, label, jmp, ld, st, mov, movImm, lea, flagsLive,
                instrs, ...} = Emit.new {name = self}
      val leave = Lbl.Local ("leave_" ^ self)
      (* The label of a block, and of the preheader that loads its promoted
       * stack slots.  By default they are local to the assembler; with
       * -vibe-amd64-symbols true, each is a symbol, so that tools (a
       * profiler, a debugger) can name the block that an address is in.
       *)
      val symbols = !Control.vibeAmd64Symbols
      fun blockLabel l =
         if symbols then Lbl.Sym (Label.toString l)
         else Lbl.Local (Label.toString l)
      fun preLabel l =
         if symbols then Lbl.Sym ("pre_" ^ Label.toString l)
         else Lbl.Local ("pre_" ^ Label.toString l)

      (* The analysis of the chunk (see vibe-amd64-analysis.sig): speculation,
       * regions, loops, and the registers of the webs of temporaries and of
       * promoted stack slots.  The blocks that it returns are the ones to
       * output.
       *)
      val analysis as {blocks, labelBlock, blockWebs, ...} =
         Analysis.analyze {blocks = blocks, labelBlock = labelBlock,
                           tempsMax = tempsMax}
      val access as {startBlock, curBlock, enterBlock, defined, home, reads,
                     needsScratch, opnd, addr, regOf, load, dstReg, store,
                     move, setDst, clearCache, noCache, resInt, resReal,
                     shuffle, saveForCall, afterCall, ...} =
         Access.new {analysis = analysis, emit = e, frame = frame,
                     labelIndex = labelIndex, tempsMax = tempsMax}
      val prim = {access = access, emit = e}
      val webHome = Web.home
      (* The label to jump to to get to l: the preheader of a loop with
       * promoted slots, if coming from outside the loop.
       *)
      (* Jump threading: a jump to a block that does nothing but jump on
       * goes where that one goes.
       *)
      fun thread l =
         let
            fun loop (l, fuel) =
               case labelBlock l of
                  Block.T {kind = Kind.Jump, statements,
                           transfer = Transfer.Goto l', ...} =>
                     let
                        val {ploop, pheader, ...} = blockWebs l'
                     in
                        if fuel > 0
                           andalso Vector.isEmpty statements
                           andalso not (Label.equals (l, l'))
                           (* what is in registers on the way to l' must be
                            * what l' expects
                            *)
                           andalso #ploop (blockWebs l) = ploop
                           andalso (ploop = 0
                                    orelse ploop = #ploop (blockWebs (curBlock ()))
                                    orelse pheader)
                           then loop (l', fuel - 1)
                        else l
                     end
                | _ => l
         in
            loop (l, 8)
         end
      fun target l =
         let
            val l = thread l
            val {ploop, pheader, ...} = blockWebs l
         in
            if ploop <> 0
               andalso ploop <> #ploop (blockWebs (curBlock ()))
               then if pheader
                       then preLabel l
                    else Error.bug "VibeAmd64Codegen.target: no preheader"
            else blockLabel l
         end

      (* the callee-save registers, in the order that they are pushed *)
      val saved = [5, 3, 12, 13, 14, 15]

      fun gcField f = Operand.gcField f
      fun gcAddr f = addr (gcField f, [])
      fun flushFrontier () =
         move {dst = gcField GCField.Frontier, src = Operand.Frontier}
      fun flushStackTop () =
         move {dst = gcField GCField.StackTop, src = Operand.StackTop}
      val jumpTables = ref 0
      (* the block that is laid out after the one being output *)
      val next: Label.t option ref = ref NONE
      (* Is l (which has been threaded) reached by falling out of the block
       * being output?  It is if it is next, or if what is next does nothing
       * but jump on to it.
       *)
      fun isNext l =
         case !next of
            SOME l' => Label.equals (l, l') orelse Label.equals (l, thread l')
          | NONE => false
      fun goto l =
         let
            val l = thread l
         in
            if isNext l andalso Lbl.equals (target l, blockLabel l)
               then ()
            else jmp (target l)
         end
      fun bcond (c, l: Lbl.t) = emit (I.Jcc (c, Tgt.Label l))
      (* if c then goto l else goto l' *)
      fun twoWay (c, l, l') =
         if isNext (thread l)
            then (bcond (Cond.negate c, target l'); goto l)
         else (bcond (c, target l); goto l')
      (* return rax to the trampoline *)
      fun leaveChunk () = (flushFrontier (); flushStackTop (); jmp leave)
      (* Transfers between chunks don't go through the trampoline: a
       * transfer to a known entry block of another chunk jumps to it.  This
       * relies on every chunk running in the same frame, with GCState,
       * StackTop and Frontier in the same registers.
       *)
      fun jump l =
         if ChunkLabel.equals (labelChunk l, chunkLabel)
            then goto l
         else emit (I.Jmp (Tgt.Label (entryLabel l)))
      (* Push a frame.  Uses only r11, which is free here: no temporary is
       * live across a transfer that pushes a frame, other than those that
       * a C call saves, and the arguments of a C call are in place.
       *)
      fun push (return, size) =
         let
            val size = Bytes.toInt size
         in
            clearCache ()
            ; ins ("movq", [imm (labelIndex return),
                            Opnd.Mem (based (Emit.stackTop, size - 8))])
            ; ins ("addq", [imm size, Q Emit.stackTop])
            ; (case labelBlock return of
                  Block.T {kind, ...} =>
                     (case Option.map (Kind.frameInfoOpt kind,
                                       FrameInfo.sporkInfo) of
                         SOME (SOME _) =>
                            (* push StackTop on the promotion stack *)
                            let
                               val pst = gcAddr GCField.PromoStackTop
                            in
                               ld (CType.CPointer, r11, pst)
                               ; st (CType.CPointer, Emit.stackTop,
                                     based (r11, 0))
                               ; ins ("addq", [imm 8, Opnd.Mem pst])
                            end
                       | _ => ()))
         end
      (* Pop a frame.  Uses only rcx: the result of a C call may be in rax
       * (see directReturn).
       *)
      fun pop (fi, {promo}) =
         (clearCache ()
          ; ins ("subq", [imm (Bytes.toInt (FrameInfo.size fi)),
                          Q Emit.stackTop])
          ; if promo andalso Option.isSome (FrameInfo.sporkInfo fi)
               then
                  let
                     val pst = gcAddr GCField.PromoStackTop
                     val psb = gcAddr GCField.PromoStackBot
                  in
                     ld (CType.CPointer, rcx, pst)
                     ; ins ("subq", [imm 8, Q rcx])
                     ; st (CType.CPointer, rcx, pst)
                     ; ins ("cmpq", [Q rcx, Opnd.Mem psb])
                     ; emit (I.Jcc (Cond.BE, Tgt.Forward 1))
                     ; st (CType.CPointer, rcx, psb)
                     ; emit (I.LocalLabel 1)
                  end
            else ())
      (* transfer to the entry block whose index is on top of the stack *)
      fun indJump () =
         (ld (CType.CPointer, rax, based (Emit.stackTop, ~8))
          ; lea (Addr.Rip (entryTable, 0), rcx)
          ; emit (I.JmpInd (Opnd.Mem (Addr.T {base = SOME rcx,
                                               index = SOME (rax, 8),
                                               disp = 0}))))
      fun rtrans rsTo =
         case rsTo of
            l :: rsTo' =>
               if List.forall (rsTo', fn l' => Label.equals (l, l'))
                  then jump l
               else indJump ()
          | [] => indJump ()
      (* Where a C call leaves its result for the block that it returns to:
       * in rax/xmm0 if directReturn, else in the frame.
       *
       * A CReturn block is reached only as the return of a C call to its
       * function (the Machine IR allows Goto and Switch only to Jump
       * blocks): directly from the call, or, if the function may switch
       * threads, through the entry table.  When neither the call nor the
       * block goes through the entry table, every predecessor arrives with
       * the result still in rax/xmm0.  Nothing that is output between the
       * call and the block's use of the result touches rax or xmm0:
       *  - after the call: the reloads of the caller-save registers, of
       *    Frontier and of StackTop;
       *  - the preheader of the block, if it heads a loop with promoted
       *    slots: loads into allocated registers;
       *  - pop, in the header: rcx.
       * That a call may collect doesn't matter: it still returns to its
       * call site.
       *)
      fun directReturn (l: Label.t): bool =
         case labelBlock l of
            Block.T {kind as Kind.CReturn {func, ...}, ...} =>
               not (CFunction.maySwitchThreadsFrom func)
               andalso not (Kind.isEntry kind)
          | _ => false
      fun ccall (args, func as CFunction.T {prototype = (ctys, retTy),
                                             target, ...}, return) =
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
            (* The registers of the arguments are those of temporaries and
             * of cached stack slots: from here on, the cache may be read,
             * but nothing more is cached.
             *)
            val () = noCache ()
            val ctys = Vector.toList ctys
            val (locs, _) = argLocs ctys
            (* The arguments: where the value is (the operand, or a slot of
             * the frame), its type in the prototype, and where it goes.
             * The function pointer of an indirect call goes in r10.
             *)
            datatype src =
               Z of Operand.t
             | Slot of CType.t * Addr.t
            val moves =
               List.map3 (args, ctys, locs, fn (z, ct, loc) => (z, ct, loc))
               @ (case fptr of
                     NONE => []
                   | SOME z => [(z, CType.CPointer, InReg r10)])
            (* the type that the value of an argument is kept at in a slot *)
            fun slotType z =
               if isReal (cty z) then cty z else CType.Word64
            (* Uses only rax and rcx, and rcx only if the operand needs a
             * scratch register.
             *)
            fun toSlot (z, k) =
               let
                  val ct = slotType z
                  val a = shuffle k
               in
                  (case (home z, isReal ct) of
                      (SOME r, _) => st (ct, r, a)
                    | (NONE, false) => (load (z, rax, [rax, rcx])
                                        ; st (ct, rax, a))
                    | (NONE, true) =>
                         (* copy its bits *)
                         let
                            val wct = if size ct = 4 then CType.Word32
                                      else CType.Word64
                         in
                            ld (wct, rax, addr (z, [rax, rcx]))
                            ; st (wct, rax, a)
                         end)
                  ; Slot (ct, a)
               end
            (* An operand that needs scratch registers is evaluated now,
             * while they are all free.
             *)
            val moves =
               List.mapi (moves, fn (k, (z, ct, loc)) =>
                          (if needsScratch z then toSlot (z, k) else Z z,
                           ct, loc, k))
            (* Put an argument in a register.  Narrow signed integers are
             * sign-extended.
             *)
            fun toReg (src, ct, d) =
               ((case src of
                    Z z => load (z, d, [])
                  | Slot (ct', a) => ld (ct', d, a))
                ; (case ct of
                      CType.Int8 =>
                         ins ("movsbl", [R (d, Size.B), R (d, Size.L)])
                    | CType.Int16 =>
                         ins ("movswl", [R (d, Size.W), R (d, Size.L)])
                    | _ => ()))
            (* first, the stack arguments *)
            val () =
               List.foreach
               (moves, fn (src, ct, loc, _) =>
                case loc of
                   InReg _ => ()
                 | OnStack off =>
                      if isReal ct
                         then (toReg (src, ct, xmm0)
                               ; st (ct, xmm0, based (rsp, off)))
                      else (toReg (src, ct, rax)
                            ; st (CType.Word64, rax, based (rsp, off))))
            (* Then, the register arguments, in an order such that no
             * register is written before it is last read.  When there is
             * no such order, the value of one argument goes through a slot
             * of the frame.
             *)
            fun readsOf src =
               case src of
                  Z z => reads z
                | Slot _ => []
            fun loop pending =
               let
                  (* does some other pending move read register d? *)
                  fun isRead (d, k) =
                     List.exists (pending, fn (src', _, _, k') =>
                                  k <> k'
                                  andalso List.contains (readsOf src', d,
                                                         op =))
                  (* does the move read the destination of another? *)
                  fun isReader (src, k) =
                     List.exists (pending, fn (_, _, d', k') =>
                                  k <> k'
                                  andalso List.contains (readsOf src, d',
                                                         op =))
               in
                  case pending of
                     [] => ()
                   | _ =>
                        (case List.peek (pending, fn (_, _, d, k) =>
                                         not (isRead (d, k))) of
                            SOME (src, ct, d, k) =>
                               (toReg (src, ct, d)
                                ; loop (List.keepAll
                                        (pending, fn (_, _, _, k') =>
                                         k <> k')))
                          | NONE =>
                               (case List.peek (pending, fn (src, _, _, k) =>
                                                isReader (src, k)) of
                                   SOME (Z z, _, _, k) =>
                                      loop (List.map
                                            (pending,
                                             fn m as (_, ct, d, k') =>
                                             if k = k'
                                                then (toSlot (z, k), ct, d, k)
                                             else m))
                                 | _ =>
                                      Error.bug
                                      "VibeAmd64Codegen.ccall: cycle"))
               end
            val () =
               loop (List.keepAllMap (moves, fn (src, ct, loc, k) =>
                                      case loc of
                                         InReg d => SOME (src, ct, d, k)
                                       | OnStack _ => NONE))
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
                     (I.Call
                      (Lbl.Sym
                       (case name of
                           (* static inline wrappers in c-chunk.h *)
                           "GC_readBarrier" => "Assignable_readBarrier"
                         | "GC_writeBarrier" => "Assignable_writeBarrier"
                         | _ => libm name)))
                | Indirect => emit (I.CallInd (Q r10))
            val () = afterCall ()
            val () =
               case (retTy, return) of
                  (NONE, _) => ()
                | (SOME ct, SOME {return, ...}) =>
                     if directReturn return
                        then ()
                     else if isReal ct
                             then st (ct, xmm0, resReal)
                          else st (CType.Word64, rax, resInt)
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
                     NONE => (movImm (rax, ~2); jmp leave)
                   | SOME {return, ...} => goto return)
         end
      fun outputTransfer t =
         case t of
            Transfer.CCall {func = CFunction.T {target = CFunction.Target.Direct
                                                         "Thread_returnToC", ...},
                            return = SOME {return, size = SOME size}, ...} =>
               (push (return, size)
                ; movImm (rax, ~1)
                ; leaveChunk ())
          | Transfer.CCall {args, func, return} => ccall (args, func, return)
          | Transfer.Call {label, return, ...} =>
               (Option.app (return, fn {return, size, ...} => push (return, size))
                ; jump label)
          | Transfer.Goto l => goto l
          | Transfer.Raise {raisesTo} =>
               (clearCache ()
                ; ld (CType.CPointer, Emit.stackTop, gcAddr GCField.StackBottom)
                ; ins ("addq", [Opnd.Mem (gcAddr GCField.ExnStack),
                                Q Emit.stackTop])
                ; rtrans raisesTo)
          | Transfer.Return {returnsTo} => rtrans returnsTo
          | Transfer.Spork {cont, ...} => goto cont
          | Transfer.Switch (Switch.T {cases, default, test, ...}) =>
               let
                  val (cases, default) = switchCases (cases, default)
                  val ct = cty test
                  val s = sz ct
                  val sfx = Size.suffix s
                  (* Compare the test, which is the operand zt, with w;
                   * equality is the condition.
                   *)
                  fun eq (zt, w) =
                     let
                        val n = WordX.toIntInfX w
                     in
                        (if size ct < 8 orelse simm32 n
                            then (case (zt, n) of
                                     (Opnd.Reg _, 0) =>
                                        ins ("test" ^ sfx, [zt, zt])
                                   | _ => ins ("cmp" ^ sfx, [Opnd.Imm n, zt]))
                         else (movImm (rdx, WordX.toIntInf w)
                               ; ins ("cmp" ^ sfx, [R (rdx, s), zt])))
                        ; Cond.E
                     end
                  fun testOpnd () = opnd (test, [rax, rcx], {imm = false})
               in
                  case cases of
                     [(w, l)] =>
                        twoWay (eq (testOpnd (), w), l, default)
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
                                       Lbl.Local
                                       (concat ["jump_", self, "_",
                                                int (!jumpTables)])
                                 in
                                    load (test, rax, [rax, rcx])
                                    ; if lo = 0 then ()
                                      else if simm32 lo
                                         then ins ("subq", [Opnd.Imm lo, Q rax])
                                      else (movImm (rdx, lo)
                                            ; ins ("subq", [Q rdx, Q rax]))
                                    ; ins ("cmpq", [imm span, Q rax])
                                    ; bcond (Cond.A, target default)
                                    ; lea (Addr.Rip (table, 0), rcx)
                                    ; ins ("movslq",
                                           [Opnd.Mem
                                            (Addr.T {base = SOME rcx,
                                                     index = SOME (rax, 4),
                                                     disp = 0}),
                                            Q rax])
                                    ; ins ("addq", [Q rcx, Q rax])
                                    ; emit (I.JmpInd (Q rax))
                                    ; emit (I.P2align 2)
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
                           else
                              let
                                 val zt = testOpnd ()
                              in
                                 List.foreach (cases, fn (w, l) =>
                                               bcond (eq (zt, w), target l))
                                 ; goto default
                              end
                        end
               end
      (* the statements of a block, as they are output *)
      datatype item = datatype Items.t
      fun outputItem it =
         case it of
            App z => PrimApp.app (prim, z)
          | Defined _ => ()
          | Move z => move z
          | Sels (a, ss) =>
               if PrimApp.selReal (prim, a, ss) then ()
               else
               let
                  val c = PrimApp.appFlags (prim, a)
                  val () = defined (#dst a)
                  val () = if isSome c then flagsLive := true else ()
               in
                  List.foreach (ss, fn s =>
                                (PrimApp.sel (prim, s, c); defined (#dst s)))
                  ; flagsLive := false
               end
      (* an item has been output: so have its definitions *)
      fun afterItem it =
         (case it of
             App {dst, also, ...} =>
                (defined dst; Option.app (also, fn (z, _) => defined z))
           | Defined z => defined z
           | Move {dst, ...} => defined dst
           | Sels _ => ())
      fun outputItems its =
         List.foreach (its, fn it => (outputItem it; afterItem it))
      fun outputBlock (b as Block.T {kind, label = l, transfer, ...}) =
         (if Kind.isEntry kind
             then let
                     val entry = entryLabel l
                  in
                     emit (I.Globl entry)
                     ; emit (I.Hidden entry)
                     ; label entry
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
                          val r = if isReal ct then xmm0 else rax
                       in
                          if directReturn l then ()
                          else if isReal ct then ld (ct, r, resReal)
                          else ld (CType.Word64, r, resInt)
                          (* The bits of the register above the size of
                           * the result may be anything.
                           *)
                          ; store (dst, r, {clean = false})
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
                            | SOME c => twoWay (c, l, l'))
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
      val chunk = Lbl.Sym self
   in
      emit (I.Section Asm.Section.Text)
      ; emit (I.P2align 4)
      ; emit (I.Globl chunk)
      ; emit (I.Hidden chunk)
      ; label chunk
      ; List.foreach (saved, fn r => ins ("pushq", [Q r]))
      ; ins ("subq", [imm frameSize, Q rsp])
      ; ins ("movq", [Q 7, Q Emit.gcState])
      ; ins ("movq", [Q 6, Q Emit.stackTop])
      ; ins ("movq", [Q 2, Q Emit.frontier])
      (* jump to the entry block with index rcx *)
      ; lea (Addr.Rip (entryTable, 0), rax)
      ; emit (I.JmpInd (Opnd.Mem (Addr.T {base = SOME rax,
                                           index = SOME (rcx, 8),
                                           disp = 0})))
      (* return rax to the trampoline *)
      ; label leave
      ; ins ("addq", [imm frameSize, Q rsp])
      ; List.foreach (List.rev saved, fn r => ins ("popq", [Q r]))
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
                         | SOME r => ld (ct, r, based (Emit.stackTop, off)))
                     ; jmp (blockLabel l))
            else ()
         end)
      ; instrs ()
   end

(* What ends an assembly file.  On ELF targets, an object file that doesn't
 * say that it doesn't need an executable stack makes the linker give the
 * whole program one.
 *)
fun endFile (print: string -> unit): unit =
   case !Control.Target.os of
      MLton.Platform.OS.Darwin => ()
    | _ => print "\t.section .note.GNU-stack,\"\",@progbits\n"

fun output {program as Program.T {chunks, frameInfos, ...}, outputC,
            outputS: unit -> {file: File.t,
                              print: string -> unit,
                              done: unit -> unit}} =
   let
      (* The frame, which is the same for all chunks: it holds the stack
       * arguments of any C call and the temporaries of any chunk.
       *)
      val frame =
         List.fold
         (chunks, {outArgs = 0, shuffle = 1}, fn (Chunk.T {blocks, ...}, f) =>
          Vector.fold
          (blocks, f, fn (Block.T {transfer, ...}, f as {outArgs, shuffle}) =>
           case transfer of
              Transfer.CCall {args, func = CFunction.T {prototype = (ctys, _),
                                                        ...}, ...} =>
                 {outArgs = Int.max (outArgs,
                                     #2 (argLocs (Vector.toList ctys))),
                  shuffle = Int.max (shuffle, 1 + Vector.length args)}
            | _ => f))
      val need =
         List.fold (chunks, 0, fn (Chunk.T {tempsMax, ...}, n) =>
                    Int.max (n, Access.frameNeed {frame = frame,
                                                  tempsMax = tempsMax}))
      (* rsp is 8 mod 16 on entry to a chunk function, which then pushes 6
       * registers; C calls need it to be 0 mod 16.
       *)
      val frameSize = 16 * ((need + 7) div 16) + 8
      (* the table of entries *)
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
            val code = ref [I.Label entryTable, I.Hidden entryTable,
                            I.Globl entryTable, I.P2align 3,
                            I.Section Asm.Section.RelRo]
            val () =
               Array.foreach
               (entries, fn lo =>
                List.push (code, I.Quad (Option.map (lo, entryLabel))))
         in
            Asm.output (List.rev (!code), print)
            ; endFile print
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
                                               labelIndex = labelIndex},
                                       {frame = frame, frameSize = frameSize}),
                          print))
             ; endFile print
             ; done ()
          end)}
   end

end

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
 *)

functor VibeArmCodegen (S: VIBE_ARM_CODEGEN_STRUCTS): VIBE_ARM_CODEGEN =
struct

open S
open Machine

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

fun outputChunk (Chunk.T {blocks, chunkLabel, tempsMax}, print,
                 {labelBlock: Label.t -> Block.t,
                  labelChunk: Label.t -> ChunkLabel.t,
                  labelIndex: Label.t -> int}) =
   let
      val int = CCodegen.C.int
      val darwin = !Control.Target.os = MLton.Platform.OS.Darwin
      fun sym s = if !Control.labelsHaveExtra_ then "_" ^ s else s
      (* emit an instruction *)
      fun i ss = (print "\t"; List.foreach (ss, print); print "\n")
      fun label s = (print s; print ":\n")
      val self = ChunkLabel.toString chunkLabel
      val switch = "Lswitch_" ^ self
      val leave = "Lleave_" ^ self
      val table = "Ltable_" ^ self

      (* Registers: 0-31 are x0-x30,sp; 32-63 are d0-d31. *)
      fun isReal ct =
         case ct of CType.Real32 => true | CType.Real64 => true | _ => false
      fun size ct = Bytes.toInt (CType.size ct)
      fun sfx ct = case size ct of 1 => "b" | 2 => "h" | _ => ""
      fun x r = if r = 31 then "sp" else "x" ^ int r
      fun reg (r, ct) =
         if r >= 32
            then (if size ct = 4 then "s" else "d") ^ int (r - 32)
         else if size ct = 8 then x r else "w" ^ int r
      fun scratch (ct, k) = if isReal ct then 48 + k else k
      fun mov (d, s, ct) =
         if d = s then ()
         else i [if d >= 32 then "fmov " else "mov ",
                 reg (d, ct), ", ", reg (s, ct)]

      fun movImm (r, n: IntInf.t) =
         let
            val top = IntInf.<< (1, 0w64)
            val n = if n < 0 then n + top else n
            fun chunk k =
               IntInf.toInt (IntInf.andb (IntInf.~>> (n, Word.fromInt (16 * k)),
                                          0xFFFF))
            fun mk (opc, k) =
               i [opc, x r, ", #", int (chunk k), ", lsl #", int (16 * k)]
         in
            if n >= top - 0x10000
               then i ["mov ", x r, ", #-", IntInf.toString (top - n)]
            else (case List.keepAll ([0, 1, 2, 3], fn k => chunk k <> 0) of
                     [] => i ["mov ", x r, ", #0"]
                   | k :: ks => (mk ("movz ", k)
                                 ; List.foreach (ks, fn k => mk ("movk ", k))))
         end
      fun movInt (r, n) = movImm (r, IntInf.fromInt n)
      (* d = s + n; may clobber x17 *)
      fun addImm (d, s, n) =
         if n = 0 then (if d = s then () else i ["mov ", x d, ", ", x s])
         else
            let
               val opc = if n > 0 then "add " else "sub "
               val n = Int.abs n
            in
               if n < 4096
                  then i [opc, x d, ", ", x s, ", #", int n]
               else (movInt (17, n); i [opc, x d, ", ", x s, ", x17"])
            end
      (* an addressing mode for [b + off]; may clobber x17 *)
      fun mem (b, off, ct) =
         if (~256 <= off andalso off < 256)
            orelse (off >= 0 andalso off mod size ct = 0
                    andalso off div size ct < 4096)
            then concat ["[", x b, ", #", int off, "]"]
         else (movInt (17, off); concat ["[", x b, ", x17]"])
      fun symAddr (r, s, {got}) =
         let
            val s = sym s
            val (hi, lo) =
               case (darwin, got) of
                  (true, true) => (s ^ "@GOTPAGE", s ^ "@GOTPAGEOFF")
                | (true, false) => (s ^ "@PAGE", s ^ "@PAGEOFF")
                | (false, true) => (":got:" ^ s, ":got_lo12:" ^ s)
                | (false, false) => (s, ":lo12:" ^ s)
         in
            i ["adrp ", x r, ", ", hi]
            ; if got
                 then i ["ldr ", x r, ", [", x r, ", ", lo, "]"]
              else i ["add ", x r, ", ", x r, ", ", lo]
         end

      (* Frame: [sp, #0] and [sp, #8] hold the int/real result of the last
       * C call; then the temporaries that don't have a home register.
       *)
      val (slots, frameSize) =
         List.fold (CType.all, ([], 16), fn (t, (slots, off)) =>
                    ((t, off) :: slots, off + 8 * (1 + tempsMax t)))
      val frameSize = 16 * ((frameSize + 15) div 16)
      fun slot ct =
         #2 (valOf (List.peek (slots, fn (t, _) => CType.equals (t, ct))))
      (* extra adjustment of sp while passing C arguments on the stack *)
      val spAdj = ref 0

      (* ---------------- Register allocation ---------------- *)
      (* A region's table of temporaries. *)
      datatype entry' = E of entry
      withtype entry = {ct: CType.t, index: int,
                    count: int ref,      (* number of uses and defs *)
                    across: bool ref,    (* live across some C call? *)
                    adj: entry' list ref, (* interferes with *)
                    live: bool ref,
                    home: int option ref}
      val {get = labelInfo: Label.t -> {near: Label.t list ref,
                                        (* is a loop header? *)
                                        loopHeader: bool ref,
                                        (* the nest of loops it is in,
                                         * outermost first
                                         *)
                                        loopPath: int list ref,
                                        placed: bool ref,
                                        region: entry list ref option ref},
           ...} =
         Property.get (Label.plist,
                       Property.initFun (fn _ => {near = ref [],
                                                  loopHeader = ref false,
                                                  loopPath = ref [],
                                                  placed = ref false,
                                                  region = ref NONE}))
      fun entry (table: entry list ref, t): entry =
         let
            val ct = Type.toCType (Temporary.ty t)
            val index = Temporary.index t
         in
            case List.peek (!table, fn {ct = ct', index = index', ...} =>
                            index = index' andalso CType.equals (ct, ct')) of
               SOME e => e
             | NONE =>
                  let
                     val e = {ct = ct, index = index, count = ref 0,
                              across = ref false, adj = ref [],
                              live = ref false, home = ref NONE}
                  in
                     List.push (table, e); e
                  end
         end
      fun foreachTemp (z, f) =
         case z of
            Operand.Address z => foreachTemp (z, f)
          | Operand.Cast (z, _) => foreachTemp (z, f)
          | Operand.Offset {base, ...} => foreachTemp (base, f)
          | Operand.SequenceOffset {base, index, ...} =>
               (foreachTemp (base, f); foreachTemp (index, f))
          | Operand.Temporary t => f t
          | _ => ()
      fun isEntry l =
         case labelBlock l of Block.T {kind, ...} => Kind.isEntry kind
      (* View a switch as a list of cases and a default target. *)
      fun switchCases (cases, default) =
         let
            val cases = Vector.toList cases
         in
            case default of
               SOME l => (cases, l)
             | NONE => let
                          val (cases, (_, l)) = List.splitLast cases
                       in
                          (cases, l)
                       end
         end
      fun nearSuccessors transfer =
         List.keepAll
         (case transfer of
             Transfer.CCall {return = SOME {return, ...}, ...} => [return]
           | Transfer.Goto l => [l]
           | Transfer.Spork {cont, ...} => [cont]
           | Transfer.Switch (Switch.T {cases, default, ...}) =>
                Vector.fold (cases, Option.fold (default, [], op ::),
                             fn ((_, l), ls) => l :: ls)
           | _ => [],
          not o isEntry)
      (* If-conversion.  Recognize a two-way switch where each target either
       * is the join block or is a block of a few moves to temporaries that
       * goes to the join block.  Such a switch is output as conditional
       * selects followed by a jump to the join block: no branch to
       * mispredict.  The moves must be safe to execute unconditionally, so
       * the sources may not be heap loads.
       *)
      fun ifConvert transfer =
         let
            fun okSrc z =
               case z of
                  Operand.Cast (z, _) => okSrc z
                | Operand.Const (Const.Null) => true
                | Operand.Const (Const.Real _) => true
                | Operand.Const (Const.Word _) => true
                | Operand.Global _ => true
                | Operand.StackOffset _ => true
                | Operand.Temporary _ => true
                | _ => false
            fun arm l =
               if isEntry l then NONE
               else
                  case labelBlock l of
                     Block.T {statements, transfer = Transfer.Goto j, ...} =>
                        let
                           val moves =
                              Vector.toListKeepAllMap
                              (statements, fn s =>
                               case s of
                                  Statement.Move (z as {dst = Operand.Temporary _,
                                                        src}) =>
                                     if okSrc src then SOME z else NONE
                                | _ => NONE)
                        in
                           if List.length moves = Vector.length statements
                              then SOME (moves, j)
                           else NONE
                        end
                   | _ => NONE
         in
            case transfer of
               Transfer.Switch (Switch.T {cases, default, expect = NONE, ...}) =>
                  (case switchCases (cases, default) of
                      ([(_, l)], l') =>
                         let
                            fun ok (ms, ms', j) =
                               if List.length ms + List.length ms' <= 4
                                  then SOME {caseLabel = l, caseMoves = ms,
                                             defaultMoves = ms', join = j}
                               else NONE
                            fun triangle (a, other) =
                               case a of
                                  SOME (ms, j) =>
                                     if Label.equals (j, other)
                                        then SOME ms
                                     else NONE
                                | NONE => NONE
                         in
                            case (arm l, arm l') of
                               (SOME (ms, j), SOME (ms', j')) =>
                                  if Label.equals (j, j')
                                     then ok (ms, ms', j)
                                  else (case (triangle (arm l, l'),
                                              triangle (arm l', l)) of
                                           (SOME ms, _) => ok (ms, [], l')
                                         | (_, SOME ms') => ok ([], ms', l)
                                         | _ => NONE)
                             | (a, a') =>
                                  (case (triangle (a, l'), triangle (a', l)) of
                                      (SOME ms, _) => ok (ms, [], l')
                                    | (_, SOME ms') => ok ([], ms', l)
                                    | _ => NONE)
                         end
                    | _ => NONE)
             | _ => NONE
         end
      (* 1. Partition the blocks into regions. *)
      val () =
         Vector.foreach
         (blocks, fn Block.T {label, transfer, ...} =>
          List.foreach
          (nearSuccessors transfer, fn l =>
           (List.push (#near (labelInfo label), l)
            ; List.push (#near (labelInfo l), label))))
      val tables = ref []
      val () =
         Vector.foreach
         (blocks, fn Block.T {label, ...} =>
          let
             val table = ref []
             fun visit l =
                let
                   val {near, region, ...} = labelInfo l
                in
                   case !region of
                      SOME _ => ()
                    | NONE => (region := SOME table
                               ; List.foreach (!near, visit))
                end
          in
             if Option.isSome (! (#region (labelInfo label)))
                then ()
             else (List.push (tables, table); visit label)
          end)
      fun blockRegion l = valOf (! (#region (labelInfo l)))
      (* the region of the block that is being output *)
      val region = ref (ref [])
      fun liveTemps (Block.T {label, live, ...}) =
         Vector.fold (live, [], fn (z, es) =>
                      case z of
                         Live.Temporary t => entry (blockRegion label, t) :: es
                       | _ => es)
      (* Loop info, as in the x86/amd64 codegens: compute the loop forest of
       * the control-flow graph of near transfers.
       *)
      val () =
         let
            val g = DirectedGraph.new ()
            val {get = nodeLabel: unit DirectedGraph.Node.t -> Label.t option,
                 set = setNodeLabel, ...} =
               Property.getSetOnce
               (DirectedGraph.Node.plist, Property.initConst NONE)
            val root = DirectedGraph.newNode g
            val {get = labelNode: Label.t -> unit DirectedGraph.Node.t, ...} =
               Property.get
               (Label.plist,
                Property.initFun (fn l =>
                                  let
                                     val n = DirectedGraph.newNode g
                                  in
                                     setNodeLabel (n, SOME l); n
                                  end))
            fun addEdge e = ignore (DirectedGraph.addEdge (g, e))
            val () =
               Vector.foreach
               (blocks, fn Block.T {kind, label, transfer, ...} =>
                (if Kind.isEntry kind
                    then addEdge {from = root, to = labelNode label}
                 else ()
                 ; List.foreach (nearSuccessors transfer, fn l =>
                                 addEdge {from = labelNode label,
                                          to = labelNode l})))
            fun walk (forest, headers, path) =
               let
                  val {loops, notInLoop} = DirectedGraph.LoopForest.dest forest
               in
                  Vector.foreach
                  (notInLoop, fn l =>
                   Option.app
                   (l, fn l =>
                    let
                       val {loopHeader, loopPath, ...} = labelInfo l
                    in
                       loopPath := List.rev path
                       ; loopHeader := Vector.exists
                                       (headers, fn l' =>
                                        case l' of
                                           SOME l' => Label.equals (l, l')
                                         | NONE => false)
                    end))
                  ; Vector.foreachi (loops, fn (k, {headers, child}) =>
                                     walk (child, headers, k :: path))
               end
         in
            walk (DirectedGraph.loopForestSteensgaard
                  (g, {root = root, nodeValue = nodeLabel}),
                  Vector.new0 (), [])
         end
      (* How much deeper in the loop nest is l' than l?  NONE if either isn't
       * in a loop, or they are in unrelated loops.
       *)
      fun loopDistance (l, l') =
         let
            fun check (p, p') =
               case (p, p') of
                  ([], _) => SOME (List.length p')
                | (_, []) => SOME (~ (List.length p))
                | (k :: p, k' :: p') => if k = k' then check (p, p') else NONE
         in
            case (! (#loopPath (labelInfo l)), ! (#loopPath (labelInfo l'))) of
               ([], _) => NONE
             | (_, []) => NONE
             | (p, p') => check (p, p')
         end
      (* 2. Count the uses of each temporary and compute interference. *)
      val () =
         Vector.foreach
         (blocks, fn b as Block.T {kind, label, statements, transfer, ...} =>
          let
             val table = blockRegion label
             (* weigh uses by loop depth *)
             val weight =
                List.fold (! (#loopPath (labelInfo label)), 1, fn (_, w) =>
                           if w < 4096 then 8 * w else w)
             fun use (z, ()) =
                foreachTemp (z, fn t =>
                             let
                                val {count, ...} = entry (table, t)
                             in
                                count := !count + weight
                             end)
             val () =
                if Kind.isEntry kind andalso not (List.isEmpty (liveTemps b))
                   then Error.bug "VibeArmCodegen: temporary live at entry"
                else ()
             val () =
                case kind of
                   Kind.CReturn {dst = SOME z, ...} =>
                      use (Live.toOperand z, ())
                 | _ => ()
             val () =
                Vector.foreach (statements, fn s =>
                                Statement.foldOperands (s, (), use))
             val () = Transfer.foldOperands (transfer, (), use)
             val () =
                case transfer of
                   Transfer.CCall {return = SOME {return, ...}, ...} =>
                      List.foreach (liveTemps (labelBlock return),
                                    fn {across, ...} => across := true)
                 | _ => ()
             (* The temporaries that are live, during a backward scan. *)
             val live: entry list ref = ref []
             fun same (e: entry, e': entry) = #live e = #live e'
             fun add (e: entry) =
                if ! (#live e) then ()
                else (#live e := true; List.push (live, e))
             fun uses z = foreachTemp (z, fn t => add (entry (table, t)))
             (* e is defined; a copy needn't interfere with its source *)
             fun def (e: entry, src: entry option) =
                (List.foreach
                 (!live, fn e' =>
                  if same (e, e')
                     orelse (case src of
                                NONE => false
                              | SOME e'' => same (e', e''))
                     then ()
                  else (List.push (#adj e, E e'); List.push (#adj e', E e)))
                 ; if ! (#live e)
                      then (#live e := false
                            ; live := List.remove (!live, fn e' => same (e, e')))
                   else ())
             val () =
                List.foreach (nearSuccessors transfer, fn l =>
                              List.foreach (liveTemps (labelBlock l), add))
             val () = Transfer.foldOperands (transfer, (), fn (z, ()) => uses z)
             val () =
                Vector.foreachr
                (statements, fn s =>
                 let
                    val (dst, srcs) =
                       case s of
                          Statement.Move {dst, src} => (SOME dst, [src])
                        | Statement.PrimApp {args, dst, ...} =>
                             (dst, Vector.toList args)
                 in
                    (case dst of
                        NONE => ()
                      | SOME (Operand.Temporary t) =>
                           def (entry (table, t),
                                case s of
                                   Statement.Move {src = Operand.Temporary t',
                                                   ...} =>
                                      SOME (entry (table, t'))
                                 | _ => NONE)
                      | SOME z => uses z)
                    ; List.foreach (srcs, uses)
                 end)
             val () =
                case kind of
                   Kind.CReturn {dst = SOME z, ...} =>
                      (case Live.toOperand z of
                          Operand.Temporary t => def (entry (table, t), NONE)
                        | _ => ())
                 | _ => ()
          in
             List.foreach (!live, fn e => #live e := false)
          end)
      val caller = [8, 9, 10, 11, 12, 13, 14, 15]
      val callee = [22, 23, 24, 25, 26, 27, 28]
      val fcaller = List.tabulate (13, fn k => 51 + k)
      val fcallee = List.tabulate (8, fn k => 40 + k)
      (* 3. Color, in order of use count. *)
      fun callerSave r = (8 <= r andalso r <= 15) orelse r >= 51
      val () =
         let
         in
            List.foreach
            (!tables, fn table =>
             List.foreach
             (List.insertionSort (!table, fn (e, e') =>
                                  ! (#count e) >= ! (#count e')),
              fn {ct, across, adj, home, ...} =>
              let
                 val used = List.keepAllMap (!adj, fn E e => ! (#home e))
                 val regs =
                    case (isReal ct, !across) of
                       (false, false) => caller @ callee
                     | (false, true) => callee @ caller
                     | (true, false) => fcaller @ fcallee
                     | (true, true) => fcallee @ fcaller
              in
                 home := List.peek (regs, fn r =>
                                    not (List.contains (used, r, op =)))
              end))
         end
      fun tempHome t = ! (#home (entry (!region, t)))
      fun callerSaves (es: entry list) =
         List.keepAll (es, fn {home, ...} =>
                       case !home of
                          SOME r => callerSave r
                        | NONE => false)
      (* store/load registers to/from their temporaries' slots in the frame *)
      fun commit (opc, es: entry list) =
         List.foreach
         (es, fn {ct, index, home, ...} =>
          let
             val ct' = if isReal ct then CType.Real64 else CType.Word64
          in
             i [opc, " ", reg (valOf (!home), ct'), ", ",
                mem (31, slot ct + 8 * index, ct')]
          end)
      (* ----------------------------------------------------- *)

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
      fun clearCache () = cache := []
      fun slotKey (StackOffset.T {offset, ty, ...}) =
         (Bytes.toInt offset, Type.toCType ty)
      fun sameSlot ((off, ct), (off', ct')) =
         off = off' andalso size ct = size ct' andalso isReal ct = isReal ct'
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
      (* load a slot into the cache, if it is worth it *)
      fun cacheIn z =
         case z of
            Operand.Cast (z, _) => cacheIn z
          | Operand.StackOffset so =>
               let
                  val k as (off, ct) = slotKey so
               in
                  case cacheReg k of
                     NONE => NONE
                   | SOME r =>
                        (i ["ldr", sfx ct, " ", reg (r, ct), ", ",
                            mem (20, off, ct)]
                         ; List.push (cache, {off = off, ct = ct, reg = r})
                         ; SOME r)
               end
          | _ => NONE
      fun home z =
         case z of
            Operand.Cast (z, _) => home z
          | Operand.Frontier => SOME 21
          | Operand.GCState => SOME 19
          | Operand.StackOffset so => Option.map (cached so, #reg)
          | Operand.StackTop => SOME 20
          | Operand.Temporary t => tempHome t
          | _ => NONE
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
                  i ["add ", x t, ", ", x b, ", ", x ix, ", lsl #", int lg]
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
                                 ; i ["fmov ", reg (d, ct), ", ", reg (17, ct)]))
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
                                let
                                   val (b, off) = addr (z, t)
                                in
                                   i ["ldr", sfx ct, " ", reg (d, ct), ", ",
                                      mem (b, off, ct)]
                                end))
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
                  1 => i ["and w", int h, ", w", int r, ", #0xff"]
                | 2 => i ["and w", int h, ", w", int r, ", #0xffff"]
                | _ => mov (h, r, ct)
            fun slot z =
               case z of
                  Operand.Cast (z, _) => slot z
                | Operand.StackOffset so => SOME so
                | _ => NONE
         in
            case (slot dst, home dst) of
               (SOME so, _) =>
                  let
                     val k as (off, _) = slotKey so
                     val () =
                        i ["str", sfx ct, " ", reg (r, ct), ", ",
                           mem (20, off, ct)]
                     (* drop other views of the bytes that were written *)
                     val () =
                        cache :=
                        List.keepAll
                        (!cache, fn {off = off', ct = ct', ...} =>
                         sameSlot (k, (off', ct'))
                         orelse off' + size ct' <= off
                         orelse off + size ct <= off')
                  in
                     case cached so of
                        SOME {reg, ...} => set reg
                      | NONE =>
                           (case cacheReg k of
                               NONE => ()
                             | SOME h =>
                                  (set h
                                   ; List.push (cache, {off = off, ct = ct,
                                                        reg = h})))
                  end
             | (NONE, SOME h) => set h
             | (NONE, NONE) =>
                  let
                     val (b, off) = addr (dst, 3)
                  in
                     i ["str", sfx ct, " ", reg (r, ct), ", ", mem (b, off, ct)]
                  end
         end
      (* A register in which to compute the value of dst. *)
      fun dstReg dst =
         case home dst of
            SOME h => if size (cty dst) < 4 then scratch (cty dst, 0) else h
          | NONE => scratch (cty dst, 0)
      fun move {dst, src} = store (dst, value (src, dstReg dst, 3))

      (* When fusing, a primitive that computes a condition leaves it in the
       * flags (and reports it in fused) rather than computing dst.
       *)
      val fusing = ref false
      val fused: string option ref = ref NONE
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
                        then "#" ^ IntInf.toString (WordX.toIntInf w)
                     else reg (v k, ct)
                | _ => reg (v k, ct)
            fun done () = store (dst, d)
            fun un (opc, ct) =
               (i [opc, " ", reg (d, dct), ", ", reg (v 0, ct)]; done ())
            fun bin' (opc, ct, r0, immOk) =
               (i [opc, " ", reg (d, ct), ", ", reg (r0, ct), ", ",
                   opnd (1, ct, immOk)]
                ; done ())
            fun bin (opc, ct, immOk) = bin' (opc, ct, v 0, immOk)
            fun tern (opc, ct) =
               (i [opc, " ", reg (d, ct), ", ", reg (v 0, ct), ", ",
                   reg (v 1, ct), ", ", reg (v 2, ct)]
                ; done ())
            fun cset cond =
               if !fusing
                  then fused := SOME cond
               else (i ["cset ", reg (d, dct), ", ", cond]; done ())
            fun cmp' (opc, ct, r0, cond) =
               (i [opc, " ", reg (r0, ct), ", ", opnd (1, ct, opc = "cmp")]
                ; cset cond)
            fun cmp (opc, ct, cond) = cmp' (opc, ct, v 0, cond)
            fun addsub (isAdd, ct) =
               let
                  fun opc b = if b then "add" else "sub"
                  fun default () = bin (opc isAdd, ct, true)
               in
                  case a 1 of
                     Operand.Const (Const.Word w) =>
                        let
                           val n = WordX.toIntInfX w
                        in
                           if size ct = 8 andalso ~4096 < n andalso n < 0
                              then (i [opc (not isAdd), " ", reg (d, ct), ", ",
                                       reg (v 0, ct), ", #", IntInf.toString (~ n)]
                                    ; done ())
                           else default ()
                        end
                   | _ => default ()
               end
            val ptr = CType.CPointer
            val rct = CType.real
            fun wct ws = CType.word (ws, {signed = false})
            fun bits ws = Bits.toInt (WordSize.bits ws)
            fun bhw n = case n of 8 => "b" | 16 => "h" | _ => "w"
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
                     then (i ["sxt", bhw n, " ",
                              if to64 then x s else "w" ^ int s, ", w", int r]
                           ; s)
                  else r
               end
            fun chk (opc, sCond, uCond, (ws, {signed}), ks) =
               let
                  val n = bits ws
                  val ops =
                     if n = 64 andalso (opc = "add" orelse opc = "sub")
                        then concat [", ", x (v 0), ", ",
                                     opnd (1, wct ws, true)]
                     else concat (List.map (ks, fn k =>
                                            ", " ^ x (ext (k, ws, signed, true))))
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
                     then (i [opc, " x5", ops]
                           ; i ["cmp x5, w5, ", if signed then "s" else "u",
                                "xt", bhw n]
                           ; finish (5, "ne"))
                  else if opc = "mul"
                     then (i [if signed then "smulh" else "umulh", " x5", ops]
                           ; if signed orelse Option.isSome (!also)
                                then i ["mul x6", ops]
                             else ()
                           ; if signed
                                then i ["cmp x5, x6, asr #63"]
                             else i ["cmp x5, #0"]
                           ; finish (6, "ne"))
                  else
                     let
                        val r = case !also of
                                   SOME (z, false) => dstReg z
                                 | _ => 5
                     in
                        i [opc, "s ", x r, ops]
                        ; finish (r, if signed then sCond else uCond)
                     end
               end
            fun shift (opc, ws, r0) = bin' (opc, wct ws, r0, true)
            (* compare-and-swap; setAddr puts the address in x7 *)
            fun cas (ct, kOld, kNew, obj, setAddr) =
               let
                  fun iv k =
                     let
                        val r = v k
                     in
                        if r >= 32
                           then (i ["fmov ", reg (k, ct), ", ", reg (r, ct)]
                                 ; k)
                        else r
                     end
                  val old = iv kOld
                  val new = iv kNew
                  val () = setAddr ()
               in
                  label "1"
                  ; i ["ldaxr", sfx ct, " ", reg (5, ct), ", [x7]"]
                  ; i ["cmp ", reg (5, ct), ", ", reg (old, ct)]
                  ; i ["b.ne 2f"]
                  ; i ["stlxr", sfx ct, " w6, ", reg (new, ct), ", [x7]"]
                  ; i ["cbnz w6, 1b"]
                  ; label "2"
                  ; (case ct of
                        CType.Objptr =>
                           let
                              (* We don't know what is live here, so commit all
                               * of the caller-save temporaries.
                               *)
                              val saves = callerSaves (! (!region))
                           in
                              mov (0, obj, ct)
                              ; i ["mov x1, x5"]
                              ; commit ("str", saves)
                              ; i ["bl ", sym "Assignable_decheckObjptr"]
                              ; clearCache ()
                              ; commit ("ldr", saves)
                              ; mov (d, 0, ct)
                           end
                      | _ => if isReal ct
                                then i ["fmov ", reg (d, ct), ", ", reg (5, ct)]
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
                          i ["add x7, ", x obj, ", ", x ix, ", lsl #", int lg])
                  end
             | CPointer_add => addsub (true, ptr)
             | CPointer_diff => bin ("sub", ptr, false)
             | CPointer_equal => cmp ("cmp", ptr, "eq")
             | CPointer_fromWord => move {dst = dst, src = a 0}
             | CPointer_lt => cmp ("cmp", ptr, "lo")
             | CPointer_sub => addsub (false, ptr)
             | CPointer_toWord => move {dst = dst, src = a 0}
             | Real_Math_sqrt rs => un ("fsqrt", rct rs)
             | Real_abs rs => un ("fabs", rct rs)
             | Real_add rs => bin ("fadd", rct rs, false)
             | Real_castToWord (rs, _) => un ("fmov", rct rs)
             | Real_div rs => bin ("fdiv", rct rs, false)
             | Real_equal rs => cmp ("fcmp", rct rs, "eq")
             | Real_le rs => cmp ("fcmp", rct rs, "ls")
             | Real_lt rs => cmp ("fcmp", rct rs, "mi")
             | Real_mul rs => bin ("fmul", rct rs, false)
             | Real_muladd rs => tern ("fmadd", rct rs)
             | Real_mulsub rs => tern ("fnmsub", rct rs)
             | Real_neg rs => un ("fneg", rct rs)
             | Real_qequal rs =>
                  (i ["fcmp ", reg (v 0, rct rs), ", ", reg (v 1, rct rs)]
                   ; i ["cset w5, eq"]
                   ; i ["csinc ", reg (d, dct), ", w5, wzr, vc"]
                   ; done ())
             | Real_rndToReal (rs, _) => un ("fcvt", rct rs)
             | Real_rndToWord (rs, _, {signed}) =>
                  un (if signed then "fcvtzs" else "fcvtzu", rct rs)
             | Real_round rs => un ("frintx", rct rs)
             | Real_sub rs => bin ("fsub", rct rs, false)
             | Ref_cas (SOME ct) =>
                  let
                     val obj = v 0
                  in
                     cas (ct, 1, 2, obj, fn () => mov (7, obj, ptr))
                  end
             | Word_add ws => addsub (true, wct ws)
             | Word_addCheckP z => chk ("add", "vs", "cs", z, [0, 1])
             | Word_andb ws => bin ("and", wct ws, false)
             | Word_castToReal (ws, _) => un ("fmov", wct ws)
             | Word_equal ws => cmp ("cmp", wct ws, "eq")
             | Word_extdToWord (ws1, ws2, {signed}) =>
                  let
                     val r = v 0
                  in
                     if signed andalso bits ws1 < bits ws2
                        then i ["sxt", bhw (bits ws1), " ", reg (d, wct ws2),
                                ", w", int r]
                     else mov (d, r, wct ws2)
                     ; done ()
                  end
             | Word_lshift ws => shift ("lsl", ws, v 0)
             | Word_lt (ws, {signed}) =>
                  let
                     val r0 = ext (0, ws, signed, false)
                     val r1 = ext (1, ws, signed, false)
                  in
                     i ["cmp ", reg (r0, wct ws), ", ", reg (r1, wct ws)]
                     ; cset (if signed then "lt" else "lo")
                  end
             | Word_mul (ws, _) => bin ("mul", wct ws, false)
             | Word_mulCheckP z => chk ("mul", "", "", z, [0, 1])
             | Word_neg ws => un ("neg", wct ws)
             | Word_negCheckP z => chk ("neg", "vs", "cc", z, [0])
             | Word_notb ws => un ("mvn", wct ws)
             | Word_orb ws => bin ("orr", wct ws, false)
             | Word_quot (ws, {signed}) =>
                  let
                     val r0 = ext (0, ws, signed, false)
                     val r1 = ext (1, ws, signed, false)
                  in
                     i [if signed then "sdiv " else "udiv ", reg (d, wct ws),
                        ", ", reg (r0, wct ws), ", ", reg (r1, wct ws)]
                     ; done ()
                  end
             | Word_rem (ws, {signed}) =>
                  let
                     val ct = wct ws
                     val r0 = ext (0, ws, signed, false)
                     val r1 = ext (1, ws, signed, false)
                  in
                     i [if signed then "sdiv " else "udiv ", reg (5, ct),
                        ", ", reg (r0, ct), ", ", reg (r1, ct)]
                     ; i ["msub ", reg (d, ct), ", ", reg (5, ct), ", ",
                          reg (r1, ct), ", ", reg (r0, ct)]
                     ; done ()
                  end
             | Word_rndToReal (ws, _, {signed}) =>
                  (i [if signed then "scvtf " else "ucvtf ", reg (d, dct), ", ",
                      reg (ext (0, ws, signed, false), wct ws)]
                   ; done ())
             | Word_rol ws =>
                  let
                     val ct = wct ws
                     val r0 = v 0
                  in
                     i ["neg ", reg (5, ct), ", ", reg (v 1, ct)]
                     ; i ["ror ", reg (d, ct), ", ", reg (r0, ct), ", ",
                          reg (5, ct)]
                     ; done ()
                  end
             | Word_ror ws => shift ("ror", ws, v 0)
             | Word_rshift (ws, {signed}) =>
                  shift (if signed then "asr" else "lsr", ws,
                         ext (0, ws, signed, false))
             | Word_sub ws => addsub (false, wct ws)
             | Word_subCheckP z => chk ("sub", "vs", "cc", z, [0, 1])
             | Word_xorb ws => bin ("eor", wct ws, false)
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
      fun goto l = if isNext l then () else i ["b ", Label.toString l]
      (* Conditional branches only reach +-1MB. *)
      val far =
         Vector.fold (blocks, 0, fn (Block.T {statements, ...}, n) =>
                      n + 1 + Vector.length statements) > 8000
      datatype cond =
         CC of string           (* condition code *)
       | Z of string * bool     (* register is (true) or isn't (false) zero *)
      val negate =
         fn CC c =>
               CC (case c of
                      "eq" => "ne" | "ne" => "eq" | "lo" => "hs" | "hs" => "lo"
                    | "lt" => "ge" | "ge" => "lt" | "vs" => "vc" | "vc" => "vs"
                    | "cs" => "cc" | "cc" => "cs" | "mi" => "pl" | "pl" => "mi"
                    | "ls" => "hi" | "hi" => "ls"
                    | _ => Error.bug "VibeArmCodegen.negate")
          | Z (r, b) => Z (r, not b)
      fun bcond (c, l: string) =
         let
            fun br (c, l) =
               case c of
                  CC c => i ["b.", c, " ", l]
                | Z (r, b) => i [if b then "cbz " else "cbnz ", r, ", ", l]
         in
            if far
               then (br (negate c, "1f"); i ["b ", l]; label "1")
            else br (c, l)
         end
      (* if c then goto l else goto l' *)
      fun twoWay (c, l, l') =
         if isNext l
            then bcond (negate c, Label.toString l')
         else (bcond (c, Label.toString l); goto l')
      (* Output an if-converted switch; the flags hold condition c, which
       * selects the case (rather than the default) target.
       *)
      fun selects (c, {caseLabel = _, caseMoves, defaultMoves, join}) =
         let
            (* The selects write temporaries of other blocks, whose registers
             * the cache may be using.
             *)
            val () = clearCache ()
            val () = cacheable := []
            fun select c {dst, src} =
               let
                  val ct = cty dst
                  (* Select the whole register, because it may be shared
                   * with a wider temporary that is live when c is false.
                   *)
                  val wide = if isReal ct then CType.Real64 else CType.Word64
                  val s = value (src, scratch (ct, 1), 3)
                  val (d, spilled) =
                     case home dst of
                        SOME h => (h, false)
                      | NONE => (scratch (ct, 0), true)
               in
                  if spilled then load (dst, d, 3) else ()
                  ; i [if isReal ct then "fcsel " else "csel ",
                       reg (d, wide), ", ", reg (s, wide), ", ",
                       reg (d, wide), ", ", c]
                  ; if spilled then store (dst, d) else ()
               end
            val c' = case negate (CC c) of
                        CC c' => c'
                      | _ => Error.bug "VibeArmCodegen.selects"
         in
            List.foreach (caseMoves, select c)
            ; List.foreach (defaultMoves, select c')
            ; goto join
         end
      (* return x0 to the trampoline *)
      fun leaveChunk () = (flushFrontier (); flushStackTop (); i ["b ", leave])
      (* Transfers between chunks don't go through the trampoline.  Every
       * chunk function has a second entry point, at a fixed offset from the
       * first, that expects to already be running in some chunk's frame:
       * x19-x21 are set, x29 points at the saved registers, and x3 is the
       * index of the entry block.
       *)
      fun enter c = sym (ChunkLabel.toString c ^ "_enter")
      val enterOffset = 4 * (2 + List.length saved + 3)
      val entryLo =
         Vector.fold (blocks, NONE, fn (Block.T {kind, label, ...}, lo) =>
                      if Kind.isEntry kind
                         then SOME (case lo of
                                       NONE => labelIndex label
                                     | SOME lo => Int.min (lo, labelIndex label))
                      else lo)
      (* jump to the entry block (of this chunk) with index x3 *)
      fun dispatch () =
         (addImm (16, 3, ~ (Option.fold (entryLo, 0, #1)))
          ; i ["adr x17, ", table]
          ; i ["ldrsw x16, [x17, x16, lsl #2]"]
          ; i ["add x17, x17, x16"]
          ; i ["br x17"])
      fun jump l =
         if ChunkLabel.equals (labelChunk l, chunkLabel)
            then goto l
         else (movInt (3, labelIndex l); i ["b ", enter (labelChunk l)])
      fun push (return, size) =
         let
            val size = Bytes.toInt size
         in
            (* only x17 is available: C arguments may already be in place *)
            clearCache ()
            ; addImm (20, 20, size)
            ; movInt (17, labelIndex return)
            ; i ["str x17, [x20, #-8]"]
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
                               ; i ["str x20, [x17], #8"]
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
                     load (pst, 0, 3)
                     ; i ["sub x0, x0, #8"]
                     ; store (pst, 0)
                     ; load (psb, 1, 3)
                     ; i ["cmp x1, x0"]
                     ; i ["b.ls 1f"]
                     ; store (psb, 0)
                     ; label "1"
                  end
            else ())
      fun indJump {mustReturnToSelf, mayReturnToSelf = _: bool} =
         (i ["ldr x3, [x20, #-8]"]
          ; if mustReturnToSelf
               then (* `adr` only reaches +-1MB *)
                    if far then i ["b ", switch] else dispatch ()
            else (symAddr (16, "nextChunks", {got = false})
                  ; i ["ldr x16, [x16, x3, lsl #3]"]
                  ; i ["add x16, x16, #", int enterOffset]
                  ; i ["br x16"]))
      fun rtrans rsTo =
         let
            fun isSelf l = ChunkLabel.equals (labelChunk l, chunkLabel)
         in
            case rsTo of
               l :: rsTo' =>
                  if List.forall (rsTo', fn l' => Label.equals (l, l'))
                     then jump l
                  else indJump {mustReturnToSelf = List.forall (rsTo, isSelf),
                                mayReturnToSelf = List.exists (rsTo, isSelf)}
             | [] => indJump {mustReturnToSelf = false, mayReturnToSelf = true}
         end
      fun ccall (args, func as CFunction.T {prototype = (ctys, retTy),
                                             target, ...}, return) =
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
               case return of
                  NONE => []
                | SOME {return, ...} =>
                     callerSaves (liveTemps (labelBlock return))
            val () = commit ("str", saves)
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
                         CType.Int8 => i ["sxtb w", int r, ", w", int r]
                       | CType.Int16 => i ["sxth w", int r, ", w", int r]
                       | _ => ()
                in
                   if loc >= 64
                      then i ["str", sfx ct, " ", reg (r, ct), ", ",
                              mem (31, loc - 64, ct)]
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
                     i ["bl ",
                        sym (case name of
                                (* static inline wrappers in c-chunk.h *)
                                "GC_readBarrier" => "Assignable_readBarrier"
                              | "GC_writeBarrier" => "Assignable_writeBarrier"
                              | _ => name)]
                | Indirect => i ["blr x16"]
            val () = clearCache ()
            val () = addImm (31, 31, stk)
            val () =
               case retTy of
                  NONE => ()
                | SOME ct => if isReal ct
                                then i ["str d0, [sp, #8]"]
                             else i ["str x0, [sp]"]
            val () = commit ("ldr", saves)
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
               then indJump {mustReturnToSelf = false, mayReturnToSelf = true}
            else (case return of
                     NONE => (movInt (0, ~2); i ["b ", leave])
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
                ; i ["add x20, ",
                   x (value (gcField GCField.StackBottom, 0, 3)), ", ",
                   x (value (gcField GCField.ExnStack, 1, 3))]
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
                                 then i ["cmp ", x r, ", #", IntInf.toString n]
                              else (movImm (16, n); i ["cmp ", x r, ", x16"])
                              ; CC "eq")
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
                                              (i ["cmp ", r, ", #0"]
                                               ; if b then "eq" else "ne"),
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
                                       concat ["Ljump_", self, "_",
                                               int (!jumpTables)]
                                 in
                                    if lo < 4096
                                       then addImm (16, r, ~ (IntInf.toInt lo))
                                    else (movImm (17, lo)
                                          ; i ["sub x16, ", x r, ", x17"])
                                    ; if span < 4096
                                         then i ["cmp x16, #", int span]
                                      else (movInt (17, span)
                                            ; i ["cmp x16, x17"])
                                    ; bcond (CC "hi", Label.toString default)
                                    ; i ["adr x17, ", table]
                                    ; i ["ldrsw x16, [x17, x16, lsl #2]"]
                                    ; i ["add x17, x17, x16"]
                                    ; i ["br x17"]
                                    ; label table
                                    ; Int.for
                                      (0, span + 1, fn k =>
                                       i [".long ",
                                          Label.toString
                                          (case List.peek
                                                (cases', fn (m, _) =>
                                                 m = lo + IntInf.fromInt k) of
                                              SOME (_, l) => l
                                            | NONE => default),
                                          " - ", table])
                                 end
                           else (List.foreach (cases, fn (w, l) =>
                                               bcond (eq w, Label.toString l))
                                 ; goto default)
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
      fun outputItem it =
         case it of
            App z => outputApp z
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
                         val e = entry (!region, t)
                         fun dead l =
                            not (List.exists (liveTemps (labelBlock l), fn e' =>
                                              #live e = #live e'))
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
      fun setupCache (Block.T {kind, live, statements, transfer, ...}) =
         let
            val busy = ref []
            fun temp t = Option.app (tempHome t, fn r => List.push (busy, r))
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
               Vector.foreach (live, fn z =>
                               case z of
                                  Live.Temporary t => temp t
                                | _ => ())
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
                           not (List.contains (!busy, r, op =)))
         end
      fun outputBlock (b as Block.T {kind, label = l, statements, transfer,
                                     ...}) =
         (label (Label.toString l)
          ; region := blockRegion l
          ; setupCache b
          ; (case kind of
                Kind.Cont {frameInfo, ...} => pop (frameInfo, {promo = true})
              | Kind.CReturn {dst, frameInfo, ...} =>
                   (Option.app (frameInfo, fn fi => pop (fi, {promo = true}))
                    ; Option.app
                      (dst, fn dst =>
                       let
                          val dst = Live.toOperand dst
                          val ct = cty dst
                          val d = dstReg dst
                       in
                          i ["ldr", sfx ct, " ", reg (d, ct),
                             if isReal ct then ", [sp, #8]" else ", [sp]"]
                          ; store (dst, d)
                       end))
              | Kind.Func _ => ()
              | Kind.Handler {frameInfo, ...} => pop (frameInfo, {promo = false})
              | Kind.Jump => ()
              | Kind.SporkSpwn {frameInfo, ...} =>
                   pop (frameInfo, {promo = false}))
          ; let
               val its = items (Vector.toList statements)
            in
               case fusable (its, transfer) of
                  NONE => (List.foreach (its, outputItem)
                           ; outputTransfer transfer)
                | SOME (its, a, l, l') =>
                     (List.foreach (its, outputItem)
                      ; fused := NONE
                      ; fusing := true
                      ; outputApp a
                      ; fusing := false
                      ; (case !fused of
                            NONE => outputTransfer transfer
                          | SOME c =>
                               (case ifConvert transfer of
                                   NONE => twoWay (CC c, l, l')
                                 | SOME (z as {caseLabel, ...}) =>
                                      selects
                                      (if Label.equals (l, caseLabel)
                                          then c
                                       else (case negate (CC c) of
                                                CC c => c
                                              | _ => Error.bug "VibeArmCodegen"),
                                       z))))
            end)

      (* Block layout: greedily extend a trace with the successor that is
       * most likely to be next.  For a two-way switch, that is the target
       * that the backend expects, else the one that is deeper in the loop
       * nest, else the one that isn't cold, else the default.
       *)
      val order: Block.t list ref = ref []
      val pending: Label.t list ref = ref []
      fun placed l = ! (#placed (labelInfo l))
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
               val () = #placed (labelInfo l) := true
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
         i [opc, reg (r1, CType.Word64), ", ", reg (r2, CType.Word64),
            ", [sp, #", int (16 * (k + 1)), "]"]
      val entries =
         List.insertionSort
         (Vector.fold (blocks, [], fn (Block.T {kind, label, ...}, es) =>
                       if Kind.isEntry kind
                          then (labelIndex label, label) :: es
                       else es),
          fn ((i1, _), (i2, _)) => i1 <= i2)
      val lo = case entries of [] => 0 | (lo, _) :: _ => lo
      (* Blocks that aren't reachable from an entry (such as the arms of
       * if-converted switches) are not output.
       *)
      val () = List.foreach (entries, fn (_, l) => (trace l; drain ()))
   in
      print "\t.text\n\t.p2align 2\n"
      ; i [".globl ", sym self]
      ; i [if darwin then ".private_extern " else ".hidden ", sym self]
      ; label (sym self)
      ; i ["stp x29, x30, [sp, #-160]!"]
      ; i ["mov x29, sp"]
      ; List.foreachi (saved, fn (k, rs) => pair ("stp ", rs, k))
      ; i ["mov x19, x0"]
      ; i ["mov x20, x1"]
      ; i ["mov x21, x2"]
      (* The entry point for transfers from other chunks; it must be
       * enterOffset bytes from the start.
       *)
      ; i [".globl ", enter chunkLabel]
      ; i [if darwin then ".private_extern " else ".hidden ", enter chunkLabel]
      ; label (enter chunkLabel)
      ; addImm (31, 29, ~ frameSize)
      ; label switch
      ; dispatch ()
      ; label table
      ; ignore (List.fold
                (entries, lo, fn ((index, l), next) =>
                 (Int.for (next, index, fn _ => i [".long 0"])
                  ; i [".long ", Label.toString l, " - ", table]
                  ; index + 1)))
      (* return x0 to the trampoline *)
      ; label leave
      ; i ["mov sp, x29"]
      ; List.foreachi (saved, fn (k, rs) => pair ("ldp ", rs, k))
      ; i ["ldp x29, x30, [sp], #160"]
      ; i ["ret"]
      ; outputBlocks (List.rev (!order))
   end

fun output {program, outputC,
            outputS: unit -> {file: File.t,
                              print: string -> unit,
                              done: unit -> unit}} =
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
              outputChunk (chunk, print, {labelBlock = labelBlock,
                                          labelChunk = labelChunk,
                                          labelIndex = labelIndex}))
             ; done ()
          end)}

end

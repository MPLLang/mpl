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

(* Each entry block has a global symbol for its entry stub, which sets up the
 * (native) stack frame of its chunk before falling into the block.  The
 * addresses of the stubs are collected in a table that is indexed by frame
 * index (that is, by "return address").
 *)
fun entryStub l = "vibeArmEntry_" ^ Label.toString l
val entryTable = "vibeArmEntries"

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
      val leave = "Lleave_" ^ self
      (* which register allocator: see "Register allocation", above *)
      val localRA = !Control.vibeArmLocalRegAlloc
      (* The label of a block, and of the preheader that loads its promoted
       * stack slots.  By default they are local to the assembler; with
       * -vibe-arm-symbols true, each is a symbol, so that tools (a
       * profiler, a debugger) can name the block that an address is in.
       *)
      val symbols = !Control.vibeArmSymbols
      fun blockLabel l =
         if symbols then sym (Label.toString l)
         else (if darwin then "L" else ".L") ^ Label.toString l
      fun preLabel l =
         if symbols then sym ("pre_" ^ Label.toString l)
         else "Lpre_" ^ Label.toString l

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
      (* then an area in which to save the caller-save registers at C calls *)
      val saveBase = frameSize
      val frameSize = 16 * ((frameSize + 8 * 21 + 15) div 16)
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
      (* Speculation.  If-conversion (below) wants the arms of a two-way
       * switch to be nothing but moves.  So, when an arm starts with a few
       * cheap computations that have no effect other than to define
       * temporaries (as do both arms of "x mod c": the sign of x picks the
       * formula), hoist them into the block of the switch, ahead of the
       * comparison.  Both arms' computations then always run, which costs
       * less than mispredicting a branch that depends on data.  It is safe
       * when what is hoisted writes only temporaries that are dead in the
       * other arm and at the join, and can't fault (so: no heap loads).
       *)
      val (blocks, labelBlock) =
         let
            datatype z = datatype Prim.t
            val {get = info: Label.t -> {preds: int ref,
                                         new: Block.t option ref}, ...} =
               Property.get (Label.plist,
                             Property.initFun (fn _ => {preds = ref 0,
                                                        new = ref NONE}))
            fun block l = case ! (#new (info l)) of
                             SOME b => b
                           | NONE => labelBlock l
            fun key t = (Type.toCType (Temporary.ty t), Temporary.index t)
            fun temps z =
               let
                  val ts = ref []
               in
                  foreachTemp (z, fn t => List.push (ts, t)); !ts
               end
            fun mem (ts, t) =
               let
                  val (ct, k) = key t
               in
                  List.exists (ts, fn t' =>
                               let
                                  val (ct', k') = key t'
                               in
                                  k = k' andalso CType.equals (ct, ct')
                               end)
               end
            fun disjoint (ts, ts') = not (List.exists (ts, fn t => mem (ts', t)))
            fun liveTs l =
               case block l of
                  Block.T {live, ...} =>
                     Vector.fold (live, [], fn (z, ts) =>
                                  temps (Live.toOperand z) @ ts)
            fun okSrc z =
               case z of
                  Operand.Cast (z, _) => okSrc z
                | Operand.Const (Const.Word _) => true
                | Operand.Global _ => true
                | Operand.StackOffset _ => true
                | Operand.Temporary _ => true
                | _ => false
            fun isMove s =
               case s of
                  Statement.Move {dst = Operand.Temporary _, src} => okSrc src
                | _ => false
            fun pure s =
               isMove s
               orelse
               (case s of
                   Statement.PrimApp {args, dst = SOME (Operand.Temporary _),
                                      prim} =>
                      Vector.forall (args, okSrc)
                      andalso (case prim of
                                  Word_add _ => true
                                | Word_andb _ => true
                                | Word_extdToWord _ => true
                                | Word_lshift _ => true
                                | Word_mul _ => true
                                | Word_neg _ => true
                                | Word_notb _ => true
                                | Word_orb _ => true
                                | Word_quot _ => true
                                | Word_rem _ => true
                                | Word_rshift _ => true
                                | Word_sub _ => true
                                | Word_xorb _ => true
                                | _ => false)
                 | _ => false)
            fun defs ss =
               List.fold (ss, [], fn (s, ts) =>
                          case s of
                             Statement.Move {dst, ...} => temps dst @ ts
                           | Statement.PrimApp {dst = SOME dst, ...} =>
                                temps dst @ ts
                           | _ => ts)
            fun uses ss =
               List.fold (ss, [], fn (s, ts) =>
                          case s of
                             Statement.Move {src, ...} => temps src @ ts
                           | Statement.PrimApp {args, ...} =>
                                Vector.fold (args, ts, fn (z, ts) =>
                                             temps z @ ts)
                           )
            (* an arm: its label, what to hoist, its moves, and its join *)
            fun arm l =
               case block l of
                  Block.T {kind = Kind.Jump, statements,
                           transfer = Transfer.Goto j, ...} =>
                     let
                        val ss = Vector.toList statements
                        val n = List.length ss
                        val k = List.fold (List.rev ss, (0, true), fn (s, (k, b)) =>
                                           if b andalso isMove s
                                              then (k + 1, true)
                                           else (k, false))
                        val k = #1 k
                        val pre = List.firstN (ss, n - k)
                     in
                        if ! (#preds (info l)) = 1
                           andalso List.forall (pre, pure)
                           then SOME (SOME l, pre, List.dropPrefix (ss, n - k),
                                      j)
                        else NONE
                     end
                | _ => NONE
            fun hoist (Block.T {kind, label, live, raises, returns, statements,
                                transfer}) =
               case transfer of
                  Transfer.Switch (Switch.T {cases, default, expect = NONE,
                                             test, ...}) =>
                     (case (Vector.toList (Vector.map (cases, #2))
                            @ (case default of NONE => [] | SOME l => [l])) of
                         [l, l'] =>
                            let
                               (* a target that is itself the join *)
                               fun plain l = (NONE, [], [], l)
                               fun tri (a as (_, _, _, j), other) =
                                  if Label.equals (j, other) then SOME a
                                  else NONE
                               val a = arm l
                               val a' = arm l'
                               val both =
                                  case (a, a') of
                                     (SOME (a as (_, _, _, j)),
                                      SOME (a' as (_, _, _, j'))) =>
                                        if Label.equals (j, j')
                                           then SOME (a, a')
                                        else NONE
                                   | _ => NONE
                               val z =
                                  if Label.equals (l, l') then NONE
                                  else
                                  case both of
                                     SOME z => SOME z
                                   | NONE =>
                                        (case (Option.fold
                                               (a, NONE, fn (a, _) =>
                                                tri (a, l')),
                                               Option.fold
                                               (a', NONE, fn (a', _) =>
                                                tri (a', l))) of
                                            (SOME a, _) => SOME (a, plain l')
                                          | (_, SOME a') => SOME (plain l, a')
                                          | _ => NONE)
                            in
                               case z of
                                  NONE => ()
                                | SOME z =>
                                     try (z, test,
                                          fn ss =>
                                          Block.T {kind = kind, label = label,
                                                   live = live, raises = raises,
                                                   returns = returns,
                                                   statements = ss,
                                                   transfer = transfer},
                                          Vector.toList statements)
                            end
                       | _ => ())
                | _ => ()
            and try (((la, pa, ma, j), (lb, pb, mb, _)), test, mk, ss) =
               let
                  val (da, db) = (defs pa, defs pb)
                  val d = da @ db
                  val liveJ = liveTs j
                  fun liveOf l = case l of NONE => liveJ | SOME l => liveTs l
                  val tt = temps test
                  val n = List.length pa + List.length pb
                  (* the comparison that the switch tests must stay last *)
                  val split =
                     case List.rev ss of
                        (s as Statement.PrimApp {dst = SOME z, ...}) :: ss' =>
                           if disjoint (temps z, tt) then SOME (ss, [])
                           else if disjoint (d, uses [s])
                                   andalso disjoint (tt, uses (pa @ pb))
                              then SOME (List.rev ss', [s])
                           else NONE
                      | _ => SOME (ss, [])
                  (* the arm that is hoisted second mustn't clobber what
                   * the moves of the first read
                   *)
                  val pre =
                     if disjoint (db, uses ma) then SOME (pa @ pb)
                     else if disjoint (da, uses mb) then SOME (pb @ pa)
                     else NONE
                  fun shrink (l, pre, ms) =
                     case l of
                        NONE => ()
                      | SOME l =>
                           case block l of
                              Block.T {kind, label, live, raises, returns,
                                       transfer, ...} =>
                                 let
                                    val old = liveTs l
                                    val us = uses ms
                                    val more =
                                       List.removeDuplicates
                                       (List.keepAll
                                        (defs pre, fn t =>
                                         mem (us, t) andalso not (mem (old, t))),
                                        fn (t, t') => mem ([t], t'))
                                 in
                                    #new (info l)
                                    := SOME (Block.T
                                             {kind = kind, label = label,
                                              live = (Vector.concat
                                                      [live,
                                                       Vector.fromListMap
                                                       (more, Live.Temporary)]),
                                              raises = raises,
                                              returns = returns,
                                              statements = Vector.fromList ms,
                                              transfer = transfer})
                                 end
               in
                  case split of
                     NONE => ()
                   | SOME (ss, cmp) =>
                        if n > 0 andalso n <= 6
                           andalso List.length ma + List.length mb <= 4
                           andalso disjoint (d, tt)
                           andalso disjoint (da, liveOf lb)
                           andalso disjoint (db, liveOf la)
                           andalso disjoint (d, liveJ)
                           andalso isSome pre
                           then (shrink (la, pa, ma)
                                 ; shrink (lb, pb, mb)
                                 ; (case mk (Vector.fromList
                                             (ss @ valOf pre @ cmp)) of
                                       b as Block.T {label, ...} =>
                                          #new (info label) := SOME b))
                        else ()
               end
            fun target l = Int.inc (#preds (info l))
            val () =
               Vector.foreach
               (blocks, fn Block.T {transfer, ...} =>
                case transfer of
                   Transfer.CCall {return = SOME {return, ...}, ...} =>
                      target return
                 | Transfer.Goto l => target l
                 | Transfer.Spork {cont, spwn, ...} => (target cont; target spwn)
                 | Transfer.Switch (Switch.T {cases, default, ...}) =>
                      (Vector.foreach (cases, target o #2)
                       ; Option.app (default, target))
                 | _ => ())
            val () = Vector.foreach (blocks, hoist)
         in
            (Vector.map (blocks, fn Block.T {label, ...} => block label), block)
         end
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
                                             defaultLabel = l',
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
      (* all of the loops: their headers, blocks, and nesting depth *)
      val allLoops: {headers: Label.t list, blocks: Label.t list,
                     depth: int} list ref = ref []
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
            fun walk (forest, headers, path): Label.t list =
               let
                  val {loops, notInLoop} = DirectedGraph.LoopForest.dest forest
                  val inside = ref (Vector.toListKeepAllMap (notInLoop, fn l => l))
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
                  ; Vector.foreachi
                    (loops, fn (k, {headers, child}) =>
                     let
                        val bs = walk (child, headers, k :: path)
                     in
                        List.push
                        (allLoops,
                         {headers = Vector.toListKeepAllMap (headers, fn l => l),
                          blocks = bs,
                          depth = 1 + List.length path})
                        ; inside := bs @ !inside
                     end)
                  ; !inside
               end
         in
            ignore (walk (DirectedGraph.loopForestSteensgaard
                          (g, {root = root, nodeValue = nodeLabel}),
                          Vector.new0 (), []))
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
      (* 2. Split each temporary into its webs: the live ranges that are
       * connected by definitions reaching uses.  A temporary gets a new
       * segment at each definition, plus one for its value at the start of
       * each block where it is live; a segment at the end of a block is the
       * same web as the one at the start of each successor.  Webs, not
       * temporaries, are what get registers, so a temporary can be in
       * different registers at different points.
       *)
      datatype web =
         W of {parent: web option ref,
               ct: CType.t,
               count: int ref,                (* weighted uses and defs *)
               across: bool ref,              (* live across a C call? *)
               adj: web list ref,             (* interferes with *)
               aff: (web * int) list ref,     (* copies to/from *)
               live: bool ref,
               mark: int ref,
               noReg: bool ref,
               home: int option ref}
      val webs: web list ref = ref []
      fun newWeb (ct, noReg) =
         let
            val w = W {parent = ref NONE, ct = ct, count = ref 0,
                       across = ref false, adj = ref [], aff = ref [],
                       live = ref false, mark = ref 0, noReg = ref noReg,
                       home = ref NONE}
         in
            List.push (webs, w); w
         end
      fun find (w as W {parent, ...}) =
         case !parent of
            NONE => w
          | SOME p => let val r = find p in parent := SOME r; r end
      fun sameWeb (W {live, ...}, W {live = live', ...}) = live = live'
      fun union (w, w') =
         let
            val w as W r = find w
            val w' as W r' = find w'
         in
            if sameWeb (w, w') then ()
            else (#parent r' := SOME w
                  ; #count r := ! (#count r) + ! (#count r')
                  ; #across r := (! (#across r) orelse ! (#across r'))
                  ; #noReg r := (! (#noReg r) orelse ! (#noReg r'))
                  ; #adj r := ! (#adj r') @ ! (#adj r)
                  ; #aff r := ! (#aff r') @ ! (#aff r))
         end
      datatype event =
         Def of web * web option    (* a copy doesn't interfere with its source *)
       | Use of web
      (* a stack slot: its offset and type *)
      fun slotKey (StackOffset.T {offset, ty, ...}) =
         (Bytes.toInt offset, Type.toCType ty)
      fun sameSlot ((off, ct), (off', ct')) =
         off = off' andalso size ct = size ct' andalso isReal ct = isReal ct'
      val {get = blockWebs: Label.t -> {(* at the start of the block *)
                                        segIn: (entry * web) list ref,
                                        (* the definitions, in order *)
                                        defs: (entry * web) list ref,
                                        (* in reverse order *)
                                        events: event list ref,
                                        (* the stack slots that are kept in
                                         * registers throughout the loop that
                                         * the block is in (see "promotion")
                                         *)
                                        promoted: ((int * CType.t) * web) list ref,
                                        (* which such loop; 0 if none *)
                                        ploop: int ref,
                                        (* is the header of such a loop? *)
                                        pheader: bool ref},
           ...} =
         Property.get (Label.plist,
                       Property.initFun (fn _ => {segIn = ref [], defs = ref [],
                                                  events = ref [],
                                                  promoted = ref [],
                                                  ploop = ref 0,
                                                  pheader = ref false}))
      fun sameEntry (e: entry, e': entry) = #live e = #live e'
      fun segIn (l, e: entry, noReg) =
         let
            val {segIn, ...} = blockWebs l
         in
            case List.peek (!segIn, fn (e', _) => sameEntry (e, e')) of
               SOME (_, w) => w
             | NONE =>
                  let
                     val w = newWeb (#ct e, noReg)
                  in
                     List.push (segIn, (e, w)); w
                  end
         end
      fun stripCast z =
         case z of
            Operand.Cast (z, _) => stripCast z
          | _ => z
      fun dstTemp z =
         case stripCast z of
            Operand.Temporary t => SOME t
          | _ => NONE
      val affinities: (web * web * int) list ref = ref []
      val () =
         Vector.foreach
         (blocks, fn b as Block.T {kind, label, statements, transfer, ...} =>
          let
             val table = blockRegion label
             val {defs, events, ...} = blockWebs label
             (* weigh uses by loop depth *)
             val weight =
                List.fold (! (#loopPath (labelInfo label)), 1, fn (_, w) =>
                           if w < 4096 then 8 * w else w)
             val () =
                if Kind.isEntry kind andalso not (List.isEmpty (liveTemps b))
                   then Error.bug "VibeArmCodegen: temporary live at entry"
                else ()
             val cur: (entry * web) list ref =
                ref (List.map (liveTemps b, fn e => (e, segIn (label, e, false))))
             fun curWeb e =
                case List.peek (!cur, fn (e', _) => sameEntry (e, e')) of
                   SOME (_, w) => w
                 | NONE =>
                      (* not live here, according to the backend: keep it in
                       * memory
                       *)
                      let
                         val w = segIn (label, e, true)
                      in
                         List.push (cur, (e, w)); w
                      end
             fun bump (W {count, ...}) = count := !count + weight
             fun use z =
                foreachTemp (z, fn t =>
                             let
                                val w = curWeb (entry (table, t))
                             in
                                bump w; List.push (events, Use w)
                             end)
             fun def (t, src) =
                let
                   val e = entry (table, t)
                   val w = newWeb (#ct e, false)
                in
                   bump w
                   ; List.push (defs, (e, w))
                   ; List.push (events, Def (w, src))
                   ; cur := (e, w) :: List.keepAll (!cur, fn (e', _) =>
                                                    not (sameEntry (e, e')))
                   ; (case src of
                         NONE => ()
                       | SOME (w' as W {aff = aff', ...}) =>
                            let
                               val W {aff, ...} = w
                            in
                               List.push (aff, (w', weight))
                               ; List.push (aff', (w, weight))
                               ; List.push (affinities, (w, w', weight))
                            end)
                end
             fun dst (z, src) =
                case dstTemp z of
                   NONE => use z
                 | SOME t => def (t, src)
             val () =
                case kind of
                   Kind.CReturn {dst = SOME z, ...} =>
                      dst (Live.toOperand z, NONE)
                 | _ => ()
             val () =
                Vector.foreach
                (statements, fn s =>
                 case s of
                    Statement.Move {dst = d, src} =>
                       (use src
                        ; dst (d,
                               case (dstTemp d, dstTemp src) of
                                  (SOME td, SOME ts) =>
                                     if CType.equals
                                        (Type.toCType (Temporary.ty td),
                                         Type.toCType (Temporary.ty ts))
                                        then SOME (curWeb (entry (table, ts)))
                                     else NONE
                                | _ => NONE))
                  | Statement.PrimApp {args, dst = d, ...} =>
                       (Vector.foreach (args, use)
                        ; Option.app (d, fn d => dst (d, NONE))))
             val () = Transfer.foldOperands (transfer, (), fn (z, ()) => use z)
          in
             defs := List.rev (!defs)
          end)
      (* the web of a temporary at the end of a block *)
      fun segOut (l, e) =
         case List.peek (List.rev (! (#defs (blockWebs l))), fn (e', _) =>
                         sameEntry (e, e')) of
            SOME (_, w) => w
          | NONE => segIn (l, e, true)
      val () =
         Vector.foreach
         (blocks, fn Block.T {label, transfer, ...} =>
          (List.foreach
           (nearSuccessors transfer, fn l =>
            List.foreach (liveTemps (labelBlock l), fn e =>
                          union (segOut (label, e), segIn (l, e, false))))
           ; (case transfer of
                 Transfer.CCall {return = SOME {return, ...}, ...} =>
                    List.foreach
                    (liveTemps (labelBlock return), fn e =>
                     let
                        val W {across, ...} = find (segIn (return, e, false))
                     in
                        #across e := true
                        ; across := true
                     end)
               | _ => ())))
      (* 3. Interference, by a backward scan of each block. *)
      val () =
         Vector.foreach
         (blocks, fn Block.T {label, transfer, ...} =>
          let
             val live: web list ref = ref []
             fun add w =
                let
                   val w as W {live = l, ...} = find w
                in
                   if !l then () else (l := true; List.push (live, w))
                end
             fun def (w, src) =
                let
                   val w as W {adj, live = l, ...} = find w
                   val src = Option.map (src, find)
                in
                   List.foreach
                   (!live, fn w' as W {adj = adj', ...} =>
                    if sameWeb (w, w')
                       orelse (case src of
                                  NONE => false
                                | SOME w'' => sameWeb (w', w''))
                       then ()
                    else (List.push (adj, w'); List.push (adj', w)))
                   ; if !l
                        then (l := false
                              ; live := List.remove (!live, fn w' =>
                                                     sameWeb (w, w')))
                     else ()
                end
             val () =
                List.foreach
                (nearSuccessors transfer, fn l =>
                 List.foreach (liveTemps (labelBlock l), fn e =>
                               add (segOut (label, e))))
             val () =
                List.foreach (! (#events (blockWebs label)), fn ev =>
                              case ev of
                                 Def z => def z
                               | Use w => add w)
          in
             List.foreach (!live, fn W {live, ...} => live := false)
          end)
      val caller = [8, 9, 10, 11, 12, 13, 14, 15]
      val callee = [22, 23, 24, 25, 26, 27, 28]
      val fcaller = List.tabulate (13, fn k => 51 + k)
      val fcallee = List.tabulate (8, fn k => 40 + k)
      fun callerSave r = (8 <= r andalso r <= 15) orelse r >= 51
      (* the distinct webs among ws *)
      val stamp = ref 0
      fun distinct ws =
         (Int.inc stamp
          ; List.fold (ws, [], fn (w, ac) =>
                       let
                          val w as W {mark, ...} = find w
                       in
                          if !mark = !stamp then ac
                          else (mark := !stamp; w :: ac)
                       end))
      (* Promotion of stack slots in loops.  Variables that are live across a
       * call that may collect live in stack slots, including ones that are
       * only used in a loop that calls nothing but has a limit check.  As
       * the x86/amd64 codegens do with their live transfers, keep such slots
       * in registers around the loop: a slot that is read in a loop gets a
       * web that is live throughout the loop.  Stores still write through to
       * memory, so nothing needs to happen at exits, calls, or collections;
       * the registers are loaded in a preheader, which every edge into the
       * loop from outside goes through (such edges only go to headers).
       *
       * A "loop" here is a region: a cycle (strongly connected component)
       * among the blocks that aren't entry blocks (so StackTop is constant)
       * and don't end in a C call that uses the stack.  A region may be
       * entered at several blocks (commonly: the top of the loop, and the
       * block after its limit check, where it is re-entered after a
       * collection); each gets a preheader.
       *)
      val () =
         let
            fun transferOf l =
               case labelBlock l of Block.T {transfer, ...} => transfer
            (* a block that can be in a region *)
            fun good l =
               not (isEntry l)
               andalso (case transferOf l of
                           Transfer.CCall {func, ...} =>
                              not (CFunction.readsStackTop func
                                   orelse CFunction.writesStackTop func)
                         | _ => true)
            val g = DirectedGraph.new ()
            val {get = nodeLabel: unit DirectedGraph.Node.t -> Label.t option,
                 set = setNodeLabel, ...} =
               Property.getSetOnce
               (DirectedGraph.Node.plist, Property.initConst NONE)
            val {get = labelNode: Label.t -> unit DirectedGraph.Node.t option,
                 ...} =
               Property.get
               (Label.plist,
                Property.initFun
                (fn l =>
                 if good l
                    then let
                            val n = DirectedGraph.newNode g
                         in
                            setNodeLabel (n, SOME l); SOME n
                         end
                 else NONE))
            val () =
               Vector.foreach
               (blocks, fn Block.T {label, transfer, ...} =>
                case labelNode label of
                   NONE => ()
                 | SOME n =>
                      List.foreach
                      (nearSuccessors transfer, fn l =>
                       case labelNode l of
                          NONE => ()
                        | SOME n' =>
                             ignore (DirectedGraph.addEdge
                                     (g, {from = n, to = n'}))))
            (* the regions: the cycles among the good blocks *)
            val regions =
               List.keepAllMap
               (DirectedGraph.stronglyConnectedComponents g, fn ns =>
                let
                   val ls = List.keepAllMap (ns, nodeLabel)
                in
                   case ls of
                      [] => NONE
                    | [l] =>
                         if List.exists (nearSuccessors (transferOf l), fn l' =>
                                         Label.equals (l, l'))
                            then SOME ls
                         else NONE
                    | _ => SOME ls
                end)
         in
            List.foreachi
            (regions, fn (id, bs) =>
                   let
                      val calls =
                         List.exists (bs, fn l =>
                                      case transferOf l of
                                         Transfer.CCall _ => true
                                       | _ => false)
                      (* the slots that occur in the loop, with their weighted
                       * numbers of reads
                       *)
                      val slots: ((int * CType.t) * int ref) list ref = ref []
                      fun note (so, n) =
                         let
                            val k = slotKey so
                         in
                            case List.peek (!slots, fn (k', _) =>
                                            sameSlot (k, k')) of
                               SOME (_, r) => r := !r + n
                             | NONE => List.push (slots, (k, ref n))
                         end
                      fun visit (z, isDst, weight) =
                         case z of
                            Operand.Address z => visit (z, true, weight)
                          | Operand.Cast (z, _) => visit (z, isDst, weight)
                          | Operand.Offset {base, ...} =>
                               visit (base, false, weight)
                          | Operand.SequenceOffset {base, index, ...} =>
                               (visit (base, false, weight)
                                ; visit (index, false, weight))
                          | Operand.StackOffset so =>
                               note (so, if isDst then 0 else weight)
                          | _ => ()
                      val () =
                            List.foreach
                            (bs, fn l =>
                             let
                                val Block.T {kind, statements, transfer, ...} =
                                   labelBlock l
                                val w =
                                   List.fold
                                   (! (#loopPath (labelInfo l)), 1, fn (_, w) =>
                                    if w < 4096 then 8 * w else w)
                             in
                                (case kind of
                                    Kind.CReturn {dst = SOME z, ...} =>
                                       visit (Live.toOperand z, true, w)
                                  | _ => ())
                                ; Vector.foreach
                                  (statements, fn s =>
                                   case s of
                                      Statement.Move {dst, src} =>
                                         (visit (dst, true, w)
                                          ; visit (src, false, w))
                                    | Statement.PrimApp {args, dst, ...} =>
                                         (Option.app (dst, fn z =>
                                                      visit (z, true, w))
                                          ; Vector.foreach (args, fn z =>
                                                            visit (z, false, w))))
                                ; Transfer.foldOperands
                                  (transfer, (), fn (z, ()) =>
                                   visit (z, false, w))
                             end)
                      fun overlaps ((off, ct), (off', ct')) =
                         not (sameSlot ((off, ct), (off', ct')))
                         andalso off + size ct > off'
                         andalso off' + size ct' > off
                      val candidates =
                         List.keepAll
                         (!slots, fn (k, n) =>
                          !n > 0
                          andalso not (List.exists (!slots, fn (k', _) =>
                                                    overlaps (k, k'))))
                      val candidates =
                         List.firstN
                         (QuickSort.sortList (candidates, fn ((_, n), (_, n')) =>
                                              !n >= !n'),
                          Int.min (16, List.length candidates))
                   in
                      if List.isEmpty candidates then ()
                      else
                         let
                            (* the webs that are live somewhere in the loop *)
                            val others =
                               distinct
                               (List.fold
                                (bs, [], fn (l, ac) =>
                                 let
                                    val {segIn, defs, ...} = blockWebs l
                                 in
                                    List.fold (!segIn @ !defs, ac, fn ((_, w), ac) =>
                                               w :: ac)
                                 end))
                            val ps =
                               List.map
                               (candidates, fn (k as (_, ct), n) =>
                                let
                                   val w as W {count, across, ...} =
                                      newWeb (ct, false)
                                in
                                   count := !n
                                   ; across := calls
                                   ; (k, w)
                                end)
                            fun edge (w as W {adj, ...}, w' as W {adj = adj', ...}) =
                               (List.push (adj, w'); List.push (adj', w))
                         in
                            List.foreach
                            (ps, fn (_, w) =>
                             (List.foreach (others, fn w' => edge (w, w'))
                              ; List.foreach
                                (ps, fn (_, w') =>
                                 if sameWeb (w, w') then () else edge (w, w'))))
                            ; List.foreach
                              (bs, fn l =>
                               let
                                  val {promoted, ploop, ...} = blockWebs l
                               in
                                  promoted := ps
                                  ; ploop := id + 1
                               end)
                         end
                   end)
            (* a block with an edge from outside its region needs a preheader *)
            ; Vector.foreach
              (blocks, fn Block.T {label, transfer, ...} =>
               List.foreach
               (nearSuccessors transfer
                @ (case ifConvert transfer of
                      NONE => []
                    | SOME {join, ...} => [join]),
                fn l =>
                let
                   val {ploop, pheader, ...} = blockWebs l
                in
                   if !ploop <> 0 andalso !ploop <> ! (#ploop (blockWebs label))
                      then pheader := true
                   else ()
                end))
         end
      (* 4. Coalesce: make a copy's source and destination the same web, when
       * they don't interfere and the result is sure to be colorable (it
       * interferes with fewer webs than there are registers).
       *)
      val () =
         if localRA then ()
         else
            List.foreach
            (QuickSort.sortList (!affinities, fn ((_, _, n), (_, _, n')) =>
                                 n >= n'),
             fn (w, w', _) =>
             let
                val w as W {ct, adj, noReg, ...} = find w
                val w' as W {ct = ct', adj = adj', noReg = noReg', ...} = find w'
             in
                if sameWeb (w, w')
                   orelse isReal ct <> isReal ct'
                   orelse !noReg orelse !noReg'
                   orelse List.exists (!adj, fn w'' => sameWeb (find w'', w'))
                   orelse (List.length
                           (List.keepAll
                            (distinct (!adj @ !adj'), fn W {ct = ct'', ...} =>
                             isReal ct'' = isReal ct))
                           >= (if isReal ct then 21 else 15))
                   then ()
                else union (w, w')
             end)
      (* 5. Color, in order of use count.  Prefer the register of a web that
       * this one is copied to or from.
       *)
      val () =
         if localRA then ()
         else
            List.foreach
            (QuickSort.sortList
             (distinct (!webs), fn (W {count, ...}, W {count = count', ...}) =>
              !count >= !count'),
             fn W {ct, across, adj, aff, noReg, home, ...} =>
             if !noReg then ()
             else
                let
                   val used =
                      List.keepAllMap (!adj, fn w =>
                                       let
                                          val W {home, ...} = find w
                                       in
                                          !home
                                       end)
                   fun free r = not (List.contains (used, r, op =))
                   val regs =
                      case (isReal ct, !across) of
                         (false, false) => caller @ callee
                       | (false, true) => callee @ caller
                       | (true, false) => fcaller @ fcallee
                       | (true, true) => fcallee @ fcaller
                   (* the best register of a copy partner *)
                   val (partner, _) =
                      List.fold
                      (!aff, (NONE, 0), fn ((w, n), best as (_, n')) =>
                       let
                          val W {ct = ct', home, ...} = find w
                       in
                          case !home of
                             NONE => best
                           | SOME r =>
                                if n > n' andalso free r
                                   andalso isReal ct' = isReal ct
                                   then (SOME r, n)
                                else best
                       end)
                in
                   home := (case partner of
                               SOME r => SOME r
                             | NONE => List.peek (regs, free))
                end)
      (* The webs of the temporaries at the point that is being output. *)
      val curWebs: (entry * web) list ref = ref []
      (* the definitions that are still to come in the block *)
      val pendingDefs: (entry * web) list ref = ref []
      (* the promoted stack slots of the block *)
      val curPromoted: ((int * CType.t) * web) list ref = ref []
      (* the block that is being output *)
      val curLabel = ref (Label.newNoname ())
      fun webHome w = let val W {home, ...} = find w in !home end
      fun enterBlock l =
         let
            val {segIn, defs, promoted, ...} = blockWebs l
         in
            curWebs := !segIn
            ; pendingDefs := !defs
            ; curPromoted := !promoted
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
               andalso !ploop <> 0
               andalso !ploop <> ! (#ploop (blockWebs (!curLabel)))
               then if !pheader
                       then preLabel l
                    else Error.bug "VibeArmCodegen.target: no preheader"
            else blockLabel l
         end
      (* the register of a temporary that is being used *)
      fun tempHome t =
         let
            val e = entry (!region, t)
         in
            case List.peek (!curWebs, fn (e', _) => sameEntry (e, e')) of
               NONE => NONE
             | SOME (_, w) => webHome w
         end
      (* the register of a temporary that is being defined *)
      fun defHome t =
         let
            val e = entry (!region, t)
         in
            case List.peek (!pendingDefs, fn (e', _) => sameEntry (e, e')) of
               NONE => NONE
             | SOME (_, w) => webHome w
         end
      (* the definition of z has been output *)
      fun defined z =
         case dstTemp z of
            NONE => ()
          | SOME t =>
               let
                  val e = entry (!region, t)
                  fun same (e', _) = sameEntry (e, e')
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
      fun commit (opc, rs) =
         List.foreach
         (rs, fn r =>
          let
             val ct = if r >= 32 then CType.Real64 else CType.Word64
          in
             i [opc, " ", reg (r, ct), ", ",
                mem (31, saveBase + 8 * (if r >= 32 then r - 43 else r - 8),
                     ct)]
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
                          i ["str ", reg (r, wideCt ct), ", ",
                             mem (b, off, wideCt ct)]
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
                     List.exists (! (!region), fn {ct = ct', index, across, ...} =>
                                  index = n andalso CType.equals (ct, ct')
                                  andalso !across)
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
                              i ["ldr", sfx ct, " ", reg (r, ct), ", ",
                                 mem (b, off, ct)]
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
                                i ["ldr", sfx ct, " ", reg (d, ct), ", ",
                                   memOf (z, t, ct)]))
         end
      (* An addressing mode for a memory operand that is accessed at type ct.
       * A sequence element is addressed as [base, index, lsl #s] when it can
       * be: no offset, and a scale that is 1 or the size of the access.
       *)
      and memOf (z, t, ct): string =
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
                              concat ["[", x b, ", ", x ix,
                                      case n of
                                         1 => "]"
                                       | 2 => ", lsl #1]"
                                       | 4 => ", lsl #2]"
                                       | _ => ", lsl #3]"]
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
                  1 => i ["and w", int h, ", w", int r, ", #0xff"]
                | 2 => i ["and w", int h, ", w", int r, ", #0xffff"]
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
                        i ["str", sfx ct, " ", reg (r, ct), ", ",
                           mem (20, off, ct)]
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
                              i ["str", sfx ct, " ", reg (r, ct), ", ",
                                 memOf (dst, 3, ct)])
             | (_, SOME h) => set h
             | (_, NONE) =>
                  i ["str", sfx ct, " ", reg (r, ct), ", ", memOf (dst, 3, ct)]
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
            fun hex (n: IntInf.t): string =
               let
                  fun loop n =
                     if n = 0 then ""
                     else loop (n div 16)
                          ^ String.fromChar
                            (String.sub ("0123456789abcdef",
                                         IntInf.toInt (n mod 16)))
               in
                  if n = 0 then "0x0" else "0x" ^ loop n
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
                              then (i [opc, " ", reg (d, ct), ", ",
                                       reg (v 0, ct), ", #", hex n]
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
                        fun r k = if N = 64 then x k else "w" ^ int k
                     in
                        if dv < 2 then false
                        else
                           let
                              val n = ext (0, ws, signed, false)
                              (* the quotient is in register q *)
                              fun finish q =
                                 (if isRem
                                     then (movImm (16, dv)
                                           ; i ["msub ", r d, ", ", r q, ", ",
                                                r 16, ", ", r n])
                                  else if q = d then ()
                                  else i ["mov ", r d, ", ", r q]
                                  ; done ())
                              fun shiftBy (opc, k) =
                                 if k > 0
                                    then i [opc, " ", r 5, ", ", r 5, ", #", int k]
                                 else ()
                           in
                              (case (log2 dv, signed) of
                                  (SOME k, false) =>
                                     (if isRem
                                         then i ["and ", r d, ", ", r n, ", #",
                                                 hex (dv - 1)]
                                      else i ["lsr ", r d, ", ", r n, ", #", int k]
                                      ; done ())
                                | (SOME k, true) =>
                                     (* round toward zero: add 2^k - 1 to a
                                      * negative dividend
                                      *)
                                     (i ["asr ", r 5, ", ", r n, ", #", int (N - 1)]
                                      ; i ["add ", r 5, ", ", r n, ", ", r 5,
                                           ", lsr #", int (N - k)]
                                      ; if isRem
                                           then (i ["and ", r 5, ", ", r 5, ", #",
                                                    hex (pow2 N - dv)]
                                                 ; i ["sub ", r d, ", ", r n, ", ",
                                                      r 5])
                                        else i ["asr ", r d, ", ", r 5, ", #",
                                                int k]
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
                                             then i ["smulh x5, ", x n, ", x16"]
                                          else (i ["smull x5, w", int n, ", w16"]
                                                ; i ["asr x5, x5, #32"])
                                        ; if m >= pow2 (N - 1)
                                             then i ["add ", r 5, ", ", r 5, ", ",
                                                     r n]
                                          else ()
                                        ; shiftBy ("asr", s - 1)
                                        ; i ["add ", r 5, ", ", r 5, ", ", r n,
                                             ", lsr #", int (N - 1)]
                                        ; finish 5
                                     end
                                | (NONE, false) =>
                                     if N = 32
                                        then
                                           (* n < 2^32: one 64-bit multiply is
                                            * exact
                                            *)
                                           (movImm (16, pow2 64 div dv + 1)
                                            ; i ["umulh x5, ", x n, ", x16"]
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
                                                    ; i ["umulh x5, ", x n, ", x16"]
                                                    ; shiftBy ("lsr", s))
                                           else
                                              (* m needs 65 bits *)
                                              (movImm (16, m - pow2 64)
                                               ; i ["umulh x5, ", x n, ", x16"]
                                               ; i ["sub x6, ", x n, ", x5"]
                                               ; i ["add x5, x5, x6, lsr #1"]
                                               ; shiftBy ("lsr", s - 1))
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
                              val saves =
                                 if localRA then []
                                 else callerSaves (List.map (!curWebs, #2)
                                                   @ List.map (!curPromoted, #2))
                           in
                              mov (0, obj, ct)
                              ; i ["mov x1, x5"]
                              ; commit ("str", saves)
                              ; flushCallerSaves (fn _ => true)
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
             | Word_andb ws => logic ("and", ws)
             | Word_castToReal (ws, _) => un ("fmov", wct ws)
             | Word_equal ws => cmp ("cmp", wct ws, "eq")
             | Word_extdToWord (ws1, ws2, {signed}) =>
                  let
                     val r = v 0
                  in
                     if signed andalso bits ws1 < bits ws2
                        then i ["sxt", bhw (bits ws1), " ", reg (d, wct ws2),
                                ", w", int r]
                     else if bits ws1 = 64 andalso bits ws2 = 32
                        (* A truncation must clear the upper half of the
                         * register, even when d = r.
                         *)
                        then i ["mov w", int d, ", w", int r]
                     else mov (d, r, wct ws2)
                     ; done ()
                  end
             | Word_lshift ws => shift ("lsl", ws, v 0)
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
                              then i ["cmp ", r, ", #", IntInf.toString n]
                           else i ["cmn ", r, ", #", IntInf.toString (~ n)]
                           ; cset cond
                        end
                  in
                     case (imm 0, imm 1) of
                        (_, SOME n) =>
                           cmpImm (0, n, if signed then "lt" else "lo")
                      | (SOME n, NONE) =>
                           cmpImm (1, n, if signed then "gt" else "hi")
                      | _ =>
                           let
                              val r0 = ext (0, ws, signed, false)
                              val r1 = ext (1, ws, signed, false)
                           in
                              i ["cmp ", reg (r0, wct ws), ", ",
                                 reg (r1, wct ws)]
                              ; cset (if signed then "lt" else "lo")
                           end
                  end
             | Word_mul (ws, _) =>
                  let
                     val ct = wct ws
                     fun default () = bin ("mul", ct, false)
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
                                    (i ["lsl ", reg (d, ct), ", ", r, ", #",
                                        int k]
                                     ; done ())
                               | (_, SOME k) =>
                                    (i ["add ", reg (d, ct), ", ", r, ", ", r,
                                        ", lsl #", int k]
                                     ; done ())
                               | _ => default ()
                           end
                      | _ => default ()
                  end
             | Word_mulCheckP z => chk ("mul", "", "", z, [0, 1])
             | Word_neg ws => un ("neg", wct ws)
             | Word_negCheckP z => chk ("neg", "vs", "cc", z, [0])
             | Word_notb ws => un ("mvn", wct ws)
             | Word_orb ws => logic ("orr", ws)
             | Word_quot (ws, {signed}) =>
                  if divConst (ws, signed, false) then ()
                  else
                  let
                     val r0 = ext (0, ws, signed, false)
                     val r1 = ext (1, ws, signed, false)
                  in
                     i [if signed then "sdiv " else "udiv ", reg (d, wct ws),
                        ", ", reg (r0, wct ws), ", ", reg (r1, wct ws)]
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
             | Word_ror ws =>
                  (case a 1 of
                      Operand.Const (Const.Word w) =>
                         (i ["ror ", reg (d, wct ws), ", ", reg (v 0, wct ws),
                             ", #",
                             IntInf.toString (WordX.toIntInf w
                                              mod IntInf.fromInt (bits ws))]
                          ; done ())
                    | _ => shift ("ror", ws, v 0))
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
                           i ["ubfx ", reg (d, ct), ", ", reg (v 0, ct), ", #",
                              IntInf.toString (const 1), ", #",
                              int (valOf (log2 (const 2 + 1)))]
                           ; done ()
                        end
                  else
                  shift (if signed then "asr" else "lsr", ws,
                         ext (0, ws, signed, false))
             | Word_sub ws => addsub (false, wct ws)
             | Word_subCheckP z => chk ("sub", "vs", "cc", z, [0, 1])
             | Word_xorb ws => logic ("eor", ws)
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
         if isNext l andalso target l = blockLabel l
            then ()
         else i ["b ", target l]
      (* (Local allocator.)  Live transfers: what registers hold at the start
       * of a block is fixed by the first of its predecessors to be output;
       * the others conform.
       *)
      fun liveIn l =
         List.map (liveTemps (labelBlock l), fn {ct, index, ...} =>
                   T (ct, index))
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
                                             (! (#promoted (blockWebs l)),
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
                  fun emit (d, s) =
                     if d >= 32
                        then i ["fmov d", int (d - 32), ", d", int (s - 32)]
                     else i ["mov ", x d, ", ", x s]
               in
                  case List.peek (ms, fn (d, _) =>
                                  not (List.exists (ms, fn (_, s) => s = d))) of
                     SOME (d, s) =>
                        (emit (d, s)
                         ; parallelMove (List.keepAll (ms, fn (d', _) =>
                                                       d' <> d)))
                   | NONE =>
                        (* a cycle: break it with a scratch register *)
                        let
                           val t = if d >= 32 then 48 else 16
                        in
                           emit (t, s)
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
                     i ["ldr", sfx ct, " ", reg (r, ct), ", ", mem (20, off, ct)]
                | T (ct, n) =>
                     let
                        val (b, off) = homeOf (ct, n)
                     in
                        i ["ldr", sfx ct, " ", reg (r, ct), ", ",
                           mem (b, off, ct)]
                     end)
         end
      (* compensation blocks, for edges that need code: output at the end of
       * the chunk
       *)
      val comps: (string * Label.t * rentry list) list ref = ref []
      (* the label to branch to in order to get to l *)
      fun edge l =
         if localRA andalso edgeNeedsCode l
            then
               let
                  val name = concat [if symbols then sym "edge_" else "Ledge_",
                                     self, "_", int (List.length (!comps))]
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
                      if !ploop <> 0
                         andalso !ploop <> ! (#ploop (blockWebs (!curLabel)))
                         andalso not (Option.isSome (! (entryState l)))
                         then
                            List.foreach
                            (!promoted, fn (k as (off, ct), _) =>
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
                                      (i ["ldr", sfx ct, " ", reg (r, ct), ", ",
                                          mem (20, off, ct)]
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
         CC of string           (* condition code *)
       | Z of string * bool     (* register is (true) or isn't (false) zero *)
      val negate =
         fn CC c =>
               CC (case c of
                      "eq" => "ne" | "ne" => "eq" | "lo" => "hs" | "hs" => "lo"
                    | "lt" => "ge" | "ge" => "lt" | "vs" => "vc" | "vc" => "vs"
                    | "cs" => "cc" | "cc" => "cs" | "mi" => "pl" | "pl" => "mi"
                    | "ls" => "hi" | "hi" => "ls" | "gt" => "le" | "le" => "gt"
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
                     then i ["ldr", sfx ct, " ", reg (d, ct), ", ",
                             memOf (dst, 3, ct)]
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
                          i [if isReal ct then "fcsel " else "csel ",
                             reg (d, wide), ", ", reg (s, wide), ", ",
                             reg (d, wide), ", ", c])
               end
            (* the moves of an arm are output in the context of its block *)
            fun arm (l, moves, c) =
               if List.isEmpty moves then ()
               else (enterBlock l; List.foreach (moves, select c))
            val c' = case negate (CC c) of
                        CC c' => c'
                      | _ => Error.bug "VibeArmCodegen.selects"
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
                                             i ["cset ", reg (d, CType.Word64),
                                                ", ", if b then c else c'])
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
      fun leaveChunk () = (flushFrontier (); flushStackTop (); i ["b ", leave])
      (* Transfers between chunks don't go through the trampoline: a
       * transfer to a known entry block of another chunk branches to its
       * entry stub.  This relies on every chunk running in the same frame
       * of saved registers (x29) with x19-x21 set.
       *)
      fun jump l =
         if ChunkLabel.equals (labelChunk l, chunkLabel)
            then gotoL l
         else i ["b ", sym (entryStub l)]
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
                     (* not x0: a C call's result may be in it (see
                      * directReturn)
                      *)
                     load (pst, 5, 3)
                     ; i ["sub x5, x5, #8"]
                     ; store (pst, 5)
                     ; load (psb, 6, 3)
                     ; i ["cmp x6, x5"]
                     ; i ["b.ls 1f"]
                     ; store (psb, 5)
                     ; label "1"
                  end
            else ())
      (* transfer to the entry block whose index is on top of the stack *)
      fun indJump () =
         (i ["ldr x16, [x20, #-8]"]
          ; symAddr (17, entryTable, {got = false})
          ; i ["ldr x16, [x17, x16, lsl #3]"]
          ; i ["br x16"])
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
               (case List.peek ([("_realCeil", "frintp "),
                                 ("_realFloor", "frintm "),
                                 ("_realTrunc", "frintz ")], fn (f, _) =>
                                List.exists (["Real32", "Real64"], fn r =>
                                             name = r ^ f)) of
                   NONE => false
                 | SOME (_, opc) =>
                      let
                         val r = scratch (ct, 0)
                      in
                         if directReturn return
                            then i [opc, reg (32, ct), ", ",
                                    reg (value (z, r, 16), ct)]
                         else (load (z, r, 16)
                               ; i [opc, reg (r, ct), ", ", reg (r, ct)]
                               ; i ["str ", reg (r, ct), ", [sp, #8]"])
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
                                            fn e => segIn (return, e, false))
                                  @ List.map (!curPromoted, #2))
                | _ => []
            val () = commit ("str", saves)
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
            val () = dropCallerSaves ()
            val () = addImm (31, 31, stk)
            val () =
               case (retTy, return) of
                  (NONE, _) => ()
                | (SOME ct, SOME {return, ...}) =>
                     if directReturn return
                        then ()
                     else if isReal ct
                             then i ["str d0, [sp, #8]"]
                          else i ["str x0, [sp]"]
                | (SOME _, NONE) => ()
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
               then indJump ()
            else (case return of
                     NONE => (movInt (0, ~2); i ["b ", leave])
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
                ; i ["add x20, ",
                   x (value (gcField GCField.StackBottom, 0, 3)), ", ",
                   x (value (gcField GCField.ExnStack, 1, 3))]
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
                                    ; bcond (CC "hi", edge default)
                                    ; i ["adr x17, ", table]
                                    ; i ["ldrsw x16, [x17, x16, lsl #2]"]
                                    ; i ["add x17, x17, x16"]
                                    ; i ["br x17"]
                                    ; label table
                                    ; Int.for
                                      (0, span + 1, fn k =>
                                       i [".long ",
                                          edge
                                          (case List.peek
                                                (cases', fn (m, _) =>
                                                 m = lo + IntInf.fromInt k) of
                                              SOME (_, l) => l
                                            | NONE => default),
                                          " - ", table])
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
               sameEntry (entry (!region, t), entry (!region, t'))
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
                                          sameEntry (e, entry (!region, t)))))
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
      fun setupCache (Block.T {kind, label, statements, transfer, ...}) =
         let
            (* the registers of the webs that are live somewhere in the block *)
            val busy =
               let
                  val {segIn, defs, promoted, ...} = blockWebs label
               in
                  List.keepAllMap (!segIn @ !defs, fn (_, w) => webHome w)
                  @ List.keepAllMap (!promoted, fn (_, w) => webHome w)
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
             then (i [".globl ", sym (entryStub l)]
                   ; i [if darwin then ".private_extern " else ".hidden ",
                        sym (entryStub l)]
                   ; label (sym (entryStub l))
                   ; addImm (31, 29, ~ frameSize))
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
                                i ["ldr", sfx ct, " ", reg (d, ct),
                                   if isReal ct then ", [sp, #8]" else ", [sp]"]
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
      (* jump to the entry block with index x3 *)
      ; symAddr (17, entryTable, {got = false})
      ; i ["ldr x16, [x17, x3, lsl #3]"]
      ; i ["br x16"]
      (* return x0 to the trampoline *)
      ; label leave
      ; i ["mov sp, x29"]
      ; List.foreachi (saved, fn (k, rs) => pair ("ldp ", rs, k))
      ; i ["ldp x29, x30, [sp], #160"]
      ; i ["ret"]
      ; outputBlocks (List.rev (!order))
      (* the preheaders (out of line, so that nothing falls through them):
       * load the promoted stack slots
       *)
      ; List.foreach
        (List.rev (!order), fn Block.T {label = l, ...} =>
         let
            val {pheader, promoted, ...} = blockWebs l
         in
            if !pheader andalso not localRA
               then (label (preLabel l)
                     ; List.foreach
                       (!promoted, fn ((off, ct), w) =>
                        case webHome w of
                           NONE => ()
                         | SOME r =>
                              i ["ldr", sfx ct, " ", reg (r, ct), ", ",
                                 mem (20, off, ct)])
                     ; i ["b ", blockLabel l])
            else ()
         end)
      (* the compensation blocks of the local register allocator *)
      ; List.foreach
        (List.rev (!comps), fn (name, l, state) =>
         (label name
          ; regs := state
          ; pinned := []
          ; conform l
          ; i ["b ", blockLabel l]))
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
            val darwin = !Control.Target.os = MLton.Platform.OS.Darwin
            fun sym s = if !Control.labelsHaveExtra_ then "_" ^ s else s
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
         in
            print (if darwin
                      then "\t.section __DATA,__const\n"
                   else "\t.section .data.rel.ro,\"aw\"\n")
            ; print "\t.p2align 3\n"
            ; print (concat ["\t.globl ", sym entryTable, "\n"])
            ; print (concat [if darwin then "\t.private_extern "
                             else "\t.hidden ",
                             sym entryTable, "\n"])
            ; print (concat [sym entryTable, ":\n"])
            ; Array.foreach
              (entries, fn lo =>
               print (concat ["\t.quad ",
                              case lo of
                                 NONE => "0"
                               | SOME l => sym (entryStub l),
                              "\n"]))
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
              outputChunk (chunk, print, {labelBlock = labelBlock,
                                          labelChunk = labelChunk,
                                          labelIndex = labelIndex}))
             ; done ()
          end)}
   end

end

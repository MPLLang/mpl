(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor VibeArmAnalysis (S: VIBE_ARM_ANALYSIS_STRUCTS): VIBE_ARM_ANALYSIS =
struct

open S
open Machine

(* Registers: 0-31 are x0-x30,sp; 32-63 are d0-d31. *)
fun isReal ct =
   case ct of CType.Real32 => true | CType.Real64 => true | _ => false
fun size ct = Bytes.toInt (CType.size ct)
(* the registers that webs may be given *)
val caller = [8, 9, 10, 11, 12, 13, 14, 15]
val callee = [22, 23, 24, 25, 26, 27, 28]
val fcaller = List.tabulate (13, fn k => 51 + k)
val fcallee = List.tabulate (8, fn k => 40 + k)
fun callerSave r = (8 <= r andalso r <= 15) orelse r >= 51

fun foreachTemp (z, f) =
   case z of
      Operand.Address z => foreachTemp (z, f)
    | Operand.Cast (z, _) => foreachTemp (z, f)
    | Operand.Offset {base, ...} => foreachTemp (base, f)
    | Operand.SequenceOffset {base, index, ...} =>
         (foreachTemp (base, f); foreachTemp (index, f))
    | Operand.Temporary t => f t
    | _ => ()
fun stripCast z =
   case z of
      Operand.Cast (z, _) => stripCast z
    | _ => z
fun dstTemp z =
   case stripCast z of
      Operand.Temporary t => SOME t
    | _ => NONE
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

(* a stack slot: its offset and type *)
type slot = int * CType.t
fun slotKey (StackOffset.T {offset, ty, ...}) =
   (Bytes.toInt offset, Type.toCType ty)
fun sameSlot ((off, ct), (off', ct')) =
   off = off' andalso size ct = size ct' andalso isReal ct = isReal ct'

(* A region's table of temporaries. *)
datatype entry' = E of entry
withtype entry = {ct: CType.t, index: int,
                  count: int ref,       (* number of uses and defs *)
                  across: bool ref,     (* live across some C call? *)
                  adj: entry' list ref, (* interferes with *)
                  live: bool ref,
                  home: int option ref}
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
fun sameEntry (e: entry, e': entry) = #live e = #live e'

(* A web: a live range of a temporary (or, for a promoted stack slot, of the
 * slot in a loop).  Webs are merged by union-find.
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

structure Temp =
   struct
      type t = entry

      fun across (e: t) = ! (#across e)
      fun ct (e: t) = #ct e
      val equals = sameEntry
      fun index (e: t) = #index e
   end

structure Region =
   struct
      type t = entry list ref

      fun new (): t = ref []
      val temp = entry
      fun temps (r: t) = !r
   end

structure Web =
   struct
      type t = web

      fun home w = let val W {home, ...} = find w in !home end
   end

type t =
   {blocks: Block.t vector,
    labelBlock: Label.t -> Block.t,
    blockRegion: Label.t -> Region.t,
    blockWebs: Label.t -> {segIn: (Temp.t * Web.t) list,
                           defs: (Temp.t * Web.t) list,
                           promoted: (slot * Web.t) list,
                           ploop: int,
                           pheader: bool},
    ifConvert: Transfer.t -> {caseLabel: Label.t,
                              caseMoves: {dst: Operand.t,
                                          src: Operand.t} list,
                              defaultLabel: Label.t,
                              defaultMoves: {dst: Operand.t,
                                             src: Operand.t} list,
                              join: Label.t} option,
    liveTemps: Block.t -> Temp.t list,
    loopDistance: Label.t * Label.t -> int option,
    nearSuccessors: Transfer.t -> Label.t list,
    segIn: Label.t * Temp.t -> Web.t}

fun analyze {blocks: Block.t vector,
             labelBlock: Label.t -> Block.t,
             localRA: bool}: t =
   let
      val {get = labelInfo: Label.t -> {near: Label.t list ref,
                                        (* is a loop header? *)
                                        loopHeader: bool ref,
                                        (* the nest of loops it is in,
                                         * outermost first
                                         *)
                                        loopPath: int list ref,
                                        region: entry list ref option ref},
           ...} =
         Property.get (Label.plist,
                       Property.initFun (fn _ => {near = ref [],
                                                  loopHeader = ref false,
                                                  loopPath = ref [],
                                                  region = ref NONE}))
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
   in
      {blocks = blocks,
       labelBlock = labelBlock,
       blockRegion = blockRegion,
       blockWebs = fn l => let
                              val {segIn, defs, promoted, ploop, pheader, ...} =
                                 blockWebs l
                           in
                              {segIn = !segIn,
                               defs = !defs,
                               promoted = !promoted,
                               ploop = !ploop,
                               pheader = !pheader}
                           end,
       ifConvert = ifConvert,
       liveTemps = liveTemps,
       loopDistance = loopDistance,
       nearSuccessors = nearSuccessors,
       segIn = fn (l, e) => segIn (l, e, false)}
   end

end

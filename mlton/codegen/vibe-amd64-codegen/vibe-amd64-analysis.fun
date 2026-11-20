(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor VibeAmd64Analysis (S: VIBE_AMD64_ANALYSIS_STRUCTS): VIBE_AMD64_ANALYSIS =
struct

open S
open Machine

(* Registers: 0-15 are rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, r8-r15;
 * 16-31 are xmm0-xmm15.
 *)
fun isReal ct =
   case ct of CType.Real32 => true | CType.Real64 => true | _ => false
fun size ct = Bytes.toInt (CType.size ct)
(* the registers that webs may be given *)
val caller = [6, 7, 8, 9, 10, 11]
val callee = [3, 14, 15]
val fcaller = List.tabulate (14, fn k => 18 + k)
val fcallee: int list = []
fun callerSave r = not (r = 3 orelse r = 14 orelse r = 15)

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

(* The analysis may make this many temporaries of each type, beyond those
 * that a chunk has: those that speculation renames to, and one for results
 * that nothing reads (see "sink").
 *)
val renameTemps = 16
val extraTemps = renameTemps + 1

(* A select: dst = if test = value then a else b.  The Machine IR has no
 * such statement; it is a PrimApp of a primitive that the IR never has
 * otherwise, with arguments test, value, a and b.
 *)
fun select {dst, test, value: WordX.t, a, b} =
   Statement.PrimApp {args = Vector.fromList [test,
                                              Operand.Const (Const.Word value),
                                              a, b],
                      dst = SOME dst,
                      prim = Prim.MLton_bogus}
fun isSelect (p: 'a Prim.t) =
   case p of
      Prim.MLton_bogus => true
    | _ => false

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
         promo: bool ref,               (* of a promoted stack slot? *)
         (* for a promoted slot: the webs that its loop defines or uses *)
         locals: web list ref,
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
    liveTemps: Block.t -> Temp.t list,
    loopDistance: Label.t * Label.t -> int option,
    nearSuccessors: Transfer.t -> Label.t list,
    segIn: Label.t * Temp.t -> Web.t}

fun analyze {blocks: Block.t vector,
             labelBlock: Label.t -> Block.t,
             tempsMax: CType.t -> int}: t =
   let
      (* a new temporary, if there is one left of its type *)
      val made: (CType.t * int ref) list ref = ref []
      fun fresh (ty: Type.t): Temporary.t option =
         let
            val ct = Type.toCType ty
            val n =
               case List.peek (!made, fn (ct', _) => CType.equals (ct, ct')) of
                  SOME (_, n) => n
                | NONE => let
                             val n = ref 0
                          in
                             List.push (made, (ct, n)); n
                          end
         in
            if !n >= renameTemps then NONE
            else (Int.inc n
                  ; SOME (Temporary.new (ty, SOME (tempsMax ct + !n))))
         end
      (* The temporary for results of its type that nothing reads.  The
       * Machine IR has primitives with an effect and no destination (a
       * compare-and-swap whose result is unused); here, every primitive that
       * is output has a destination.
       *)
      fun sink (ty: Type.t): Temporary.t =
         Temporary.new (ty, SOME (tempsMax (Type.toCType ty) + extraTemps))
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
      (* If-conversion.  A two-way switch where each target either is the
       * join block or is a block of a few moves to temporaries that goes to
       * the join block is replaced by selects (see "select", above) and a
       * jump to the join block: no branch to mispredict.  The moves must be
       * safe to execute unconditionally, so the sources may not be heap
       * loads.
       *
       * Speculation.  If-conversion wants the arms of the switch to be
       * nothing but moves.  So, when an arm starts with a few cheap
       * computations that have no effect other than to define temporaries
       * (as do both arms of "x mod c": the sign of x picks the formula),
       * hoist them into the block of the switch, ahead of the comparison.
       * Both arms' computations then always run, which costs less than
       * mispredicting a branch that depends on data.  It is safe when what
       * is hoisted can't fault (so: no heap loads) and writes only
       * temporaries that are dead in the other arm.
       *
       * The two take turns: an if-converted switch is itself a few cheap
       * computations, which can be hoisted into the switch that it is an
       * arm of.  That is how a nest of conditionals (Real.max, with its
       * tests for NaN) becomes straight-line code.
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
            val changed = ref false
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
                | Operand.Const Const.Null => true
                | Operand.Const (Const.Real _) => true
                | Operand.Const (Const.Word _) => true
                | Operand.Global _ => true
                | Operand.StackOffset _ => true
                | Operand.Temporary _ => true
                | _ => false
            fun isMove s =
               case s of
                  Statement.Move {dst = Operand.Temporary _, src} => okSrc src
                | _ => false
            (* A division traps if the divisor is 0, or is -1 and the
             * quotient overflows: only divide by other constants.
             *)
            fun okDivisor args =
               case Vector.sub (args, 1) of
                  Operand.Const (Const.Word w) =>
                     not (WordX.isZero w) andalso not (WordX.isAllOnes w)
                | _ => false
            fun pure s =
               isMove s
               orelse
               (case s of
                   Statement.PrimApp {args, dst = SOME (Operand.Temporary t),
                                      prim} =>
                      Vector.forall (args, okSrc)
                      andalso (case prim of
                                  CPointer_equal => true
                                | CPointer_lt => true
                                  (* A select of integers is a conditional
                                   * move.  One of reals is slow (it goes
                                   * through the integer registers); a
                                   * chain of them costs more than the
                                   * branches that it replaces.
                                   *)
                                | MLton_bogus =>
                                     not (isReal (Type.toCType
                                                  (Temporary.ty t)))
                                | Real_abs _ => true
                                | Real_add _ => true
                                | Real_equal _ => true
                                | Real_le _ => true
                                | Real_lt _ => true
                                | Real_neg _ => true
                                | Real_qequal _ => true
                                | Real_sub _ => true
                                | Word_add _ => true
                                | Word_andb _ => true
                                | Word_equal _ => true
                                | Word_extdToWord _ => true
                                | Word_lshift _ => true
                                | Word_lt _ => true
                                | Word_mul _ => true
                                | Word_neg _ => true
                                | Word_notb _ => true
                                | Word_orb _ => true
                                | Word_quot _ => okDivisor args
                                | Word_rem _ => okDivisor args
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
            (* the two arms of a switch that go to the same block *)
            fun arms (l, l') =
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
               in
                  if Label.equals (l, l') then NONE
                  else
                     case both of
                        SOME z => SOME z
                      | NONE =>
                           (case (Option.fold (a, NONE, fn (a, _) =>
                                               tri (a, l')),
                                  Option.fold (a', NONE, fn (a', _) =>
                                               tri (a', l))) of
                               (SOME a, _) => SOME (a, plain l')
                             | (_, SOME a') => SOME (plain l, a')
                             | _ => NONE)
               end
            fun hoist (Block.T {kind, label, live, raises, returns, statements,
                                transfer}) =
               case transfer of
                  Transfer.Switch (Switch.T {cases, default, expect = NONE,
                                             test, ...}) =>
                     (case (Vector.toList (Vector.map (cases, #2))
                            @ (case default of NONE => [] | SOME l => [l])) of
                         [l, l'] =>
                            (case arms (l, l') of
                                NONE => ()
                              | SOME z =>
                                   try (z, test,
                                        fn ss =>
                                        Block.T {kind = kind, label = label,
                                                 live = live, raises = raises,
                                                 returns = returns,
                                                 statements = ss,
                                                 transfer = transfer},
                                        Vector.toList statements))
                       | _ => ())
                | _ => ()
            and try (((la, pa, ma, j), (lb, pb, mb, _)), test, mk, ss) =
               let
                  val liveJ = liveTs j
                  fun liveOf l = case l of NONE => liveJ | SOME l => liveTs l
                  val tt = temps test
                  (* What is hoisted from an arm may not define what is
                   * live in the other arm, or what the switch tests.  When
                   * it does, it defines a new temporary instead, and the
                   * arm starts by moving that to the one that it stood for
                   * (if that one is still needed).
                   *)
                  fun rename (pre, ms, other) =
                     let
                        val clash =
                           List.removeDuplicates
                           (List.keepAll (defs pre, fn t =>
                                          mem (other, t) orelse mem (tt, t)),
                            fn (t, t') => mem ([t], t'))
                        val us = uses ms
                        fun needed t = mem (liveJ, t) orelse mem (us, t)
                        val news =
                           List.map (clash, fn t =>
                                     (t, fresh (Temporary.ty t)))
                        fun subst (env, z) =
                           case z of
                              Operand.Cast (z, ty) =>
                                 Operand.Cast (subst (env, z), ty)
                            | Operand.Temporary t =>
                                 (case List.peek (env, fn (t', _) =>
                                                  mem ([t], t')) of
                                     SOME (_, t') => Operand.Temporary t'
                                   | NONE => z)
                            | _ => z
                        fun new t =
                           case List.peek (news, fn (t', _) => mem ([t], t')) of
                              SOME (_, SOME t') => SOME t'
                            | _ => NONE
                     in
                        if List.isEmpty clash then SOME (pre, ms)
                        else if List.exists (news, fn (t, t') =>
                                             not (isSome t')
                                             orelse (needed t
                                                     andalso mem (tt, t)))
                           then NONE
                        else
                           let
                              val (pre, _) =
                                 List.fold
                                 (pre, ([], []), fn (s, (ac, env)) =>
                                  let
                                     fun dst (t, mk) =
                                        case new t of
                                           SOME t' =>
                                              (mk (Operand.Temporary t')
                                               :: ac,
                                               (t, t') :: env)
                                         | NONE =>
                                              (* t stands for itself again *)
                                              (mk (Operand.Temporary t)
                                               :: ac,
                                               List.keepAll
                                               (env, fn (t', _) =>
                                                not (mem ([t], t'))))
                                  in
                                     case s of
                                        Statement.Move
                                        {dst = Operand.Temporary t, src} =>
                                           dst (t, fn d =>
                                                Statement.Move
                                                {dst = d,
                                                 src = subst (env, src)})
                                      | Statement.PrimApp
                                        {args, dst = SOME (Operand.Temporary t),
                                         prim} =>
                                           dst (t, fn d =>
                                                Statement.PrimApp
                                                {args = (Vector.map
                                                         (args, fn z =>
                                                          subst (env, z))),
                                                 dst = SOME d,
                                                 prim = prim})
                                      | _ => (s :: ac, env)
                                  end)
                           in
                              SOME (List.rev pre,
                                    List.keepAllMap
                                    (news, fn (t, t') =>
                                     if needed t
                                        then SOME
                                             (Statement.Move
                                              {dst = Operand.Temporary t,
                                               src = (Operand.Temporary
                                                      (valOf t'))})
                                     else NONE)
                                    @ ms)
                           end
                     end
               in
                  case (rename (pa, ma, liveOf lb), rename (pb, mb, liveOf la)) of
                     (SOME (pa, ma), SOME (pb, mb)) =>
                        try' (((la, pa, ma, j), (lb, pb, mb, j)), test, mk, ss)
                   | _ => ()
               end
            and try' (((la, pa, ma, j), (lb, pb, mb, _)), test, mk, ss) =
               let
                  val (da, db) = (defs pa, defs pb)
                  val d = da @ db
                  val liveJ = liveTs j
                  fun liveOf l = case l of NONE => liveJ | SOME l => liveTs l
                  val tt = temps test
                  val n = List.length pa + List.length pb
                  (* The comparison that the switch tests stays last, if it
                   * can: it is then fused with the selects.
                   *)
                  val (ss, cmp) =
                     case List.rev ss of
                        (s as Statement.PrimApp {dst = SOME z, ...}) :: ss' =>
                           if not (disjoint (temps z, tt))
                              andalso disjoint (d, uses [s])
                              andalso disjoint (tt, uses (pa @ pb))
                              then (List.rev ss', [s])
                           else (ss, [])
                      | _ => (ss, [])
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
                                    (* What was defined in the arm now
                                     * comes into it.
                                     *)
                                    val more =
                                       List.removeDuplicates
                                       (List.keepAll
                                        (defs pre, fn t =>
                                         (mem (us, t) orelse mem (liveJ, t))
                                         andalso not (mem (old, t))),
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
                  if n > 0
                     andalso n <= 6
                     andalso List.length ma + List.length mb <= 4
                     andalso disjoint (d, tt)
                     andalso disjoint (da, liveOf lb)
                     andalso disjoint (db, liveOf la)
                     (* What is hoisted from one arm may define what is
                      * live at the join (the other arm then defines it
                      * too, in its moves); but not from both arms.
                      *)
                     andalso (disjoint (da, liveJ) orelse disjoint (db, liveJ))
                     andalso isSome pre
                     then (shrink (la, pa, ma)
                           ; shrink (lb, pb, mb)
                           ; changed := true
                           ; (case mk (Vector.fromList
                                       (ss @ valOf pre @ cmp)) of
                                 b as Block.T {label, ...} =>
                                    #new (info label) := SOME b))
                  else ()
               end
            fun convert (Block.T {kind, label, live, raises, returns,
                                  statements, transfer}) =
               case transfer of
                  Transfer.Switch (Switch.T {cases, default, expect = NONE,
                                             test, ...}) =>
                     (case switchCases (cases, default) of
                         ([(w, l)], l') =>
                            (case arms (l, l') of
                                SOME ((_, [], ms, j), (_, [], ms', _)) =>
                                   let
                                      val tt = temps test
                                      val ds = defs (ms @ ms')
                                      fun dstOf s =
                                         case s of
                                            Statement.Move {dst, src} =>
                                               (dst, src)
                                          | _ => Error.bug
                                                 "VibeAmd64Analysis.convert"
                                      val ms = List.map (ms, dstOf)
                                      val ms' = List.map (ms', dstOf)
                                      fun sel (dst, a, b) =
                                         select {dst = dst, test = test,
                                                 value = w, a = a, b = b}
                                      (* Both arms set the same
                                       * temporaries, from what neither
                                       * sets: one select for each.
                                       *)
                                      val paired =
                                         List.length ms = List.length ms'
                                         andalso
                                         List.forall2
                                         (ms, ms', fn ((d, a), (d', b)) =>
                                          Operand.equals (d, d')
                                          andalso disjoint (ds, temps a)
                                          andalso disjoint (ds, temps b))
                                      val sels =
                                         if paired
                                            then List.map2
                                                 (ms, ms', fn ((d, a), (_, b)) =>
                                                  sel (d, a, b))
                                         else List.map (ms, fn (d, a) =>
                                                        sel (d, a, d))
                                              @ List.map (ms', fn (d, b) =>
                                                          sel (d, d, b))
                                   in
                                      if List.length ms + List.length ms' <= 4
                                         andalso okSrc test
                                         andalso disjoint (ds, tt)
                                         then (changed := true
                                               ; #new (info label)
                                                 := SOME
                                                    (Block.T
                                                     {kind = kind,
                                                      label = label,
                                                      live = live,
                                                      raises = raises,
                                                      returns = returns,
                                                      statements =
                                                      Vector.concat
                                                      [statements,
                                                       Vector.fromList sels],
                                                      transfer =
                                                      Transfer.Goto j}))
                                      else ()
                                   end
                              | _ => ())
                       | _ => ())
                | _ => ()
            fun target l = Int.inc (#preds (info l))
            fun round () =
               (changed := false
                ; Vector.foreach (blocks, fn Block.T {label, ...} =>
                                  #preds (info label) := 0)
                ; Vector.foreach
                  (blocks, fn Block.T {label, ...} =>
                   case block label of
                      Block.T {transfer, ...} =>
                         (case transfer of
                             Transfer.CCall {return = SOME {return, ...}, ...} =>
                                target return
                           | Transfer.Goto l => target l
                           | Transfer.Spork {cont, spwn, ...} =>
                                (target cont; target spwn)
                           | Transfer.Switch (Switch.T {cases, default, ...}) =>
                                (Vector.foreach (cases, target o #2)
                                 ; Option.app (default, target))
                           | _ => ()))
                ; Vector.foreach (blocks, fn Block.T {label, ...} =>
                                  hoist (block label))
                ; Vector.foreach (blocks, fn Block.T {label, ...} =>
                                  convert (block label))
                ; !changed)
            fun rounds k = if k > 0 andalso round () then rounds (k - 1) else ()
            val () = rounds 6
            (* The arms of the if-converted switches are no longer reached:
             * drop the blocks that can't be reached from an entry block.
             *)
            val {get = reached: Label.t -> bool ref, ...} =
               Property.get (Label.plist, Property.initFun (fn _ => ref false))
            fun visit l =
               if ! (reached l) then ()
               else
                  (reached l := true
                   ; (case block l of
                         Block.T {transfer, ...} =>
                            (case transfer of
                                Transfer.CCall {return = SOME {return, ...},
                                                ...} => visit' return
                              | Transfer.Goto l => visit' l
                              | Transfer.Spork {cont, spwn, ...} =>
                                   (visit' cont; visit' spwn)
                              | Transfer.Switch (Switch.T {cases, default,
                                                           ...}) =>
                                   (Vector.foreach (cases, visit' o #2)
                                    ; Option.app (default, visit'))
                              | _ => ())))
            and visit' l = if ! (reached l) then () else visit l
            val () =
               Vector.foreach
               (blocks, fn Block.T {kind, label, ...} =>
                if Kind.isEntry kind then visit label else ())
         in
            (Vector.keepAllMap (blocks, fn Block.T {label, ...} =>
                                if ! (reached label) then SOME (block label)
                                else NONE),
             block)
         end
      (* Dead code.  The Machine IR has moves to temporaries that are never
       * read (such as the copy, on the way into a loop, of a variable that
       * the loop keeps in its stack slot).  A C compiler drops them; so do
       * we: a backward scan of each block, starting from the temporaries
       * that the backend recorded as live for the successor blocks.
       *)
      val (blocks, labelBlock) =
         let
            val {get = new: Label.t -> Block.t option ref, ...} =
               Property.get (Label.plist, Property.initFun (fn _ => ref NONE))
            fun key t = (Type.toCType (Temporary.ty t), Temporary.index t)
            fun same (t, t') =
               let
                  val (ct, k) = key t
                  val (ct', k') = key t'
               in
                  k = k' andalso CType.equals (ct, ct')
               end
            fun liveOut transfer =
               List.concatMap
               (case transfer of
                   Transfer.CCall {return = SOME {return, ...}, ...} => [return]
                 | Transfer.Goto l => [l]
                 | Transfer.Spork {cont, ...} => [cont]
                 | Transfer.Switch (Switch.T {cases, default, ...}) =>
                      Vector.fold (cases, Option.fold (default, [], op ::),
                                   fn ((_, l), ls) => l :: ls)
                 | _ => [],
                fn l =>
                case labelBlock l of
                   Block.T {kind, live, ...} =>
                      if Kind.isEntry kind then []
                      else Vector.fold (live, [], fn (z, ts) =>
                                        case z of
                                           Live.Temporary t => t :: ts
                                         | _ => ts))
            fun dce (b as Block.T {kind, label, live, raises, returns,
                                   statements, transfer}) =
               let
                  val changed = ref false
                  fun uses (z, live) =
                     let
                        val live = ref live
                     in
                        foreachTemp (z, fn t => List.push (live, t))
                        ; !live
                     end
                  fun isLive (t, live) = List.exists (live, fn t' => same (t, t'))
                  fun kill (t, live) = List.keepAll (live, fn t' => not (same (t, t')))
                  val live0 =
                     Transfer.foldOperands (transfer, liveOut transfer,
                                            fn (z, live) => uses (z, live))
                  val (ss, _) =
                     List.fold
                     (List.rev (Vector.toList statements), ([], live0),
                      fn (s, (ss, live)) =>
                      case s of
                         Statement.Move {dst = Operand.Temporary t, src} =>
                            if isLive (t, live)
                               then (s :: ss, uses (src, kill (t, live)))
                            else (changed := true; (ss, live))
                       | Statement.Move {dst, src} =>
                            (s :: ss, uses (src, uses (dst, live)))
                       | Statement.PrimApp {args, dst, prim} =>
                            let
                               fun keep' (s, live) =
                                  (s :: ss,
                                   Vector.fold (args, live, fn (z, live) =>
                                                uses (z, live)))
                               fun keep live = keep' (s, live)
                               (* the argument that has the type of the
                                * result of a compare-and-swap
                                *)
                               val effect =
                                  case prim of
                                     Prim.Array_cas _ => SOME 2
                                   | Prim.Ref_cas _ => SOME 1
                                   | _ => NONE
                            in
                               case (dst, effect) of
                                  (SOME (Operand.Temporary t), _) =>
                                     if isLive (t, live) orelse isSome effect
                                        then keep (kill (t, live))
                                     else (changed := true; (ss, live))
                                | (SOME z, _) => keep (uses (z, live))
                                | (NONE, SOME k) =>
                                     let
                                        val t =
                                           sink (Operand.ty (Vector.sub (args, k)))
                                     in
                                        changed := true
                                        ; keep' (Statement.PrimApp
                                                 {args = args,
                                                  dst = SOME (Operand.Temporary t),
                                                  prim = prim},
                                                 live)
                                     end
                                | (NONE, NONE) =>
                                     (* the other primitives are pure *)
                                     (changed := true; (ss, live))
                            end)
               in
                  if !changed
                     then Block.T {kind = kind, label = label, live = live,
                                   raises = raises, returns = returns,
                                   statements = Vector.fromList ss,
                                   transfer = transfer}
                  else b
               end
            val blocks =
               Vector.map (blocks, fn b as Block.T {label, ...} =>
                           let
                              val b = dce b
                           in
                              new label := SOME b; b
                           end)
         in
            (blocks, fn l => case ! (new l) of
                                SOME b => b
                              | NONE => labelBlock l)
         end
      (* Load folding.  An x86 instruction can read one of its operands
       * from memory.  So, when a temporary is loaded from memory only to be
       * used once (as an argument of a primitive or of a C call, the source
       * of a move, or the test of a switch), use the memory operand in its
       * place: the temporary goes away, and with it a register and a move.
       *
       * The load moves to its use, past the statements in between.  They
       * may only write temporaries, and not those that the address of the
       * load reads.  (A racy program may see that two loads have changed
       * places, as it may with a C compiler.)
       *)
      val (blocks, labelBlock) =
         let
            val {get = new: Label.t -> Block.t option ref, ...} =
               Property.get (Label.plist, Property.initFun (fn _ => ref NONE))
            fun key t = (Type.toCType (Temporary.ty t), Temporary.index t)
            fun same (t, t') =
               let
                  val (ct, k) = key t
                  val (ct', k') = key t'
               in
                  k = k' andalso CType.equals (ct, ct')
               end
            (* the number of times that z mentions t *)
            fun count (z, t) =
               let
                  val n = ref 0
               in
                  foreachTemp (z, fn t' => if same (t, t') then Int.inc n else ())
                  ; !n
               end
            fun isT (z, t) =
               case stripCast z of
                  Operand.Temporary t' => same (t, t')
                | _ => false
            (* a compare-and-swap *)
            fun writesMemory (p: Type.t Prim.t) =
               case p of
                  Prim.Array_cas _ => true
                | Prim.Ref_cas _ => true
                | _ => false
            fun foldable src =
               case src of
                  Operand.Cast (z, _) => foldable z
                | Operand.Global _ => true
                | Operand.Offset {volatile, ...} => not volatile
                | Operand.SequenceOffset {volatile, ...} => not volatile
                | Operand.StackOffset (StackOffset.T {volatile, ...}) =>
                     not volatile
                | _ => false
            fun liveOut transfer =
               List.concatMap
               (case transfer of
                   Transfer.CCall {return = SOME {return, ...}, ...} => [return]
                 | Transfer.Goto l => [l]
                 | Transfer.Spork {cont, ...} => [cont]
                 | Transfer.Switch (Switch.T {cases, default, ...}) =>
                      Vector.fold (cases, Option.fold (default, [], op ::),
                                   fn ((_, l), ls) => l :: ls)
                 | _ => [],
                fn l =>
                case labelBlock l of
                   Block.T {kind, live, ...} =>
                      if Kind.isEntry kind then []
                      else Vector.fold (live, [], fn (z, ts) =>
                                        case z of
                                           Live.Temporary t => t :: ts
                                         | _ => ts))
            fun stmtCount (s, t) =
               case s of
                  Statement.Move {dst, src} => count (dst, t) + count (src, t)
                | Statement.PrimApp {args, dst, ...} =>
                     Vector.fold (args, case dst of
                                           NONE => 0
                                         | SOME z => count (z, t),
                                  fn (z, n) => n + count (z, t))
            fun transferCount (transfer, t) =
               Transfer.foldOperands (transfer, 0, fn (z, n) => n + count (z, t))
            fun fold (b as Block.T {kind, label, live, raises, returns,
                                    statements, transfer}) =
               let
                  val out = liveOut transfer
                  val changed = ref false
                  (* Replace the one use of t by z, if it is where a memory
                   * operand can be.
                   *)
                  fun subst (z', t, z) =
                     if isT (z', t) then Operand.Cast (z, Temporary.ty t)
                     else z'
                  (* t is loaded from src; ss and transfer follow *)
                  fun try (t, src, ss, transfer) =
                     let
                        val srcTemps =
                           let
                              val ts = ref []
                           in
                              foreachTemp (src, fn t => List.push (ts, t))
                              ; !ts
                           end
                        fun dead (ss, transfer) =
                           List.forall (ss, fn s => stmtCount (s, t) = 0)
                           andalso transferCount (transfer, t) = 0
                           andalso not (List.exists (out, fn t' => same (t, t')))
                        fun loop (pre, ss) =
                           case ss of
                              [] =>
                                 if transferCount (transfer, t) <> 1
                                    orelse List.exists (out, fn t' =>
                                                        same (t, t'))
                                    then NONE
                                 else
                                    (case transfer of
                                        Transfer.CCall {args, func, return} =>
                                           if Vector.exists (args, fn z =>
                                                             isT (z, t))
                                              then SOME
                                                   (List.rev pre,
                                                    Transfer.CCall
                                                    {args = (Vector.map
                                                             (args, fn z =>
                                                              subst (z, t,
                                                                     src))),
                                                     func = func,
                                                     return = return})
                                           else NONE
                                      | Transfer.Switch
                                        (Switch.T {cases, default, expect,
                                                   size, test}) =>
                                           if isT (test, t)
                                              then SOME
                                                   (List.rev pre,
                                                    Transfer.Switch
                                                    (Switch.T
                                                     {cases = cases,
                                                      default = default,
                                                      expect = expect,
                                                      size = size,
                                                      test = subst (test, t,
                                                                    src)}))
                                           else NONE
                                      | _ => NONE)
                            | s :: ss =>
                                 if stmtCount (s, t) = 0
                                    then
                                       (* may the load move past s? *)
                                       let
                                          val ok =
                                             case s of
                                                Statement.Move
                                                {dst = Operand.Temporary t', ...} =>
                                                   not (List.exists
                                                        (srcTemps, fn t'' =>
                                                         same (t', t'')))
                                              | Statement.PrimApp
                                                {dst = SOME (Operand.Temporary t'),
                                                 prim, ...} =>
                                                   not (writesMemory prim)
                                                   andalso
                                                   not (List.exists
                                                        (srcTemps, fn t'' =>
                                                         same (t', t'')))
                                              | _ => false
                                       in
                                          if ok then loop (s :: pre, ss)
                                          else NONE
                                       end
                                 else if stmtCount (s, t) = 1
                                         andalso
                                         (* An operation and its overflow
                                          * check have the same arguments,
                                          * and are output as one.
                                          *)
                                         (case (s, ss) of
                                             (Statement.PrimApp
                                              {args, dst = SOME d, prim},
                                              Statement.PrimApp
                                              {args = args', dst = SOME d',
                                               prim = prim'} :: ss') =>
                                                Vector.equals
                                                (args, args', Operand.equals)
                                                andalso count (d', t) = 0
                                                (* If the two aren't output
                                                 * as one, the second reads
                                                 * the memory operand after
                                                 * the first has written its
                                                 * destination, which the
                                                 * address mustn't read.
                                                 *)
                                                andalso (case (d, d') of
                                                            (Operand.Temporary td,
                                                             Operand.Temporary _) =>
                                                               not (List.exists
                                                                    (srcTemps, fn t'' =>
                                                                     same (td, t'')))
                                                          | _ => false)
                                                andalso List.forall
                                                        ([prim, prim'], fn p =>
                                                         case p of
                                                            Prim.Array_cas _ => false
                                                          | Prim.Ref_cas _ => false
                                                          | Prim.MLton_bogus => false
                                                          | _ => true)
                                                andalso Vector.exists
                                                        (args, fn z => isT (z, t))
                                                andalso dead (ss', transfer)
                                           | _ => false)
                                    then
                                       (case (s, ss) of
                                           (Statement.PrimApp {args, dst, prim},
                                            Statement.PrimApp
                                            {dst = dst', prim = prim', ...}
                                            :: ss') =>
                                              let
                                                 val args =
                                                    Vector.map
                                                    (args, fn z =>
                                                     subst (z, t, src))
                                              in
                                                 SOME
                                                 (List.rev pre
                                                  @ (Statement.PrimApp
                                                     {args = args, dst = dst,
                                                      prim = prim}
                                                     :: Statement.PrimApp
                                                        {args = args,
                                                         dst = dst',
                                                         prim = prim'}
                                                     :: ss'),
                                                  transfer)
                                              end
                                         | _ => NONE)
                                 else if stmtCount (s, t) = 1
                                         andalso dead (ss, transfer)
                                    then
                                       (case s of
                                           Statement.Move {dst, src = z} =>
                                              if isT (z, t)
                                                 then SOME
                                                      (List.rev pre
                                                       @ (Statement.Move
                                                          {dst = dst,
                                                           src = subst (z, t,
                                                                        src)}
                                                          :: ss),
                                                       transfer)
                                              else NONE
                                         | Statement.PrimApp {args, dst, prim} =>
                                              if Vector.exists (args, fn z =>
                                                                isT (z, t))
                                                 andalso
                                                 (case prim of
                                                     (* these read an
                                                      * argument twice
                                                      *)
                                                     Prim.Array_cas _ => false
                                                   | Prim.Ref_cas _ => false
                                                   (* a select *)
                                                   | Prim.MLton_bogus => false
                                                   | _ => true)
                                                 then SOME
                                                      (List.rev pre
                                                       @ (Statement.PrimApp
                                                          {args = (Vector.map
                                                                   (args, fn z =>
                                                                    subst
                                                                    (z, t, src))),
                                                           dst = dst,
                                                           prim = prim}
                                                          :: ss),
                                                       transfer)
                                              else NONE)
                                 else NONE
                     in
                        loop ([], ss)
                     end
                  fun go (ss, transfer) =
                     case ss of
                        [] => ([], transfer)
                      | (s as Statement.Move {dst = Operand.Temporary t, src})
                        :: rest =>
                           (case (if foldable src
                                     then try (t, src, rest, transfer)
                                  else NONE) of
                               SOME (rest, transfer) =>
                                  (changed := true; go (rest, transfer))
                             | NONE =>
                                  let
                                     val (rest, transfer) = go (rest, transfer)
                                  in
                                     (s :: rest, transfer)
                                  end)
                      | s :: rest =>
                           let
                              val (rest, transfer) = go (rest, transfer)
                           in
                              (s :: rest, transfer)
                           end
                  val (ss, transfer) = go (Vector.toList statements, transfer)
               in
                  if !changed
                     then Block.T {kind = kind, label = label, live = live,
                                   raises = raises, returns = returns,
                                   statements = Vector.fromList ss,
                                   transfer = transfer}
                  else b
               end
            val blocks =
               Vector.map (blocks, fn b as Block.T {label, ...} =>
                           let
                              val b = fold b
                           in
                              new label := SOME b; b
                           end)
         in
            (blocks, fn l => case ! (new l) of
                                SOME b => b
                              | NONE => labelBlock l)
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
      (* The loops in which stack slots are promoted (see "promotion", below),
       * and their depths.
       *
       * The loop forest above doesn't see a loop that is re-entered at a
       * block other than its header, as one is after a collection at its
       * limit check: the block that the collector returns to is an entry
       * block, so what follows it looks like a way into the enclosing loop.
       * Here, the graph has only the blocks that a slot can be promoted in
       * (not entry blocks, nor C calls that use the stack), so such a loop
       * is whole.  A cycle of this graph is a nest of loops; it is taken
       * apart by removing the edges into its headers (the blocks that are
       * entered from outside it, or just one block if there is none): the
       * cycles that are left are the loops inside it.  The regions are the
       * cycles, each without the blocks of the cycles inside it.
       *)
      val {get = promoDepth: Label.t -> int ref, ...} =
         Property.get (Label.plist, Property.initFun (fn _ => ref 0))
      val promoRegions: Label.t list list =
         let
            fun transferOf l =
               case labelBlock l of Block.T {transfer, ...} => transfer
            fun good l =
               not (isEntry l)
               andalso (case transferOf l of
                           Transfer.CCall {func, ...} =>
                              not (CFunction.readsStackTop func
                                   orelse CFunction.writesStackTop func)
                         | _ => true)
            fun succs l = List.keepAll (nearSuccessors (transferOf l), good)
            (* which set of blocks a block is in *)
            val {get = setOf: Label.t -> int ref, ...} =
               Property.get (Label.plist, Property.initFun (fn _ => ref 0))
            val sets = ref 0
            fun mark ls =
               (Int.inc sets
                ; List.foreach (ls, fn l => setOf l := !sets)
                ; !sets)
            val {get = preds: Label.t -> Label.t list ref, ...} =
               Property.get (Label.plist, Property.initFun (fn _ => ref []))
            val all =
               Vector.fold (blocks, [], fn (Block.T {label, ...}, ls) =>
                            if good label then label :: ls else ls)
            val () =
               List.foreach (all, fn l =>
                             List.foreach (succs l, fn l' =>
                                           List.push (preds l', l)))
            (* the cycles among the blocks ls, with only the edges that ok
             * allows
             *)
            fun cycles (ls, ok: Label.t -> bool): Label.t list list =
               let
                  val id = mark ls
                  val g = DirectedGraph.new ()
                  val {get = nodeLabel, set = setNodeLabel, ...} =
                     Property.getSetOnce
                     (DirectedGraph.Node.plist, Property.initConst NONE)
                  val {get = node: Label.t -> unit DirectedGraph.Node.t option ref,
                       ...} =
                     Property.get (Label.plist,
                                   Property.initFun (fn _ => ref NONE))
                  val () =
                     List.foreach
                     (ls, fn l =>
                      let
                         val n = DirectedGraph.newNode g
                      in
                         setNodeLabel (n, SOME l); node l := SOME n
                      end)
                  val self = ref []
                  val () =
                     List.foreach
                     (ls, fn l =>
                      List.foreach
                      (succs l, fn l' =>
                       if ! (setOf l') = id andalso ok l'
                          then (if Label.equals (l, l')
                                   then List.push (self, l)
                                else ()
                                ; ignore (DirectedGraph.addEdge
                                          (g, {from = valOf (! (node l)),
                                               to = valOf (! (node l'))})))
                       else ()))
                  val cs =
                     List.keepAllMap
                     (DirectedGraph.stronglyConnectedComponents g, fn ns =>
                      case List.keepAllMap (ns, nodeLabel) of
                         [] => NONE
                       | [l] => if List.exists (!self, fn l' =>
                                                Label.equals (l, l'))
                                   then SOME [l]
                                else NONE
                       | ls => SOME ls)
               in
                  List.foreach (ls, fn l => node l := NONE)
                  ; cs
               end
            fun split (ls, depth): Label.t list list =
               let
                  val id = mark ls
                  val () = List.foreach (ls, fn l => promoDepth l := depth)
                  val headers =
                     case List.keepAll (ls, fn l =>
                                        List.exists (! (preds l), fn l' =>
                                                     ! (setOf l') <> id)) of
                        [] => [hd ls]
                      | hs => hs
                  val {get = isHeader: Label.t -> bool ref, ...} =
                     Property.get (Label.plist,
                                   Property.initFun (fn _ => ref false))
                  val () = List.foreach (headers, fn l => isHeader l := true)
                  val inner = cycles (ls, fn l => not (! (isHeader l)))
                  val () = List.foreach (headers, fn l => isHeader l := false)
                  val id' = mark (List.concat inner)
                  val rest = List.keepAll (ls, fn l => ! (setOf l) <> id')
               in
                  (if List.isEmpty rest then [] else [rest])
                  @ List.concatMap (inner, fn c => split (c, depth + 1))
               end
         in
            List.concatMap (cycles (all, fn _ => true), fn c => split (c, 1))
         end
      (* the weight of a use in a block: by loop depth *)
      fun weightOf l =
         let
            val d = Int.max (List.length (! (#loopPath (labelInfo l))),
                             ! (promoDepth l))
            fun loop (d, w) =
               if d = 0 then w
               else loop (d - 1, if w < 4096 then 8 * w else w)
         in
            loop (d, 1)
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
                       promo = ref false, locals = ref [],
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
             val weight = weightOf label
             val () =
                if Kind.isEntry kind andalso not (List.isEmpty (liveTemps b))
                   then Error.bug "VibeAmd64Codegen: temporary live at entry"
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
            val regions = promoRegions
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
                                val w = weightOf l
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
                            (* those that the loop defines or uses *)
                            val used =
                               List.fold
                               (bs, [], fn (l, ac) =>
                                List.fold
                                (! (#events (blockWebs l)), ac, fn (ev, ac) =>
                                 case ev of
                                    Def (w, _) => w :: ac
                                  | Use w => w :: ac))
                            val ps =
                               List.map
                               (candidates, fn (k as (_, ct), n) =>
                                let
                                   val w as W {count, across, promo, locals,
                                               ...} =
                                      newWeb (ct, false)
                                in
                                   promo := true
                                   ; locals := used
                                   ; count := !n
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
               (nearSuccessors transfer, fn l =>
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
                        >= (if isReal ct then 14 else 9))
                then ()
             else union (w, w')
          end)
      (* 5. Color, in order of use count.  Prefer the register of a web that
       * this one is copied to or from.
       *
       * The temporaries come before the promoted stack slots.  A promoted
       * slot is live throughout its loop (which may be a whole nest of
       * loops), so it takes a register from every temporary there; and
       * there are few registers.  A slot that stays in memory costs little
       * (an instruction can read it where it is), but a temporary in the
       * frame costs a store and a load.  So, a promoted slot gets a
       * register only if no temporary of its loop wants it.
       *)
      val () =
         List.foreach
         (QuickSort.sortList
          (distinct (!webs), fn (W {count, promo, ...},
                                 W {count = count', promo = promo', ...}) =>
           if !promo = !promo' then !count >= !count'
           else not (!promo)),
          fn W {ct, count, across, adj, aff, noReg, home, promo, locals, ...} =>
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
                (* A promoted slot that found no register may take one
                 * from temporaries that are only live through its loop
                 * (the loop neither defines nor uses them), if they are
                 * used less than it is: they then live in the frame.
                 *)
                fun evict () =
                   let
                      val () = List.foreach (!locals, fn w =>
                                             let
                                                val W {live, ...} = find w
                                             in
                                                live := true
                                             end)
                      val adjs = distinct (!adj)
                      val best =
                         List.fold
                         (regs, NONE, fn (r, best) =>
                          let
                             val blockers =
                                List.keepAll (adjs, fn W {home, ...} =>
                                              !home = SOME r)
                             val cost =
                                List.fold (blockers, 0, fn (W {count, ...}, n) =>
                                           n + !count)
                          in
                             if List.exists (blockers, fn W {promo, live, ...} =>
                                             !promo orelse !live)
                                orelse 2 * cost > !count
                                then best
                             else (case best of
                                      SOME (_, _, cost') =>
                                         if cost < cost'
                                            then SOME (r, blockers, cost)
                                         else best
                                    | NONE => SOME (r, blockers, cost))
                          end)
                      val () = List.foreach (!locals, fn w =>
                                             let
                                                val W {live, ...} = find w
                                             in
                                                live := false
                                             end)
                   in
                      case best of
                         NONE => NONE
                       | SOME (r, blockers, _) =>
                            (List.foreach (blockers, fn W {home, ...} =>
                                           home := NONE)
                             ; SOME r)
                   end
             in
                home := (case partner of
                            SOME r => SOME r
                          | NONE =>
                               (case List.peek (regs, free) of
                                   SOME r => SOME r
                                 | NONE =>
                                      if !promo
                                         then evict ()
                                      else NONE))
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
       liveTemps = liveTemps,
       loopDistance = loopDistance,
       nearSuccessors = nearSuccessors,
       segIn = fn (l, e) => segIn (l, e, false)}
   end

end

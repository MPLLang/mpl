(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor VibeArmAccessLocal (S: VIBE_ARM_ACCESS_LOCAL_STRUCTS): VIBE_ARM_ACCESS_LOCAL =
struct

open S
structure Emit = Access.Emit
structure Items = Access.Items
structure Analysis = Items.Analysis
structure Machine = Emit.Machine
open Machine

structure Arm64 = Emit.Arm64
structure I = Arm64.Instr
structure Lbl = Arm64.Label
structure R = Arm64.Reg

structure Region = Analysis.Region
structure Temp = Analysis.Temp
structure Web = Analysis.Web
val caller = Analysis.caller
val callee = Analysis.callee
val fcaller = Analysis.fcaller
val fcallee = Analysis.fcallee
val callerSave = Analysis.callerSave
val isReal = Analysis.isReal
val sameSlot = Analysis.sameSlot
val size = Analysis.size
val slotKey = Analysis.slotKey
val stripCast = Analysis.stripCast

val x = Emit.x
val scratch = Emit.scratch

val cty = Access.cty

datatype item = datatype Items.t

(* What a register can hold. *)
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

fun new ({analysis, emit = e, labelIndex, tempsMax}: Access.args): Access.t =
   let
      val {blockRegion, blockWebs, ifConvert, labelBlock, liveTemps,
           nearSuccessors, ...} = analysis
      val {emit, ldr, str, mem, ...} = e
      val {frameSize, homeOf, setSpAdj, ...} =
         Access.frame {emit = e, tempsMax = tempsMax}
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
      (* the region of the block that is being output *)
      val region = ref (Region.new ())
      (* the promoted stack slots of the block *)
      val curPromoted: (Analysis.slot * Web.t) list ref = ref []
      (* the block that is being output *)
      val curLabel = ref (Label.newNoname ())
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
      (* forget which registers hold stack slots *)
      fun clearCache () =
         regs := List.keepAll (!regs, fn {loc, ...} => isT loc)
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
                           NONE => Error.bug "VibeArmAccess.allocReg"
                         | SOME (e as {reg, loc, ...}, _) =>
                              (if nextUse loc <= never then commitEntry e else ()
                               ; dropReg reg
                               ; reg)
                     end
         in
            pin r; r
         end
      (* Load a location into a register, if it is worth it. *)
      fun cacheIn z =
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
      fun pinnedReg (e: rentry option) =
         case e of
            NONE => NONE
          | SOME {reg, ...} => (pin reg; SOME reg)
      (* the register that an operand is in *)
      fun home z =
         case z of
            Operand.Cast (z, _) => home z
          | Operand.Frontier => SOME 21
          | Operand.GCState => SOME 19
          | Operand.StackOffset so => pinnedReg (inReg (S (slotKey so)))
          | Operand.StackTop => SOME 20
          | Operand.Temporary t => pinnedReg (inReg (tloc t))
          | _ => NONE
      val {value, load, memOf, setReg} =
         Access.operands {emit = e, labelIndex = labelIndex, homeOf = homeOf,
                          home = home, cacheIn = cacheIn}
      fun store (dst, r) =
         let
            val ct = cty dst
            fun set h = setReg (ct, h, r)
         in
            case (stripCast dst, home dst) of
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
                        regs := List.keepAll (!regs, fn {loc, ...} =>
                                              case loc of
                                                 S k' => not (other k')
                                               | T _ => true)
                  in
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
                  end
             | (Operand.Temporary t, _) =>
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
             | (_, SOME h) => set h
             | (_, NONE) =>
                  str (ct, r, memOf (dst, 3, ct))
         end
      (* A register in which to compute the value of dst. *)
      fun dstReg dst =
         let
            val narrow = size (cty dst) < 4
         in
            case stripCast dst of
               Operand.Temporary t =>
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
             | _ =>
                  (case home dst of
                      SOME h => if narrow then scratch (cty dst, 0) else h
                    | NONE => scratch (cty dst, 0))
         end
      fun move {dst, src} =
         let
            fun default () = store (dst, value (src, dstReg dst, 3))
         in
            case (stripCast dst, stripCast src) of
               (Operand.Temporary td, Operand.Temporary ts) =>
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
      (* Set dst with the instruction that set outputs, given a register
       * that holds dst's value.
       *)
      fun setDst (dst, _: bool, set) =
         let
            val d = value (dst, scratch (cty dst, 0), 3)
         in
            set d
            ; store (dst, d)
         end

      (* Before a C call: write back the caller-save registers that hold
       * live temporaries, and forget what the caller-save registers hold.
       *)
      fun commitCallerSaves live =
         List.foreach (!regs, fn e as {reg, loc, ...} =>
                       if callerSave reg andalso live loc
                          then commitEntry e
                       else ())
      fun dropCallerSaves () =
         regs := List.keepAll (!regs, fn {reg, ...} =>
                               not (callerSave reg))
      fun flushCallerSaves live = (commitCallerSaves live; dropCallerSaves ())
      (* Live transfers: what registers hold at the start
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
      (* Before going to l at the end of a block. *)
      fun beforeGoto l =
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
      (* Find where each temporary and stack slot occurs
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
      fun beginItem k = (pos := k; pinned := []; reserved := [])
      (* after an item, drop what is dead *)
      fun endItem () =
         (pos := !pos + 1
          ; pinned := []
          ; reserved := []
          ; regs := List.keepAll
                    (!regs, fn {loc, ...} =>
                     isLiveOut loc
                     orelse List.exists (occsOf loc, fn p =>
                                         p >= !pos)))
      fun enterBlock l = curPromoted := #promoted (blockWebs l)
      fun startBlock (b as Block.T {label = l, ...}) =
         (curLabel := l
          ; region := blockRegion l
          ; enterBlock l
          ; setupLocal (b, Items.ofBlock (analysis, b)))
      fun saveForCall (return: Label.t option) =
         let
            val live = case return of
                          NONE => []
                        | SOME return => liveIn return
         in
            commitCallerSaves (fn l => member (live, l))
            (* a temporary is reloaded from its home when it is next used *)
            ; {restore = fn () => ()}
         end
      fun afterCall () = (clearCache (); dropCallerSaves ())
      fun callRuntime name =
         (flushCallerSaves (fn _ => true)
          ; emit (I.Bl (Lbl.Sym name))
          ; clearCache ())
      fun edgeCode l =
         let
            val state =
               List.map (!regs, fn {reg, loc, dirty} =>
                         {reg = reg, loc = loc, dirty = ref (!dirty)})
         in
            fn () =>
            (regs := state
             ; pinned := []
             ; conform l)
         end
   in
      {frameSize = frameSize,
       startBlock = startBlock,
       curBlock = fn () => !curLabel,
       enterBlock = enterBlock,
       beginItem = beginItem,
       endItem = endItem,
       (* nothing is kept per definition: see setupLocal *)
       defined = fn _ => (),
       value = value,
       load = load,
       dstReg = dstReg,
       store = store,
       move = move,
       setDst = setDst,
       clearCache = clearCache,
       noCache = clearCache,
       setSpAdj = setSpAdj,
       saveForCall = saveForCall,
       afterCall = afterCall,
       callRuntime = callRuntime,
       edgeNeedsCode = edgeNeedsCode,
       beforeGoto = beforeGoto,
       edgeCode = edgeCode}
   end

end

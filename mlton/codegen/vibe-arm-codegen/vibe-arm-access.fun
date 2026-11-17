(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor VibeArmAccess (S: VIBE_ARM_ACCESS_STRUCTS): VIBE_ARM_ACCESS =
struct

open S
structure Analysis = Items.Analysis
structure Machine = Emit.Machine
open Machine

structure Arm64 = Emit.Arm64
structure Addr = Arm64.Addr
structure I = Arm64.Instr
structure Lbl = Arm64.Label
structure Opnd = Arm64.Operand
structure R = Arm64.Reg

structure Region = Analysis.Region
structure Temp = Analysis.Temp
structure Web = Analysis.Web
val caller = Analysis.caller
val callee = Analysis.callee
val fcaller = Analysis.fcaller
val fcallee = Analysis.fcallee
val callerSave = Analysis.callerSave
val dstTemp = Analysis.dstTemp
val isReal = Analysis.isReal
val sameSlot = Analysis.sameSlot
val size = Analysis.size
val slotKey = Analysis.slotKey

val x = Emit.x
val reg = Emit.reg
val immOp = Emit.immOp
val scratch = Emit.scratch

datatype item = datatype Items.t

fun cty z = Type.toCType (Operand.ty z)

(* (Local allocator.)  What a register can hold. *)
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
type regs = rentry list

type t =
   {frameSize: int,
    startBlock: Block.t -> unit,
    curBlock: unit -> Label.t,
    enterBlock: Label.t -> unit,
    beginItem: int -> unit,
    endItem: unit -> unit,
    defined: Operand.t -> unit,
    value: Operand.t * int * int -> int,
    load: Operand.t * int * int -> unit,
    memOf: Operand.t * int * CType.t -> Addr.t,
    dstReg: Operand.t -> int,
    defHome: Temporary.t -> int option,
    store: Operand.t * int -> unit,
    move: {dst: Operand.t, src: Operand.t} -> unit,
    clearCache: unit -> unit,
    noCache: unit -> unit,
    setSpAdj: int -> unit,
    saveForCall: Label.t option -> {restore: unit -> unit},
    afterCall: unit -> unit,
    callRuntime: string -> unit,
    edgeNeedsCode: Label.t -> bool,
    beforeGoto: Label.t -> unit,
    copyRegs: unit -> regs,
    conformFrom: regs * Label.t -> unit}

fun new {analysis: Analysis.t,
         emit = e: Emit.t,
         labelIndex: Label.t -> int,
         localRA: bool,
         tempsMax: CType.t -> int}: t =
   let
      val {blockRegion, blockWebs, ifConvert, labelBlock, liveTemps,
           nearSuccessors, segIn, ...} = analysis
      val {emit, ldr, str, mov, movImm, movInt, addImm, mem, symAddr, ...} = e

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
                           NONE => Error.bug "VibeArmAccess.allocReg"
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
                              then Error.bug "VibeArmAccess.addr: nesting"
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
          | _ => Error.bug (concat ["VibeArmAccess.addr: ", Operand.toString z])
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
                             NONE => Error.bug "VibeArmAccess.load: Real"
                           | SOME w =>
                                (movImm (17, WordX.toIntInf w)
                                 ; emit (I.Un (I.FMOV, reg (d, ct),
                                               reg (17, ct)))))
                    | Const.Word w => movImm (d, WordX.toIntInf w)
                    | _ => Error.bug "VibeArmAccess.load: Const")
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
      (* (Local allocator.)  Before going to l at the end of a block. *)
      fun beforeGoto l =
         if localRA
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
      fun startBlock (b as Block.T {label = l, ...}) =
         (curLabel := l
          ; region := blockRegion l
          ; enterBlock l
          ; setupCache b
          ; if localRA
               then setupLocal (b, Items.ofBlock (analysis, b))
            else ())
      fun noCache () = (clearCache (); cacheable := [])
      fun saveForCall (return: Label.t option) =
         let
            (* Commit the caller-save temporaries that are live across the
             * call.
             *)
            val saves =
               case (localRA, return) of
                  (false, SOME return) =>
                     callerSaves (List.map (liveTemps (labelBlock return),
                                            fn e => segIn (return, e))
                                  @ List.map (!curPromoted, #2))
                | _ => []
            val () = commit (str, saves)
            val () =
               let
                  val live = case return of
                                NONE => []
                              | SOME return => liveIn return
               in
                  commitCallerSaves (fn l => member (live, l))
               end
         in
            {restore = fn () => commit (ldr, saves)}
         end
      fun afterCall () = (clearCache (); dropCallerSaves ())
      fun callRuntime name =
         let
            (* We don't know what is live here, so commit all of the
             * caller-save temporaries.
             *)
            val saves =
               if localRA then []
               else callerSaves (List.map (!curWebs, #2)
                                 @ List.map (!curPromoted, #2))
         in
            commit (str, saves)
            ; flushCallerSaves (fn _ => true)
            ; emit (I.Bl (Lbl.Sym name))
            ; clearCache ()
            ; commit (ldr, saves)
         end
      fun copyRegs () =
         List.map (!regs, fn {reg, loc, dirty} =>
                   {reg = reg, loc = loc, dirty = ref (!dirty)})
      fun conformFrom (state, l) =
         (regs := state
          ; pinned := []
          ; conform l)
   in
      {frameSize = frameSize,
       startBlock = startBlock,
       curBlock = fn () => !curLabel,
       enterBlock = enterBlock,
       beginItem = beginItem,
       endItem = endItem,
       defined = defined,
       value = value,
       load = load,
       memOf = memOf,
       dstReg = dstReg,
       defHome = defHome,
       store = store,
       move = move,
       clearCache = clearCache,
       noCache = noCache,
       setSpAdj = fn n => spAdj := n,
       saveForCall = saveForCall,
       afterCall = afterCall,
       callRuntime = callRuntime,
       edgeNeedsCode = fn l => localRA andalso edgeNeedsCode l,
       beforeGoto = beforeGoto,
       copyRegs = copyRegs,
       conformFrom = conformFrom}
   end

end

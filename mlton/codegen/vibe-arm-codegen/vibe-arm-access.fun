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
val stripCast = Analysis.stripCast

val x = Emit.x
val reg = Emit.reg
val immOp = Emit.immOp
val scratch = Emit.scratch

fun cty z = Type.toCType (Operand.ty z)

type t =
   {frameSize: int,
    startBlock: Block.t -> unit,
    curBlock: unit -> Label.t,
    enterBlock: Label.t -> unit,
    defined: Operand.t -> unit,
    value: Operand.t * int * int -> int,
    load: Operand.t * int * int -> unit,
    dstReg: Operand.t -> int,
    store: Operand.t * int -> unit,
    move: {dst: Operand.t, src: Operand.t} -> unit,
    setDst: Operand.t * bool * (int -> unit) -> unit,
    clearCache: unit -> unit,
    noCache: unit -> unit,
    setSpAdj: int -> unit,
    saveForCall: Label.t option -> {restore: unit -> unit},
    afterCall: unit -> unit,
    callRuntime: string -> unit}

fun new {analysis: Analysis.t,
         emit = e: Emit.t,
         labelIndex: Label.t -> int,
         tempsMax: CType.t -> int}: t =
   let
      val {blockRegion, blockWebs, labelBlock, liveTemps, segIn, ...} = analysis
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
      fun homeOf (ct, n) = (31, !spAdj + slot ct + 8 * n)
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
      fun clearCache () = cache := []
      (* Load a stack slot into a register of the cache, if it is worth it. *)
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
                        (ldr (ct, r, mem (20, off, ct))
                         ; List.push (cache, {off = off, ct = ct, reg = r})
                         ; SOME r)
               end
          | _ => NONE
      (* the register that an operand is in *)
      fun home z =
         case z of
            Operand.Cast (z, _) => home z
          | Operand.Frontier => SOME 21
          | Operand.GCState => SOME 19
          | Operand.StackOffset so =>
               (case promotedHome so of
                   SOME r => SOME r
                 | NONE => Option.map (cached so, #reg))
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
               homeOf (cty z, Temporary.index tmp)
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
      (* set a register home, keeping narrow values zero-extended *)
      fun setReg (ct, h, r) =
         case size ct of
            1 => emit (I.Bin (I.AND, R.W h, R.W r, immOp 0xff))
          | 2 => emit (I.Bin (I.AND, R.W h, R.W r, immOp 0xffff))
          | _ => mov (h, r, ct)
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
                        cache := List.keepAll (!cache, fn {off, ct, ...} =>
                                               not (other (off, ct)))
                  in
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
                  (case defHome t of
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
            val narrow = size (cty dst) < 4
         in
            case stripCast dst of
               Operand.Temporary t =>
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
            case (case stripCast dst of
                     Operand.Temporary t => defHome t
                   | _ => NONE) of
               SOME h =>
                  if size (cty dst) < 4
                     andalso size (cty src) = size (cty dst)
                     andalso inMemory (stripCast src)
                     andalso not (isSome (home src))
                     andalso not (isSome (cacheIn src))
                     then load (src, h, 3)
                  else default ()
             | NONE => default ()
         end
      (* Set dst with the instruction that set outputs, given dst's
       * register (which holds dst's value, if old).
       *)
      fun setDst (dst, old, set) =
         let
            val ct = cty dst
            val (d, spilled) =
               case Option.fold (dstTemp dst, NONE, fn (t, _) =>
                                 defHome t) of
                  SOME h => (h, false)
                | NONE => (scratch (ct, 0), true)
         in
            (* The old value of a spilled destination is in its slot
             * (not where the block finds the temporary on entry,
             * which is what load would read).
             *)
            if spilled andalso old
               then ldr (ct, d, memOf (dst, 3, ct))
            else ()
            ; set d
            ; if spilled then store (dst, d) else ()
            ; defined dst
         end
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
      fun startBlock (b as Block.T {label = l, ...}) =
         (curLabel := l
          ; region := blockRegion l
          ; enterBlock l
          ; setupCache b)
      fun noCache () = (clearCache (); cacheable := [])
      fun saveForCall (return: Label.t option) =
         let
            (* Commit the caller-save temporaries that are live across the
             * call.
             *)
            val saves =
               case return of
                  SOME return =>
                     callerSaves (List.map (liveTemps (labelBlock return),
                                            fn e => segIn (return, e))
                                  @ List.map (!curPromoted, #2))
                | NONE => []
            val () = commit (str, saves)
         in
            {restore = fn () => commit (ldr, saves)}
         end
      fun callRuntime name =
         let
            (* We don't know what is live here, so commit all of the
             * caller-save temporaries.
             *)
            val saves =
               callerSaves (List.map (!curWebs, #2)
                            @ List.map (!curPromoted, #2))
         in
            commit (str, saves)
            ; emit (I.Bl (Lbl.Sym name))
            ; clearCache ()
            ; commit (ldr, saves)
         end
   in
      {frameSize = frameSize,
       startBlock = startBlock,
       curBlock = fn () => !curLabel,
       enterBlock = enterBlock,
       defined = defined,
       value = value,
       load = load,
       dstReg = dstReg,
       store = store,
       move = move,
       setDst = setDst,
       clearCache = clearCache,
       noCache = noCache,
       setSpAdj = fn n => spAdj := n,
       saveForCall = saveForCall,
       afterCall = clearCache,
       callRuntime = callRuntime}
   end

end

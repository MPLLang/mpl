(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor VibeAmd64Access (S: VIBE_AMD64_ACCESS_STRUCTS): VIBE_AMD64_ACCESS =
struct

open S
structure Analysis = Items.Analysis
structure Machine = Emit.Machine
open Machine

structure Asm = Emit.Asm
structure Addr = Asm.Addr
structure I = Asm.Instr
structure Lbl = Asm.Label
structure Opnd = Asm.Operand
structure Size = Asm.Size

structure Region = Analysis.Region
structure Temp = Analysis.Temp
structure Web = Analysis.Web
val caller = Analysis.caller
val callee = Analysis.callee
val fcaller = Analysis.fcaller
val fcallee = Analysis.fcallee
val callerSave = Analysis.callerSave
val dstTemp = Analysis.dstTemp
val sameSlot = Analysis.sameSlot
val slotKey = Analysis.slotKey
val stripCast = Analysis.stripCast

val rax = Emit.rax
val rcx = Emit.rcx
val rdx = Emit.rdx
val rsp = Emit.rsp
val isReal = Emit.isReal
val size = Emit.size
val sz = Emit.sz
val reg = Emit.reg
val based = Emit.based
val pow2 = Emit.pow2
val simm32 = Emit.simm32

fun cty z = Type.toCType (Operand.ty z)

type frame = {outArgs: int, shuffle: int}

(* the number of registers that are saved around calls *)
val numSaves = List.length caller + List.length fcaller
fun tempBase ({outArgs, shuffle}: frame) =
   outArgs + 16 + 8 * shuffle + 8 * numSaves
fun frameNeed {frame, tempsMax} =
   List.fold (CType.all, tempBase frame, fn (t, off) =>
              off + 8 * (1 + tempsMax t + Analysis.extraTemps))

type t =
   {startBlock: Block.t -> unit,
    curBlock: unit -> Label.t,
    enterBlock: Label.t -> unit,
    defined: Operand.t -> unit,
    home: Operand.t -> int option,
    reads: Operand.t -> int list,
    needsScratch: Operand.t -> bool,
    immOf: Operand.t -> IntInf.t option,
    opnd: Operand.t * int list * {imm: bool} -> Opnd.t,
    addr: Operand.t * int list -> Addr.t,
    regOf: Operand.t * int * int list -> int,
    load: Operand.t * int * int list -> unit,
    dstReg: Operand.t -> int option,
    store: Operand.t * int * {clean: bool} -> unit,
    move: {dst: Operand.t, src: Operand.t} -> unit,
    setDst: Operand.t * bool * (int -> unit) -> unit,
    clearCache: unit -> unit,
    noCache: unit -> unit,
    resInt: Addr.t,
    resReal: Addr.t,
    shuffle: int -> Addr.t,
    saveForCall: Label.t option -> {restore: unit -> unit},
    afterCall: unit -> unit,
    callRuntime: string * (unit -> unit) -> unit}

fun new {analysis: Analysis.t,
         emit = e: Emit.t,
         frame as {outArgs, ...}: frame,
         labelIndex: Label.t -> int,
         tempsMax: CType.t -> int}: t =
   let
      val {blockRegion, blockWebs, labelBlock, liveTemps, segIn, ...} = analysis
      val {emit, ins, ld, st, mov, movImm, lea, flagsLive, literal, symAddr,
           ...} = e

      (* the frame *)
      val resInt = based (rsp, outArgs)
      val resReal = based (rsp, outArgs + 8)
      fun shuffle k = based (rsp, outArgs + 16 + 8 * k)
      val saveBase = outArgs + 16 + 8 * #shuffle frame
      val (slots, _) =
         List.fold (CType.all, ([], tempBase frame), fn (t, (slots, off)) =>
                    ((t, off) :: slots, off + 8 * (1 + tempsMax t + Analysis.extraTemps)))
      fun slot ct =
         #2 (valOf (List.peek (slots, fn (t, _) => CType.equals (t, ct))))
      fun tempAddr (ct, n) = based (rsp, slot ct + 8 * n)
      (* store/load registers to/from the save area of the frame *)
      fun commit (f, rs) =
         List.foreach
         (rs, fn r =>
          let
             val ct = if r >= 16 then CType.Real64 else CType.Word64
             val k = if r >= 16 then List.length caller + (r - 18) else r - 6
          in
             f (ct, r, based (rsp, saveBase + 8 * k))
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
                            (r >= 16) = isReal ct
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
                        (ld (ct, r, based (Emit.stackTop, off))
                         ; List.push (cache, {off = off, ct = ct, reg = r})
                         ; SOME r)
               end
          | _ => NONE
      (* the register that an operand is in *)
      fun home z =
         case z of
            Operand.Cast (z, _) => home z
          | Operand.Frontier => SOME Emit.frontier
          | Operand.GCState => SOME Emit.gcState
          | Operand.StackOffset so =>
               (case promotedHome so of
                   SOME r => SOME r
                 | NONE => Option.map (cached so, #reg))
          | Operand.StackTop => SOME Emit.stackTop
          | Operand.Temporary t => tempHome t
          | _ => NONE
      (* the same, after caching it if it is worth it *)
      fun regHome z =
         case home z of
            SOME h => SOME h
          | NONE => cacheIn z
      fun homes z = case home z of SOME h => [h] | NONE => []
      fun constWord z =
         case z of
            Operand.Cast (z, _) => constWord z
          | Operand.Const (Const.Word w) => SOME w
          | _ => NONE
      fun reads z =
         case z of
            Operand.Address z => reads z
          | Operand.Cast (z, _) => reads z
          | Operand.Offset {base, ...} => homes base
          | Operand.SequenceOffset {base, index, ...} =>
               homes base @ homes index
          | _ => homes z
      fun needsScratch z =
         case z of
            Operand.Address z => needsScratch z
          | Operand.Cast (z, _) => needsScratch z
          | Operand.Offset {base, ...} =>
               (case stripCast base of
                   Operand.StaticHeapRef _ => false
                 | _ => not (isSome (home base)))
          | Operand.SequenceOffset {base, index, ...} =>
               not (isSome (home base))
               orelse not (isSome (home index)
                           orelse isSome (constWord index))
          | _ => false
      fun immOf z =
         let
            val ct = cty z
         in
            case stripCast z of
               Operand.Const (Const.Word w) =>
                  let
                     val n = WordX.toIntInfX w
                  in
                     if size ct < 8 orelse simm32 n then SOME n else NONE
                  end
             | Operand.Const Const.Null => SOME 0
             | Operand.Label l => SOME (IntInf.fromInt (labelIndex l))
             | _ => NONE
         end
      fun realBits r =
         case RealX.castToWord r of
            NONE => Error.bug "VibeAmd64Access.realBits"
          | SOME w => WordX.toIntInf w
      (* An operand that is in memory and that needs no register to
       * address.
       *)
      fun simpleMem z: Addr.t option =
         case z of
            Operand.Cast (z, _) => simpleMem z
          | Operand.Global g =>
               let
                  val ct = Type.toCType (Global.ty g)
               in
                  SOME (Addr.Rip (Lbl.Sym ("global" ^ CType.toString ct),
                                  size ct * Global.index g))
               end
          | Operand.StackOffset (StackOffset.T {offset, ...}) =>
               SOME (based (Emit.stackTop, Bytes.toInt offset))
          | Operand.Temporary tmp =>
               SOME (tempAddr (cty z, Temporary.index tmp))
          | _ => NONE
      (* The address of a memory operand.  May clobber the registers scr. *)
      fun addr (z, scr: int list): Addr.t =
         let
            fun scratch () =
               case scr of
                  r :: _ => r
                | [] => Error.bug (concat ["VibeAmd64Access.addr: no scratch: ",
                                           Operand.toString z])
            (* a register that holds the value of z: where it lives, or
             * the one that r gives
             *)
            fun inReg (z, r: unit -> int) =
               case regHome z of
                  SOME h => h
                | NONE => let
                             val r = r ()
                          in
                             load (z, r, [r]); r
                          end
         in
            case z of
               Operand.Cast (z, _) => addr (z, scr)
             | Operand.Offset {base, offset, ...} =>
                  (case stripCast base of
                      Operand.StaticHeapRef (StaticHeap.Ref.T
                                             {kind, offset = offset', ...}) =>
                         Addr.Rip (Lbl.Sym ("staticHeap"
                                            ^ StaticHeap.Kind.name kind),
                                   Bytes.toInt offset' + Bytes.toInt offset)
                    | _ =>
                         (case regHome base of
                             SOME h => based (h, Bytes.toInt offset)
                           | NONE => based (inReg (base, scratch),
                                            Bytes.toInt offset)))
             | Operand.SequenceOffset {base, index, offset, scale, ...} =>
                  let
                     val n = Bytes.toInt (Scale.toBytes scale)
                     val off = Bytes.toInt offset
                     fun mk (b, i) =
                        Addr.T {base = SOME b, index = SOME (i, n), disp = off}
                  in
                     case constWord index of
                        SOME w =>
                           based (inReg (base, scratch),
                                  off + n * IntInf.toInt (WordX.toIntInf w))
                      | NONE =>
                           (case (regHome base, regHome index) of
                               (SOME b, SOME i) => mk (b, i)
                             | (SOME b, NONE) =>
                                  mk (b, inReg (index, scratch))
                             | (NONE, SOME i) =>
                                  mk (inReg (base, scratch), i)
                             | (NONE, NONE) =>
                                  (case scr of
                                      r :: r' :: _ =>
                                         let
                                            val b = inReg (base, fn () => r)
                                         in
                                            mk (b, inReg (index, fn () => r'))
                                         end
                                    | [r] =>
                                         (* with one register: compute
                                          * base + index * scale in it
                                          *)
                                         (case simpleMem base of
                                             NONE =>
                                                Error.bug
                                                "VibeAmd64Access.addr: nesting"
                                           | SOME a =>
                                                (if !flagsLive
                                                    then Error.bug
                                                         "VibeAmd64Access.addr: flags"
                                                 else ()
                                                 ; load (index, r, [r])
                                                 ; if n > 1
                                                      then ins
                                                           ("shlq",
                                                            [Opnd.Imm
                                                             (case n of
                                                                 2 => 1
                                                               | 4 => 2
                                                               | _ => 3),
                                                             Opnd.Reg (r, Size.Q)])
                                                   else ()
                                                 ; ins ("addq",
                                                        [Opnd.Mem a,
                                                         Opnd.Reg (r, Size.Q)])
                                                 ; based (r, off)))
                                    | [] => based (scratch (), 0)))
                  end
             | _ =>
                  (case simpleMem z of
                      SOME a => a
                    | NONE =>
                         Error.bug (concat ["VibeAmd64Access.addr: ",
                                            Operand.toString z]))
         end
      (* Put the value of z in register d. May clobber the registers scr. *)
      and load (z, d, scr): unit =
         let
            val ct = cty z
         in
            case z of
               Operand.Address z => lea (addr (z, scr), d)
             | Operand.Cast (z, _) => load (z, d, scr)
             | Operand.Const c =>
                  (case c of
                      Const.CSymbol (CSymbol.T {name, ...}) =>
                         symAddr (d, name, {got = true})
                    | Const.Null => movImm (d, 0)
                    | Const.Real r =>
                         let
                            val bits = realBits r
                         in
                            if d < 16 then movImm (d, bits)
                            else if bits = 0
                               then ins ("xorps", [Opnd.Reg (d, Size.Q),
                                                   Opnd.Reg (d, Size.Q)])
                            else ld (ct, d, literal (size ct, bits))
                         end
                    | Const.Word w => movImm (d, WordX.toIntInf w)
                    | _ => Error.bug "VibeAmd64Access.load: Const")
             | Operand.Label l => movImm (d, IntInf.fromInt (labelIndex l))
             | Operand.StaticHeapRef (StaticHeap.Ref.T {kind, offset, ...}) =>
                  lea (Addr.Rip (Lbl.Sym ("staticHeap"
                                          ^ StaticHeap.Kind.name kind),
                                 Bytes.toInt offset),
                       d)
             | _ =>
                  (case regHome z of
                      SOME h => mov (d, h, ct)
                    | NONE => ld (ct, d, addr (z, scr)))
         end
      fun regOf (z, d, scr): int =
         case regHome z of
            SOME h => h
          | NONE => (load (z, d, scr); d)
      fun opnd (z, scr, {imm}): Opnd.t =
         let
            val ct = cty z
            fun materialize () =
               case scr of
                  r :: _ => (load (z, r, scr); reg (r, ct))
                | [] => Error.bug "VibeAmd64Access.opnd: no scratch"
         in
            case (imm, immOf z) of
               (true, SOME n) => Opnd.Imm n
             | _ =>
                  (case stripCast z of
                      Operand.Address _ => materialize ()
                    | Operand.Const (Const.Real r) =>
                         Opnd.Mem (literal (size ct, realBits r))
                    | Operand.Const _ => materialize ()
                    | Operand.Label _ => materialize ()
                    | Operand.StaticHeapRef _ => materialize ()
                    | z' =>
                         (case regHome z' of
                             SOME h => reg (h, ct)
                           | NONE => Opnd.Mem (addr (z', scr))))
         end
      (* set a register home, keeping narrow values zero-extended *)
      fun setReg (ct, h, r, clean) =
         if h >= 16 orelse clean then mov (h, r, ct)
         else
            case size ct of
               1 => ins ("movzbl", [Opnd.Reg (r, Size.B), Opnd.Reg (h, Size.L)])
             | 2 => ins ("movzwl", [Opnd.Reg (r, Size.W), Opnd.Reg (h, Size.L)])
             | 4 => ins ("movl", [Opnd.Reg (r, Size.L), Opnd.Reg (h, Size.L)])
             | _ => mov (h, r, ct)
      (* what is stored: a register (and whether it is clean), or an
       * immediate
       *)
      datatype src =
         R of int * bool
       | N of IntInf.t
      fun put (dst, src) =
         let
            val ct = cty dst
            fun set h =
               case src of
                  R (r, clean) => setReg (ct, h, r, clean)
                | N n => movImm (h, if n < 0 then n + pow2 (8 * size ct) else n)
            fun toMem a =
               case src of
                  R (r, _) => st (ct, r, a)
                | N n => ins ("mov" ^ Size.suffix (sz ct),
                              [Opnd.Imm n, Opnd.Mem a])
            fun scr () =
               List.keepAll ([rax, rcx, rdx], fn s =>
                             case src of
                                R (r, _) => r <> s
                              | N _ => true)
         in
            case (stripCast dst, home dst) of
               (Operand.StackOffset so, _) =>
                  let
                     val k as (off, _) = slotKey so
                     val () = toMem (based (Emit.stackTop, off))
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
                    | NONE => toMem (tempAddr (ct, Temporary.index t)))
             | (_, SOME h) => set h
             | (z, NONE) => toMem (addr (z, scr ()))
         end
      fun store (dst, r, {clean}) = put (dst, R (r, clean))
      (* A register in which to compute the value of dst. *)
      fun dstReg dst =
         case stripCast dst of
            Operand.Temporary t => defHome t
          | _ => home dst
      fun move {dst, src} =
         let
            val ct = cty dst
            val regDst =
               case stripCast dst of
                  Operand.StackOffset _ => NONE
                | Operand.Temporary t => defHome t
                | _ => home dst
         in
            case regDst of
               SOME h => load (src, h, [rax, rcx])
             | NONE =>
                  if isReal ct
                     then store (dst, regOf (src, Emit.xmm0, [rax, rcx]),
                                 {clean = true})
                  else
                     (case immOf src of
                         SOME n => put (dst, N n)
                       | NONE =>
                            store (dst, regOf (src, rax, [rax, rcx]),
                                   {clean = true}))
         end
      fun setDst (dst, old, set) =
         let
            val ct = cty dst
            val (d, spilled) =
               case Option.fold (dstTemp dst, NONE, fn (t, _) =>
                                 defHome t) of
                  SOME h => (h, false)
                | NONE => (if isReal ct then Emit.xmm0 else rax, true)
         in
            (* The old value of a spilled destination is in its slot
             * (not where the block finds the temporary on entry,
             * which is what load would read).
             *)
            if spilled andalso old
               then ld (ct, d, addr (dst, [rcx]))
            else ()
            ; set d
            ; if spilled then store (dst, d, {clean = true}) else ()
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
                                            (* An instruction can read a slot where
                                             * it is; a register is worth it from
                                             * the third read.  (An operation and
                                             * its overflow check count as two.)
                                             *)
                                            if !n >= 3 then SOME k else NONE)
            ; freeRegs := List.keepAll
                          (callee @ caller @ fcaller @ fcallee, fn r =>
                           not (List.contains (busy, r, op =)))
         end
      fun startBlock (b as Block.T {label = l, ...}) =
         (curLabel := l
          ; region := blockRegion l
          ; enterBlock l
          ; setupCache b)
      fun noCache () = cacheable := []
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
            val () = commit (st, saves)
         in
            {restore = fn () => commit (ld, saves)}
         end
      fun callRuntime (name, setup) =
         let
            (* We don't know what is live here, so commit all of the
             * caller-save temporaries.
             *)
            val saves =
               callerSaves (List.map (!curWebs, #2)
                            @ List.map (!curPromoted, #2))
         in
            commit (st, saves)
            ; setup ()
            ; emit (I.Call (Lbl.Sym name))
            ; clearCache ()
            ; commit (ld, saves)
         end
   in
      {startBlock = startBlock,
       curBlock = fn () => !curLabel,
       enterBlock = enterBlock,
       defined = defined,
       home = home,
       reads = reads,
       needsScratch = needsScratch,
       immOf = immOf,
       opnd = opnd,
       addr = addr,
       regOf = regOf,
       load = load,
       dstReg = dstReg,
       store = store,
       move = move,
       setDst = setDst,
       clearCache = clearCache,
       noCache = noCache,
       resInt = resInt,
       resReal = resReal,
       shuffle = shuffle,
       saveForCall = saveForCall,
       afterCall = clearCache,
       callRuntime = callRuntime}
   end

end

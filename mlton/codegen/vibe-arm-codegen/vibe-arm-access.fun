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
structure Opnd = Arm64.Operand
structure R = Arm64.Reg

val size = Analysis.size

val x = Emit.x
val reg = Emit.reg
val immOp = Emit.immOp

fun cty z = Type.toCType (Operand.ty z)

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
    dstReg: Operand.t -> int,
    store: Operand.t * int -> unit,
    move: {dst: Operand.t, src: Operand.t} -> unit,
    setDst: Operand.t * bool * (int -> unit) -> unit,
    clearCache: unit -> unit,
    noCache: unit -> unit,
    setSpAdj: int -> unit,
    saveForCall: Label.t option -> {restore: unit -> unit},
    afterCall: unit -> unit,
    callRuntime: string -> unit,
    edgeNeedsCode: Label.t -> bool,
    beforeGoto: Label.t -> unit,
    edgeCode: Label.t -> unit -> unit}

type args = {analysis: Analysis.t,
             emit: Emit.t,
             labelIndex: Label.t -> int,
             tempsMax: CType.t -> int}

fun frame {emit = {mem, ...}: Emit.t, tempsMax} =
   let
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
   in
      {frameSize = frameSize,
       homeOf = homeOf,
       setSpAdj = fn n => spAdj := n,
       commit = commit}
   end

fun operands {emit = e: Emit.t, labelIndex, homeOf, home, cacheIn} =
   let
      val {emit, ldr, mov, movImm, movInt, addImm, mem, symAddr, ...} = e
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
   in
      {value = value,
       load = load,
       memOf = memOf,
       setReg = setReg}
   end

end

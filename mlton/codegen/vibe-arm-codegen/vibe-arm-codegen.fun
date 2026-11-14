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
 *    x22-x28, d8-d15: the low-numbered temporaries (see tempHome)
 *    x9-x11, d16-d18: operands/results of the current statement
 *    x12-x15: address computations
 *    x16-x17: constants, large offsets
 * All other temporaries live in the (native) stack frame.
 * Integer values narrower than 64 bits are kept zero-extended in registers.
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
      fun scratch (ct, k) = if isReal ct then 48 + k else 9 + k
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
      fun tempHome t =
         let
            val n = Temporary.index t
            fun home (k, base) = if n < k then SOME (base + n) else NONE
         in
            case Type.toCType (Temporary.ty t) of
               CType.Objptr => home (3, 22)
             | CType.Word64 => home (2, 25)
             | CType.Word32 => home (2, 27)
             | CType.Real64 => home (8, 40)
             | _ => NONE
         end
      val saved = [(19, 20), (21, 22), (23, 24), (25, 26), (27, 28),
                   (40, 41), (42, 43), (44, 45), (46, 47)]

      fun cty z = Type.toCType (Operand.ty z)
      fun home z =
         case z of
            Operand.Cast (z, _) => home z
          | Operand.Frontier => SOME 21
          | Operand.GCState => SOME 19
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
                         let
                            val (b, off) = addr (z, t)
                         in
                            i ["ldr", sfx ct, " ", reg (d, ct), ", ",
                               mem (b, off, ct)]
                         end)
         end
      (* A register holding the value of z: either its home or d. *)
      and value (z, d, t): int =
         case home z of
            SOME h => h
          | NONE => (load (z, d, t); d)
      fun store (dst, r) =
         let
            val ct = cty dst
         in
            case home dst of
               SOME h => mov (h, r, ct)
             | NONE =>
                  let
                     val (b, off) = addr (dst, 12)
                  in
                     i ["str", sfx ct, " ", reg (r, ct), ", ", mem (b, off, ct)]
                  end
         end
      (* A register in which to compute the value of dst. *)
      fun dstReg dst =
         case home dst of
            SOME h => h
          | NONE => scratch (cty dst, 0)
      fun move {dst, src} = store (dst, value (src, dstReg dst, 12))

      fun primApp (prim: Type.t Prim.t, args, dst) =
         let
            datatype z = datatype Prim.t
            val dct = cty dst
            val d = dstReg dst
            fun a k = Vector.sub (args, k)
            (* a register holding argument k *)
            fun v k = value (a k, scratch (cty (a k), k), 12)
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
            fun cset cond = (i ["cset ", reg (d, dct), ", ", cond]; done ())
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
                  val s = 9 + k
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
                     concat (List.map (ks, fn k =>
                                       ", " ^ x (ext (k, ws, signed, true))))
               in
                  if n < 64
                     then (i [opc, " x11", ops]
                           ; i ["cmp x11, w11, ", if signed then "s" else "u",
                                "xt", bhw n]
                           ; cset "ne")
                  else if opc = "mul"
                     then (i [if signed then "smulh" else "umulh", " x11", ops]
                           ; if signed
                                then (i ["mul x12", ops]
                                      ; i ["cmp x11, x12, asr #63"])
                             else i ["cmp x11, #0"]
                           ; cset "ne")
                  else (i [opc, "s x11", ops]
                        ; cset (if signed then sCond else uCond))
               end
            fun shift (opc, ws, r0) = bin' (opc, wct ws, r0, true)
            (* compare-and-swap; setAddr puts the address in x15 *)
            fun cas (ct, kOld, kNew, obj, setAddr) =
               let
                  fun iv k =
                     let
                        val r = v k
                     in
                        if r >= 32
                           then (i ["fmov ", reg (9 + k, ct), ", ", reg (r, ct)]
                                 ; 9 + k)
                        else r
                     end
                  val old = iv kOld
                  val new = iv kNew
                  val () = setAddr ()
               in
                  label "1"
                  ; i ["ldaxr", sfx ct, " ", reg (13, ct), ", [x15]"]
                  ; i ["cmp ", reg (13, ct), ", ", reg (old, ct)]
                  ; i ["b.ne 2f"]
                  ; i ["stlxr", sfx ct, " w14, ", reg (new, ct), ", [x15]"]
                  ; i ["cbnz w14, 1b"]
                  ; label "2"
                  ; (case ct of
                        CType.Objptr =>
                           (mov (0, obj, ct)
                            ; i ["mov x1, x13"]
                            ; i ["bl ", sym "Assignable_decheckObjptr"]
                            ; mov (d, 0, ct))
                      | _ => if isReal ct
                                then i ["fmov ", reg (d, ct), ", ", reg (13, ct)]
                             else mov (d, 13, ct))
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
                          i ["add x15, ", x obj, ", ", x ix, ", lsl #", int lg])
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
                   ; i ["cset w11, eq"]
                   ; i ["csinc ", reg (d, dct), ", w11, wzr, vc"]
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
                     cas (ct, 1, 2, obj, fn () => mov (15, obj, ptr))
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
                     i [if signed then "sdiv " else "udiv ", reg (11, ct),
                        ", ", reg (r0, ct), ", ", reg (r1, ct)]
                     ; i ["msub ", reg (d, ct), ", ", reg (11, ct), ", ",
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
                     i ["neg ", reg (11, ct), ", ", reg (v 1, ct)]
                     ; i ["ror ", reg (d, ct), ", ", reg (r0, ct), ", ",
                          reg (11, ct)]
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
      fun outputStatement s =
         case s of
            Statement.Move z => move z
          | Statement.PrimApp {args, dst, prim} =>
               (* all primitives are pure *)
               Option.app (dst, fn dst => primApp (prim, args, dst))

      fun gcField f = Operand.gcField f
      fun flushFrontier () =
         move {dst = gcField GCField.Frontier, src = Operand.Frontier}
      fun flushStackTop () =
         move {dst = gcField GCField.StackTop, src = Operand.StackTop}
      fun goto l = i ["b ", Label.toString l]
      (* return nextBlock (in x0) to the trampoline *)
      fun leaveChunk () = (flushFrontier (); flushStackTop (); i ["b ", leave])
      fun jump l =
         if ChunkLabel.equals (labelChunk l, chunkLabel)
            then goto l
         else (movInt (0, labelIndex l); leaveChunk ())
      fun push (return, size) =
         let
            val size = Bytes.toInt size
         in
            movInt (9, labelIndex return)
            ; i ["str x9, ", mem (20, size - 8, CType.Word64)]
            ; addImm (20, 20, size)
            ; (case labelBlock return of
                  Block.T {kind, ...} =>
                     (case Option.map (Kind.frameInfoOpt kind,
                                       FrameInfo.sporkInfo) of
                         SOME (SOME _) =>
                            (* push StackTop on the promotion stack *)
                            let
                               val pst = gcField GCField.PromoStackTop
                            in
                               load (pst, 9, 12)
                               ; i ["str x20, [x9], #8"]
                               ; store (pst, 9)
                            end
                       | _ => ()))
         end
      fun pop (fi, {promo}) =
         (addImm (20, 20, ~ (Bytes.toInt (FrameInfo.size fi)))
          ; if promo andalso Option.isSome (FrameInfo.sporkInfo fi)
               then
                  let
                     val pst = gcField GCField.PromoStackTop
                     val psb = gcField GCField.PromoStackBot
                  in
                     load (pst, 9, 12)
                     ; i ["sub x9, x9, #8"]
                     ; store (pst, 9)
                     ; load (psb, 10, 12)
                     ; i ["cmp x10, x9"]
                     ; i ["b.ls 1f"]
                     ; store (psb, 9)
                     ; label "1"
                  end
            else ())
      fun indJump {mustReturnToSelf, mayReturnToSelf} =
         (i ["ldr x3, [x20, #-8]"]
          ; if mustReturnToSelf
               then i ["b ", switch]
            else (if mayReturnToSelf
                     then (symAddr (16, "nextChunks", {got = false})
                           ; i ["ldr x16, [x16, x3, lsl #3]"]
                           ; symAddr (17, self, {got = false})
                           ; i ["cmp x16, x17"]
                           ; i ["b.ne 1f"]
                           ; i ["b ", switch]
                           ; label "1")
                  else ()
                  ; i ["mov x0, x3"]
                  ; leaveChunk ()))
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
            val args =
               case (target, args) of
                  (Indirect, fptr :: args) => (load (fptr, 11, 12); args)
                | _ => args
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
                   val () = load (z, r, 12)
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
                | Indirect => i ["blr x11"]
            val () = addImm (31, 31, stk)
            val () =
               case retTy of
                  NONE => ()
                | SOME ct => if isReal ct
                                then i ["str d0, [sp, #8]"]
                             else i ["str x0, [sp]"]
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
               (i ["add x20, ",
                   x (value (gcField GCField.StackBottom, 9, 12)), ", ",
                   x (value (gcField GCField.ExnStack, 10, 12))]
                ; rtrans raisesTo)
          | Transfer.Return {returnsTo} => rtrans returnsTo
          | Transfer.Spork {cont, ...} => goto cont
          | Transfer.Switch (Switch.T {cases, default, test, ...}) =>
               let
                  val r = value (test, 9, 12)
                  val cases = Vector.toList cases
                  val (cases, default) =
                     case default of
                        SOME l => (cases, l)
                      | NONE => let
                                   val (cases, (_, l)) = List.splitLast cases
                                in
                                   (cases, l)
                                end
               in
                  List.foreach
                  (cases, fn (w, l) =>
                   let
                      val n = WordX.toIntInf w
                   in
                      if n = 0
                         then i ["cbnz ", x r, ", 1f"]
                      else (if n < 4096
                               then i ["cmp ", x r, ", #", IntInf.toString n]
                            else (movImm (16, n); i ["cmp ", x r, ", x16"])
                            ; i ["b.ne 1f"])
                      ; goto l
                      ; label "1"
                   end)
                  ; goto default
               end
      fun outputBlock (Block.T {kind, label = l, statements, transfer, ...}) =
         (label (Label.toString l)
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
          ; Vector.foreach (statements, outputStatement)
          ; outputTransfer transfer)

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
   in
      print "\t.text\n\t.p2align 2\n"
      ; i [".globl ", sym self]
      ; i [if darwin then ".private_extern " else ".hidden ", sym self]
      ; label (sym self)
      ; i ["stp x29, x30, [sp, #-160]!"]
      ; i ["mov x29, sp"]
      ; List.foreachi (saved, fn (k, rs) => pair ("stp ", rs, k))
      ; addImm (31, 31, ~ frameSize)
      ; i ["mov x19, x0"]
      ; i ["mov x20, x1"]
      ; i ["mov x21, x2"]
      (* jump to the entry block with index x3 *)
      ; label switch
      ; addImm (16, 3, ~ lo)
      ; i ["adr x17, ", table]
      ; i ["ldrsw x16, [x17, x16, lsl #2]"]
      ; i ["add x17, x17, x16"]
      ; i ["br x17"]
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
      ; Vector.foreach (blocks, outputBlock)
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

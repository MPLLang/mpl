(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor VibeArm64Prim (S: VIBE_ARM64_PRIM_STRUCTS): VIBE_ARM64_PRIM =
struct

open S
structure Emit = Access.Emit
structure Items = Access.Items
structure Machine = Emit.Machine
open Machine

structure Arm64 = Emit.Arm64
structure Addr = Arm64.Addr
structure Cond = Arm64.Cond
structure I = Arm64.Instr
structure Opnd = Arm64.Operand
structure R = Arm64.Reg
structure Tgt = Arm64.Target

val isReal = Items.Analysis.isReal
val size = Items.Analysis.size

val x = Emit.x
val reg = Emit.reg
val regOp = Emit.regOp
val immOp = Emit.immOp
val scratch = Emit.scratch
val pow2 = Emit.pow2
val log2 = Emit.log2
val bitmask = Emit.bitmask

val cty = Access.cty

type t = {access: Access.t,
          emit: Emit.t}

(* If flags, a primitive that computes a condition leaves it in the flags,
 * and returns it, rather than computing dst.
 *
 * If there is an "also", an overflow-checking primitive also stores the
 * result of the operation itself to that operand; the bool is true if the
 * check originally came before the operation.
 *)
fun primApp ({access, emit = e}: t,
             {args, dst, prim, also}: Items.app,
             {flags = fusing: bool}): Cond.t option =
   let
      val {callRuntime, dstReg, move, store, value, ...} = access
      val {emit, mov, movImm, ...} = e
      val fused: Cond.t option ref = ref NONE
      val () =
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
                        then Opnd.Imm (WordX.toIntInf w)
                     else regOp (reg (v k, ct))
                | _ => regOp (reg (v k, ct))
            fun done () = store (dst, d)
            fun un (opc, ct) =
               (emit (I.Un (opc, reg (d, dct), reg (v 0, ct))); done ())
            fun bin' (opc, ct, r0, immOk) =
               (emit (I.Bin (opc, reg (d, ct), reg (r0, ct),
                             opnd (1, ct, immOk)))
                ; done ())
            fun bin (opc, ct, immOk) = bin' (opc, ct, v 0, immOk)
            fun tern (opc, ct) =
               (emit (I.Tern (opc, reg (d, ct), reg (v 0, ct), reg (v 1, ct),
                              reg (v 2, ct)))
                ; done ())
            fun cset cond =
               if fusing
                  then fused := SOME cond
               else (emit (I.Cset (reg (d, dct), cond)); done ())
            fun cmp' (opc, ct, r0, cond) =
               (emit (I.Cmp (opc, reg (r0, ct), opnd (1, ct, opc = I.CMP)))
                ; cset cond)
            fun cmp (opc, ct, cond) = cmp' (opc, ct, v 0, cond)
            fun addsub (isAdd, ct) =
               let
                  fun opc b = if b then I.ADD else I.SUB
                  fun default () = bin (opc isAdd, ct, true)
               in
                  case a 1 of
                     Operand.Const (Const.Word w) =>
                        let
                           val n = WordX.toIntInfX w
                        in
                           if size ct = 8 andalso ~4096 < n andalso n < 0
                              then (emit (I.Bin (opc (not isAdd), reg (d, ct),
                                                 reg (v 0, ct), Opnd.Imm (~ n)))
                                    ; done ())
                           else default ()
                        end
                   | _ => default ()
               end
            val ptr = CType.CPointer
            val rct = CType.real
            fun wct ws = CType.word (ws, {signed = false})
            fun bits ws = Bits.toInt (WordSize.bits ws)
            (* sign extension from n bits *)
            fun sxt n = case n of 8 => I.SXTB | 16 => I.SXTH | _ => I.SXTW
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
                     then (emit (I.Un (sxt n, if to64 then x s else R.W s,
                                       R.W r))
                           ; s)
                  else r
               end
            datatype arith = Add | Mul | Neg | Sub
            (* an overflow check for the operation opc on arguments ks *)
            fun chk (opc: arith, (ws, {signed}), ks) =
               let
                  val n = bits ws
                  (* the operands of the operation *)
                  val (r0, z1) =
                     if n = 64 andalso (opc = Add orelse opc = Sub)
                        then
                           let
                              val r0 = x (v 0)
                           in
                              (r0, SOME (opnd (1, wct ws, true)))
                           end
                     else (case List.map (ks, fn k =>
                                          x (ext (k, ws, signed, true))) of
                              [r0] => (r0, NONE)
                            | [r0, r1] => (r0, SOME (regOp r1))
                            | _ => Error.bug "VibeArm64Prim.chk: operands")
                  (* a binary instruction on the operands *)
                  fun binary (opc, d) =
                     case z1 of
                        SOME z => emit (I.Bin (opc, d, r0, z))
                      | NONE => Error.bug "VibeArm64Prim.chk: binary"
                  (* the operation itself, which may also set the flags *)
                  fun arith (d, {flags}) =
                     case opc of
                        Add => binary (if flags then I.ADDS else I.ADD, d)
                      | Mul => binary (I.MUL, d)
                      | Neg => emit (I.Un (if flags then I.NEGS else I.NEG,
                                           d, r0))
                      | Sub => binary (if flags then I.SUBS else I.SUB, d)
                  (* r holds the result of the operation itself *)
                  fun finish (r, cond) =
                     case also of
                        NONE => cset cond
                      | SOME (z, chkFirst) =>
                           if chkFirst
                              then (cset cond; store (z, r))
                           else (store (z, r); cset cond)
               in
                  if n < 64
                     then (arith (R.X 5, {flags = false})
                           ; emit (I.Cmp
                                   (I.CMP, R.X 5,
                                    Opnd.Extended
                                    (R.W 5,
                                     case (signed, n) of
                                        (true, 8) => Opnd.SXTB
                                      | (true, 16) => Opnd.SXTH
                                      | (true, _) => Opnd.SXTW
                                      | (false, 8) => Opnd.UXTB
                                      | (false, 16) => Opnd.UXTH
                                      | (false, _) => Opnd.UXTW)))
                           ; finish (5, Cond.NE))
                  else if opc = Mul
                     then (binary (if signed then I.SMULH else I.UMULH, R.X 5)
                           ; if signed orelse Option.isSome also
                                then arith (R.X 6, {flags = false})
                             else ()
                           ; if signed
                                then emit (I.Cmp (I.CMP, R.X 5,
                                                  Opnd.Shifted (R.X 6, Opnd.ASR,
                                                                63)))
                             else emit (I.Cmp (I.CMP, R.X 5, immOp 0))
                           ; finish (6, Cond.NE))
                  else
                     let
                        val r = case also of
                                   SOME (z, false) => dstReg z
                                 | _ => 5
                     in
                        arith (x r, {flags = true})
                        ; finish (r, case (signed, opc) of
                                        (true, _) => Cond.VS
                                      | (false, Add) => Cond.CS
                                      | (false, _) => Cond.CC)
                     end
               end
            fun shift (opc, ws, r0) = bin' (opc, wct ws, r0, true)
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
                              then (emit (I.Bin (opc, reg (d, ct),
                                                 reg (v 0, ct), Opnd.Imm n))
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
                        fun r k = if N = 64 then x k else R.W k
                        fun ins (opc, d, s, z) = emit (I.Bin (opc, d, s, z))
                        fun lsr (r, k) = Opnd.Shifted (r, Opnd.LSR, k)
                     in
                        if dv < 2 then false
                        else
                           let
                              val n = ext (0, ws, signed, false)
                              (* the quotient is in register q *)
                              fun finish q =
                                 (if isRem
                                     then (movImm (16, dv)
                                           ; emit (I.Tern (I.MSUB, r d, r q,
                                                           r 16, r n)))
                                  else if q = d then ()
                                  else emit (I.Un (I.MOV, r d, r q))
                                  ; done ())
                              fun shiftBy (opc, k) =
                                 if k > 0
                                    then ins (opc, r 5, r 5, immOp k)
                                 else ()
                              (* x5 = the high half of n * x16 *)
                              fun umulh () =
                                 ins (I.UMULH, R.X 5, x n, regOp (R.X 16))
                           in
                              (case (log2 dv, signed) of
                                  (SOME k, false) =>
                                     (if isRem
                                         then ins (I.AND, r d, r n,
                                                   Opnd.Imm (dv - 1))
                                      else ins (I.LSR, r d, r n, immOp k)
                                      ; done ())
                                | (SOME k, true) =>
                                     (* round toward zero: add 2^k - 1 to a
                                      * negative dividend
                                      *)
                                     (ins (I.ASR, r 5, r n, immOp (N - 1))
                                      ; ins (I.ADD, r 5, r n, lsr (r 5, N - k))
                                      ; if isRem
                                           then (ins (I.AND, r 5, r 5,
                                                      Opnd.Imm (pow2 N - dv))
                                                 ; ins (I.SUB, r d, r n,
                                                        regOp (r 5)))
                                        else ins (I.ASR, r d, r 5, immOp k)
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
                                                      "VibeArm64Prim.divConst"
                                              else search (s + 1)
                                           end
                                        val (m, s) = search 1
                                     in
                                        movImm (16, m)
                                        ; if N = 64
                                             then ins (I.SMULH, R.X 5, x n,
                                                       regOp (R.X 16))
                                          else (ins (I.SMULL, R.X 5, R.W n,
                                                     regOp (R.W 16))
                                                ; ins (I.ASR, R.X 5, R.X 5,
                                                       immOp 32))
                                        ; if m >= pow2 (N - 1)
                                             then ins (I.ADD, r 5, r 5,
                                                       regOp (r n))
                                          else ()
                                        ; shiftBy (I.ASR, s - 1)
                                        ; ins (I.ADD, r 5, r 5, lsr (r n, N - 1))
                                        ; finish 5
                                     end
                                | (NONE, false) =>
                                     if N = 32
                                        then
                                           (* n < 2^32: one 64-bit multiply is
                                            * exact
                                            *)
                                           (movImm (16, pow2 64 div dv + 1)
                                            ; umulh ()
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
                                                         "VibeArm64Prim.divConst"
                                                 else search (s + 1)
                                              end
                                           val (m, s) = search 0
                                        in
                                           if m < pow2 64
                                              then (movImm (16, m)
                                                    ; umulh ()
                                                    ; shiftBy (I.LSR, s))
                                           else
                                              (* m needs 65 bits *)
                                              (movImm (16, m - pow2 64)
                                               ; umulh ()
                                               ; ins (I.SUB, R.X 6, x n,
                                                      regOp (R.X 5))
                                               ; ins (I.ADD, R.X 5, R.X 5,
                                                      lsr (R.X 6, 1))
                                               ; shiftBy (I.LSR, s - 1))
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
                           then (emit (I.Un (I.FMOV, reg (k, ct), reg (r, ct)))
                                 ; k)
                        else r
                     end
                  val old = iv kOld
                  val new = iv kNew
                  val () = setAddr ()
                  val (ldaxr, stlxr) =
                     case size ct of
                        1 => (I.LDAXRB, I.STLXRB)
                      | 2 => (I.LDAXRH, I.STLXRH)
                      | _ => (I.LDAXR, I.STLXR)
               in
                  emit (I.LocalLabel 1)
                  ; emit (I.Load (ldaxr, reg (5, ct), Addr.Base (R.X 7)))
                  ; emit (I.Cmp (I.CMP, reg (5, ct), regOp (reg (old, ct))))
                  ; emit (I.Bcond (Cond.NE, Tgt.Forward 2))
                  ; emit (I.StoreX (stlxr, R.W 6, reg (new, ct),
                                    Addr.Base (R.X 7)))
                  ; emit (I.Cbnz (R.W 6, Tgt.Backward 1))
                  ; emit (I.LocalLabel 2)
                  ; (case ct of
                        CType.Objptr =>
                           (* Assignable_decheckObjptr (s, obj, x5); obj
                            * may be in x0, x1 or x2, so read it first.
                            *)
                           (mov (1, obj, ct)
                            ; emit (I.Un (I.MOV, R.X 2, R.X 5))
                            ; emit (I.Un (I.MOV, R.X 0, R.X 19))
                            ; callRuntime "Assignable_decheckObjptr"
                            ; mov (d, 0, ct))
                      | _ => if isReal ct
                                then emit (I.Un (I.FMOV, reg (d, ct),
                                                 reg (5, ct)))
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
                          emit (I.Bin (I.ADD, R.X 7, x obj,
                                       Opnd.Shifted (x ix, Opnd.LSL, lg))))
                  end
             | CPointer_add => addsub (true, ptr)
             | CPointer_diff => bin (I.SUB, ptr, false)
             | CPointer_equal => cmp (I.CMP, ptr, Cond.EQ)
             | CPointer_fromWord => move {dst = dst, src = a 0}
             | CPointer_lt => cmp (I.CMP, ptr, Cond.LO)
             | CPointer_sub => addsub (false, ptr)
             | CPointer_toWord => move {dst = dst, src = a 0}
             | Real_Math_sqrt rs => un (I.FSQRT, rct rs)
             | Real_abs rs => un (I.FABS, rct rs)
             | Real_add rs => bin (I.FADD, rct rs, false)
             | Real_castToWord (rs, _) => un (I.FMOV, rct rs)
             | Real_div rs => bin (I.FDIV, rct rs, false)
             | Real_equal rs => cmp (I.FCMP, rct rs, Cond.EQ)
             | Real_le rs => cmp (I.FCMP, rct rs, Cond.LS)
             | Real_lt rs => cmp (I.FCMP, rct rs, Cond.MI)
             | Real_mul rs => bin (I.FMUL, rct rs, false)
             | Real_muladd rs => tern (I.FMADD, rct rs)
             | Real_mulsub rs => tern (I.FNMSUB, rct rs)
             | Real_neg rs => un (I.FNEG, rct rs)
             | Real_qequal rs =>
                  (emit (I.Cmp (I.FCMP, reg (v 0, rct rs),
                                regOp (reg (v 1, rct rs))))
                   ; emit (I.Cset (R.W 5, Cond.EQ))
                   ; emit (I.Sel (I.CSINC, reg (d, dct), R.W 5, R.WZR, Cond.VC))
                   ; done ())
             | Real_rndToReal (rs, _) => un (I.FCVT, rct rs)
             | Real_rndToWord (rs, _, {signed}) =>
                  un (if signed then I.FCVTZS else I.FCVTZU, rct rs)
             | Real_round rs => un (I.FRINTX, rct rs)
             | Real_sub rs => bin (I.FSUB, rct rs, false)
             | Ref_cas (SOME ct) =>
                  let
                     val obj = v 0
                  in
                     cas (ct, 1, 2, obj, fn () => mov (7, obj, ptr))
                  end
             | Word_add ws => addsub (true, wct ws)
             | Word_addCheckP z => chk (Add, z, [0, 1])
             | Word_andb ws => logic (I.AND, ws)
             | Word_castToReal (ws, _) => un (I.FMOV, wct ws)
             | Word_equal ws => cmp (I.CMP, wct ws, Cond.EQ)
             | Word_extdToWord (ws1, ws2, {signed}) =>
                  let
                     val r = v 0
                  in
                     if signed andalso bits ws1 < bits ws2
                        then emit (I.Un (sxt (bits ws1), reg (d, wct ws2),
                                         R.W r))
                     else if bits ws1 = 64 andalso bits ws2 = 32
                        (* A truncation must clear the upper half of the
                         * register, even when d = r.
                         *)
                        then emit (I.Un (I.MOV, R.W d, R.W r))
                     else mov (d, r, wct ws2)
                     ; done ()
                  end
             | Word_lshift ws => shift (I.LSL, ws, v 0)
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
                              then emit (I.Cmp (I.CMP, r, Opnd.Imm n))
                           else emit (I.Cmp (I.CMN, r, Opnd.Imm (~ n)))
                           ; cset cond
                        end
                  in
                     case (imm 0, imm 1) of
                        (_, SOME n) =>
                           cmpImm (0, n, if signed then Cond.LT else Cond.LO)
                      | (SOME n, NONE) =>
                           cmpImm (1, n, if signed then Cond.GT else Cond.HI)
                      | _ =>
                           let
                              val r0 = ext (0, ws, signed, false)
                              val r1 = ext (1, ws, signed, false)
                           in
                              emit (I.Cmp (I.CMP, reg (r0, wct ws),
                                           regOp (reg (r1, wct ws))))
                              ; cset (if signed then Cond.LT else Cond.LO)
                           end
                  end
             | Word_mul (ws, _) =>
                  let
                     val ct = wct ws
                     fun default () = bin (I.MUL, ct, false)
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
                                    (emit (I.Bin (I.LSL, reg (d, ct), r,
                                                  immOp k))
                                     ; done ())
                               | (_, SOME k) =>
                                    (emit (I.Bin (I.ADD, reg (d, ct), r,
                                                  Opnd.Shifted (r, Opnd.LSL,
                                                                k)))
                                     ; done ())
                               | _ => default ()
                           end
                      | _ => default ()
                  end
             | Word_mulCheckP z => chk (Mul, z, [0, 1])
             | Word_neg ws => un (I.NEG, wct ws)
             | Word_negCheckP z => chk (Neg, z, [0])
             | Word_notb ws => un (I.MVN, wct ws)
             | Word_orb ws => logic (I.ORR, ws)
             | Word_quot (ws, {signed}) =>
                  if divConst (ws, signed, false) then ()
                  else
                  let
                     val r0 = ext (0, ws, signed, false)
                     val r1 = ext (1, ws, signed, false)
                  in
                     emit (I.Bin (if signed then I.SDIV else I.UDIV,
                                  reg (d, wct ws), reg (r0, wct ws),
                                  regOp (reg (r1, wct ws))))
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
                     emit (I.Bin (if signed then I.SDIV else I.UDIV,
                                  reg (5, ct), reg (r0, ct),
                                  regOp (reg (r1, ct))))
                     ; emit (I.Tern (I.MSUB, reg (d, ct), reg (5, ct),
                                     reg (r1, ct), reg (r0, ct)))
                     ; done ()
                  end
             | Word_rndToReal (ws, _, {signed}) =>
                  (emit (I.Un (if signed then I.SCVTF else I.UCVTF,
                               reg (d, dct),
                               reg (ext (0, ws, signed, false), wct ws)))
                   ; done ())
             | Word_rol ws =>
                  let
                     val ct = wct ws
                     val r0 = v 0
                  in
                     emit (I.Un (I.NEG, reg (5, ct), reg (v 1, ct)))
                     ; emit (I.Bin (I.ROR, reg (d, ct), reg (r0, ct),
                                    regOp (reg (5, ct))))
                     ; done ()
                  end
             | Word_ror ws =>
                  (case a 1 of
                      Operand.Const (Const.Word w) =>
                         (emit (I.Bin (I.ROR, reg (d, wct ws),
                                       reg (v 0, wct ws),
                                       Opnd.Imm
                                       (WordX.toIntInf w
                                        mod IntInf.fromInt (bits ws))))
                          ; done ())
                    | _ => shift (I.ROR, ws, v 0))
             | Word_rshift (ws, {signed}) =>
                  if Vector.length args = 3
                     then
                        (* a bitfield extract: see "direct", below *)
                        let
                           fun const k =
                              case a k of
                                 Operand.Const (Const.Word w) => WordX.toIntInf w
                               | _ => Error.bug "VibeArm64Prim.primApp: extract"
                           val ct = wct ws
                        in
                           emit (I.Ubfx (reg (d, ct), reg (v 0, ct),
                                         IntInf.toInt (const 1),
                                         valOf (log2 (const 2 + 1))))
                           ; done ()
                        end
                  else
                  shift (if signed then I.ASR else I.LSR, ws,
                         ext (0, ws, signed, false))
             | Word_sub ws => addsub (false, wct ws)
             | Word_subCheckP z => chk (Sub, z, [0, 1])
             | Word_xorb ws => logic (I.EOR, ws)
             | _ => Error.bug (concat ["VibeArm64Prim.primApp: ",
                                       Prim.toString prim])
         end
   in
      !fused
   end

fun app (z, a) = ignore (primApp (z, a, {flags = false}))
fun appFlags (z, a) = primApp (z, a, {flags = true})

end

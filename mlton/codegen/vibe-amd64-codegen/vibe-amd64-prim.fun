(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor VibeAmd64Prim (S: VIBE_AMD64_PRIM_STRUCTS): VIBE_AMD64_PRIM =
struct

open S
structure Emit = Access.Emit
structure Items = Access.Items
structure Machine = Emit.Machine
open Machine

structure Asm = Emit.Asm
structure Addr = Asm.Addr
structure Cond = Asm.Cond
structure I = Asm.Instr
structure Opnd = Asm.Operand
structure Size = Asm.Size
structure Tgt = Asm.Target

val rax = Emit.rax
val rcx = Emit.rcx
val rdx = Emit.rdx
val rsi = Emit.rsi
val rdi = Emit.rdi
val xmm0 = Emit.xmm0
val xmm1 = Emit.xmm1
val isReal = Emit.isReal
val size = Emit.size
val sz = Emit.sz
val pow2 = Emit.pow2
val log2 = Emit.log2
val simm32 = Emit.simm32

val cty = Access.cty

type t = {access: Access.t,
          emit: Emit.t}

val R = Opnd.Reg
fun X r = Opnd.Reg (r, Size.Q)
fun imm (n: int) = Opnd.Imm (IntInf.fromInt n)

(* If flags, a primitive that computes a condition leaves it in the flags,
 * and returns it, rather than computing dst.
 *
 * If there is an "also", an overflow-checking primitive also stores the
 * result of the operation itself to that operand.
 *
 * An x86 instruction overwrites one of its operands, so a destination that
 * lives in a register can be computed in place only if the register isn't
 * read later in the sequence; otherwise the result is computed in rax (or
 * xmm0) and then moved.  The destination's register may be that of an
 * argument, since the argument may die here.
 *)
fun primApp ({access, emit = e}: t,
             {args, dst, prim, also}: Items.app,
             {flags = fusing: bool, selCond: Cond.t option}): Cond.t option =
   let
      val {addr, callRuntime, dstReg, home, immOf, load, move, opnd, reads,
           regOf, store, ...} = access
      val {emit, flagsLive, ins, ld, lea, literal, mov, movImm, ...} = e
      val fused: Cond.t option ref = ref NONE
      val () =
         let
            datatype z = datatype Prim.t
            val dct = cty dst
            fun a k = Vector.sub (args, k)
            fun done r = store (dst, r, {clean = true})
            (* a register in which to compute dst *)
            fun target () =
               case dstReg dst of
                  SOME h => h
                | NONE => if isReal dct then xmm0 else rax
            fun cset cond =
               if fusing
                  then fused := SOME cond
               else
                  let
                     val d = target ()
                  in
                     emit (I.Setcc (cond, d))
                     ; ins ("movzbl", [R (d, Size.B), R (d, Size.L)])
                     ; done d
                  end
            val ptr = CType.CPointer
            val rct = CType.real
            fun wct ws = CType.word (ws, {signed = false})
            fun bits ws = Bits.toInt (WordSize.bits ws)
            fun constWord z =
               case z of
                  Operand.Cast (z, _) => constWord z
                | Operand.Const (Const.Word w) => SOME w
                | _ => NONE
            fun reads' (k, r) = List.contains (reads (a k), r, op =)
            (* dst' = a0 op a1, for an instruction "op src, dst" on integers
             * of type ct.  The flags are those of the operation, unless
             * useLea.
             *)
            fun alu (mn, comm, ct, dst', {useLea}) =
               let
                  val s = sz ct
                  val m = mn ^ Size.suffix s
                  fun rg r = R (r, s)
                  fun fin r = store (dst', r, {clean = true})
                  fun viaScratch () =
                     (load (a 0, rax, [rax, rdx])
                      ; ins (m, [opnd (a 1, [rcx, rdx], {imm = true}), rg rax])
                      ; fin rax)
                  fun leaIns (addr, h) =
                     ins (if size ct = 8 then "leaq" else "leal",
                          [Opnd.Mem addr, rg h])
               in
                  case dstReg dst' of
                     NONE => viaScratch ()
                   | SOME h =>
                        let
                           val h0 = home (a 0)
                           val h1 = home (a 1)
                           fun inPlace () =
                              (ins (m, [opnd (a 1, [rax, rcx], {imm = true}),
                                        rg h])
                               ; fin h)
                        in
                           if h0 = SOME h then inPlace ()
                           else
                              case (useLea andalso size ct >= 4, mn, h0, h1,
                                    immOf (a 1)) of
                                 (true, "add", SOME r0, _, SOME n) =>
                                    (leaIns (Emit.based (r0, IntInf.toInt n), h)
                                     ; fin h)
                               | (true, "add", SOME r0, SOME r1, _) =>
                                    (leaIns (Addr.T {base = SOME r0,
                                                     index = SOME (r1, 1),
                                                     disp = 0},
                                             h)
                                     ; fin h)
                               | (true, "sub", SOME r0, _, SOME n) =>
                                    if simm32 (~ n)
                                       then (leaIns (Emit.based
                                                     (r0, IntInf.toInt (~ n)),
                                                     h)
                                             ; fin h)
                                    else viaScratch ()
                               | _ =>
                                    if not (reads' (1, h))
                                       then (load (a 0, h, [rax, rcx])
                                             ; inPlace ())
                                    else if comm andalso h1 = SOME h
                                       then (ins (m, [opnd (a 0, [rax, rcx],
                                                            {imm = true}),
                                                      rg h])
                                             ; fin h)
                                    else viaScratch ()
                        end
               end
            fun bin (mn, comm, ct) = alu (mn, comm, ct, dst, {useLea = true})
            fun un (mn, ct) =
               let
                  val d = target ()
               in
                  load (a 0, d, [rax, rcx])
                  ; ins (mn ^ Size.suffix (sz ct), [R (d, sz ct)])
                  ; done d
               end
            fun shift (mn, ct) =
               let
                  val s = sz ct
                  val count =
                     case constWord (a 1) of
                        SOME w => Opnd.Imm (IntInf.andb (WordX.toIntInf w, 63))
                      | NONE => (load (a 1, rcx, [rcx, rdx]); R (rcx, Size.B))
                  val d = target ()
               in
                  load (a 0, d, [rax, rdx])
                  ; ins (mn ^ Size.suffix s, [count, R (d, s)])
                  ; done d
               end
            (* Put argument k (of size ws) in register r, sign extended (if
             * signed) to at least 32 bits.
             *)
            fun loadExt (k, r, ws, signed, scr) =
               (load (a k, r, scr)
                ; if signed
                     then (case bits ws of
                              8 => ins ("movsbl", [R (r, Size.B), R (r, Size.L)])
                            | 16 => ins ("movswl", [R (r, Size.W), R (r, Size.L)])
                            | _ => ())
                  else ())
            (* Compare x with y, as integers of type ct: the flags are those
             * of x - y.
             *)
            fun compare (x, y, ct) =
               let
                  val s = sz ct
                  val zx = opnd (x, [rax], {imm = false})
                  val zx =
                     case zx of
                        Opnd.Mem a =>
                           if isSome (home y) orelse isSome (immOf y)
                              then zx
                           else (ld (ct, rax, a); R (rax, s))
                      | _ => zx
                  val zy = opnd (y, [rcx, rdx], {imm = true})
               in
                  case (zx, zy) of
                     (Opnd.Reg _, Opnd.Imm 0) =>
                        ins ("test" ^ Size.suffix s, [zx, zx])
                   | _ => ins ("cmp" ^ Size.suffix s, [zy, zx])
               end
            (* cond holds if a0 ? a1; swapped, if a1 ? a0 *)
            fun cmp (ct, cond, swapped) =
               case (immOf (a 0), immOf (a 1)) of
                  (SOME _, NONE) => (compare (a 1, a 0, ct); cset swapped)
                | _ => (compare (a 0, a 1, ct); cset cond)
            (* reals *)
            fun fs ct = if size ct = 4 then "ss" else "sd"
            fun fopnd k = opnd (a k, [rax, rcx], {imm = false})
            fun fbin (mn, comm, ct) =
               let
                  val m = mn ^ fs ct
                  fun viaScratch () =
                     (load (a 0, xmm0, [rax, rcx])
                      ; ins (m, [fopnd 1, X xmm0])
                      ; done xmm0)
               in
                  case dstReg dst of
                     NONE => viaScratch ()
                   | SOME h =>
                        if home (a 0) = SOME h
                           then (ins (m, [fopnd 1, X h]); done h)
                        else if not (reads' (1, h))
                           then (load (a 0, h, [rax, rcx])
                                 ; ins (m, [fopnd 1, X h])
                                 ; done h)
                        else if comm andalso home (a 1) = SOME h
                           then (ins (m, [fopnd 0, X h]); done h)
                        else viaScratch ()
               end
            (* d = op a0, for an instruction "op src, dst" *)
            fun fun1 m =
               let
                  val d = target ()
               in
                  ins (m, [fopnd 0, X d]); done d
               end
            (* Combine a0 with a mask of the sign bit (or of all the other
             * bits).
             *)
            fun signMask (ct, mn, {sign}) =
               let
                  val d = target ()
                  val w = if size ct = 4 then "d" else "q"
               in
                  load (a 0, d, [rax, rcx])
                  ; ins ("pcmpeqd", [X xmm1, X xmm1])
                  ; if sign
                       then ins ("psll" ^ w, [imm (8 * size ct - 1), X xmm1])
                    else ins ("psrl" ^ w, [imm 1, X xmm1])
                  ; ins (mn, [X xmm1, X d])
                  ; done d
               end
            (* Compare x with y, as reals: afterwards, "above" means
             * x > y, "above or equal" x >= y (both false if unordered),
             * and "equal" that they are equal or unordered.
             *)
            fun fcompare (x, y, ct) =
               let
                  val rx = regOf (a x, xmm0, [rax, rcx])
               in
                  ins ("ucomi" ^ fs ct, [fopnd y, X rx])
               end
            datatype arith = Add | Mul | Neg | Sub
            (* an overflow check for an operation *)
            fun chk (opc: arith, (ws, {signed})) =
               let
                  val ct = wct ws
                  val s = sz ct
                  val n = bits ws
                  val sfx = Size.suffix s
                  (* the result of the operation, in rax, goes to "also" *)
                  fun result () =
                     Option.app (also, fn (z, _) =>
                                 store (z, rax, {clean = true}))
                  fun binary (mn, comm) =
                     case also of
                        SOME (z, _) => alu (mn, comm, ct, z, {useLea = false})
                      | NONE =>
                           (load (a 0, rax, [rax, rdx])
                            ; ins (mn ^ sfx, [opnd (a 1, [rcx, rdx],
                                                    {imm = true}),
                                              R (rax, s)]))
                  val cond =
                     case opc of
                        Add => (binary ("add", true)
                                ; if signed then Cond.O else Cond.B)
                      | Sub => (binary ("sub", false)
                                ; if signed then Cond.O else Cond.B)
                      | Neg =>
                           (load (a 0, rax, [rax, rcx])
                            ; ins ("neg" ^ sfx, [R (rax, s)])
                            ; result ()
                            (* the carry is set unless the operand is 0 *)
                            ; if signed then Cond.O else Cond.B)
                      | Mul =>
                           if signed andalso n >= 16
                              then (binary ("imul", true); Cond.O)
                           else if signed
                              then
                                 (loadExt (0, rax, ws, true, [rax, rdx])
                                  ; loadExt (1, rcx, ws, true, [rcx, rdx])
                                  ; ins ("imull", [R (rcx, Size.L),
                                                   R (rax, Size.L)])
                                  ; ins ("movsbl", [R (rax, Size.B),
                                                    R (rcx, Size.L)])
                                  ; ins ("cmpl", [R (rcx, Size.L),
                                                  R (rax, Size.L)])
                                  ; ins ("movzbl", [R (rax, Size.B),
                                                    R (rax, Size.L)])
                                  ; result ()
                                  ; Cond.NE)
                           else
                              (* rdx:rax (or, for bytes, ax) = rax * a1; the
                               * carry is set if the high half isn't 0
                               *)
                              (load (a 0, rax, [rax, rdx])
                               ; ins ("mul" ^ sfx,
                                      [opnd (a 1, [rcx, rdx], {imm = false})])
                               ; (case n of
                                     8 => ins ("movzbl", [R (rax, Size.B),
                                                          R (rax, Size.L)])
                                   | 16 => ins ("movzwl", [R (rax, Size.W),
                                                           R (rax, Size.L)])
                                   | _ => ())
                               ; result ()
                               ; Cond.B)
               in
                  cset cond
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
               case constWord (a 1) of
                  NONE => false
                | SOME w =>
                     let
                        val nb = bits ws
                        val N = if nb = 64 then 64 else 32
                        val s = if N = 64 then Size.Q else Size.L
                        val nct = if N = 64 then CType.Word64 else CType.Word32
                        val sfx = Size.suffix s
                        val dv = if signed then WordX.toIntInfX w
                                 else WordX.toIntInf w
                        fun rg r = R (r, s)
                        fun i2 (mn, x, y) = ins (mn ^ sfx, [x, y])
                        val clean = nb >= 32
                        (* as an immediate of an N-bit instruction *)
                        fun immN (k: IntInf.t): IntInf.t option =
                           if N = 32
                              then SOME (if k >= pow2 31 then k - pow2 32
                                         else k)
                           else if simm32 k then SOME k
                           else NONE
                        fun shiftBy (mn, k, r) =
                           if k > 0 then i2 (mn, imm k, rg r) else ()
                        (* the dividend is in rcx, the quotient in q *)
                        fun finish q =
                           if isRem
                              then
                                 let
                                    val t = if q = rax then rdx else rax
                                 in
                                    (case immN dv of
                                        SOME k =>
                                           ins ("imul" ^ sfx,
                                                [Opnd.Imm k, rg q, rg t])
                                      | NONE =>
                                           (movImm (t, dv)
                                            ; i2 ("imul", rg q, rg t)))
                                    ; i2 ("sub", rg t, rg rcx)
                                    ; store (dst, rcx, {clean = clean})
                                 end
                           else store (dst, q, {clean = clean})
                     in
                        if dv < 2 then false
                        else
                           ((case (log2 dv, signed) of
                                (SOME k, false) =>
                                   (load (a 0, rax, [rax, rcx])
                                    ; if isRem
                                         then (case immN (dv - 1) of
                                                  SOME m =>
                                                     i2 ("and", Opnd.Imm m,
                                                         rg rax)
                                                | NONE =>
                                                     (movImm (rcx, dv - 1)
                                                      ; i2 ("and", rg rcx,
                                                            rg rax)))
                                      else i2 ("shr", imm k, rg rax)
                                    ; store (dst, rax, {clean = true}))
                              | (SOME k, true) =>
                                   (* round toward zero: add 2^k - 1 to a
                                    * negative dividend
                                    *)
                                   (loadExt (0, rcx, ws, true, [rcx, rdx])
                                    ; mov (rax, rcx, nct)
                                    ; i2 ("sar", imm (N - 1), rg rax)
                                    ; i2 ("shr", imm (N - k), rg rax)
                                    ; i2 ("add", rg rcx, rg rax)
                                    ; if isRem
                                         then ((case immN (pow2 N - dv) of
                                                   SOME m =>
                                                      i2 ("and", Opnd.Imm m,
                                                          rg rax)
                                                 | NONE =>
                                                      (movImm (rdx, ~ dv)
                                                       ; i2 ("and", rg rdx,
                                                             rg rax)))
                                               ; i2 ("sub", rg rax, rg rcx)
                                               ; store (dst, rcx,
                                                        {clean = clean}))
                                      else (i2 ("sar", imm k, rg rax)
                                            ; store (dst, rax,
                                                     {clean = clean})))
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
                                                    "VibeAmd64Prim.divConst"
                                            else search (s + 1)
                                         end
                                      val (m, sh) = search 1
                                      val () =
                                         loadExt (0, rcx, ws, true, [rcx, rdx])
                                      (* q = the high half of n * m, with m
                                       * taken as signed
                                       *)
                                      val q =
                                         if N = 64
                                            then (movImm (rax, m)
                                                  ; ins ("imulq",
                                                         [R (rcx, Size.Q)])
                                                  ; rdx)
                                         else (ins ("movslq",
                                                    [R (rcx, Size.L),
                                                     R (rax, Size.Q)])
                                               ; ins ("imulq",
                                                      [Opnd.Imm
                                                       (valOf (immN m)),
                                                       R (rax, Size.Q),
                                                       R (rax, Size.Q)])
                                               ; ins ("sarq", [imm 32,
                                                               R (rax, Size.Q)])
                                               ; rax)
                                      val t = if q = rax then rdx else rax
                                   in
                                      if m >= pow2 (N - 1)
                                         then i2 ("add", rg rcx, rg q)
                                      else ()
                                      ; shiftBy ("sar", sh - 1, q)
                                      (* add 1 if the dividend is negative *)
                                      ; mov (t, rcx, nct)
                                      ; i2 ("shr", imm (N - 1), rg t)
                                      ; i2 ("add", rg t, rg q)
                                      ; finish q
                                   end
                              | (NONE, false) =>
                                   if N = 32
                                      then
                                         (* n < 2^32: one 64-bit multiply is
                                          * exact
                                          *)
                                         (load (a 0, rcx, [rcx, rdx])
                                          ; movImm (rax, pow2 64 div dv + 1)
                                          ; ins ("mulq", [R (rcx, Size.Q)])
                                          ; finish rdx)
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
                                                       "VibeAmd64Prim.divConst"
                                               else search (s + 1)
                                            end
                                         val (m, sh) = search 0
                                      in
                                         load (a 0, rcx, [rcx, rdx])
                                         ; if m < pow2 64
                                              then (movImm (rax, m)
                                                    ; ins ("mulq",
                                                           [R (rcx, Size.Q)])
                                                    ; shiftBy ("shr", sh, rdx))
                                           else
                                              (* m needs 65 bits *)
                                              (movImm (rax, m - pow2 64)
                                               ; ins ("mulq", [R (rcx, Size.Q)])
                                               ; mov (rax, rcx, nct)
                                               ; i2 ("sub", rg rdx, rg rax)
                                               ; i2 ("shr", imm 1, rg rax)
                                               ; i2 ("add", rg rax, rg rdx)
                                               ; shiftBy ("shr", sh - 1, rdx))
                                         ; finish rdx
                                      end)
                            ; true)
                     end
            fun divide (ws, signed, isRem) =
               if divConst (ws, signed, isRem) then ()
               else
                  let
                     val n = bits ws
                     val s = if n = 64 then Size.Q else Size.L
                     val sfx = Size.suffix s
                     (* the divisor: where it lives, or rcx *)
                     val dv =
                        case (n >= 32, home (a 1)) of
                           (true, SOME h) => h
                         | _ => (loadExt (1, rcx, ws, signed, [rcx, rdx]); rcx)
                  in
                     loadExt (0, rax, ws, signed, [rax, rdx])
                     ; if signed
                          then (ins (if n = 64 then "cqto" else "cltd", [])
                                ; ins ("idiv" ^ sfx, [R (dv, s)]))
                       else (ins ("xorl", [R (rdx, Size.L), R (rdx, Size.L)])
                             ; ins ("div" ^ sfx, [R (dv, s)]))
                     ; store (dst, if isRem then rdx else rax,
                              {clean = n >= 32})
                  end
            (* Argument k, as an integer of the same size, in a register:
             * where it lives, or r.
             *)
            fun intReg (k, r, scr) =
               let
                  val ct = cty (a k)
               in
                  if isReal ct
                     then
                        (case opnd (a k, scr, {imm = false}) of
                            Opnd.Mem m =>
                               ld (if size ct = 4 then CType.Word32
                                   else CType.Word64, r, m)
                          | z =>
                               if size ct = 4
                                  then ins ("movd", [z, R (r, Size.L)])
                               else ins ("movq", [z, R (r, Size.Q)])
                         ; r)
                  else regOf (a k, r, scr)
               end
            (* Compare-and-swap, of a value of type ct at the address that
             * mkAddr gives (which may use rcx and rdx).
             *)
            fun cas (ct, kOld, kNew, mkAddr: unit -> Addr.t) =
               let
                  val s = sz ct
                  val ict =
                     case size ct of
                        1 => CType.Word8
                      | 2 => CType.Word16
                      | 4 => CType.Word32
                      | _ => CType.Word64
                  val ad = mkAddr ()
                  (* free rdx, for the new value *)
                  val ad = if Addr.uses (ad, rdx)
                              then (lea (ad, rcx); Emit.based (rcx, 0))
                           else ad
                  val new = intReg (kNew, rdx, [rdx, rax])
                  val old = intReg (kOld, rax, [rax])
                  val () = mov (rax, old, ict)
               in
                  ins ("lock cmpxchg" ^ Size.suffix s,
                       [R (new, s), Opnd.Mem ad])
                  ; (case ct of
                        CType.Objptr =>
                           (callRuntime
                            ("Assignable_decheckObjptr", fn () =>
                             (load (a 0, rdi, [rcx, rdx])
                              ; mov (rsi, rax, ct)))
                            ; done rax)
                      | _ =>
                           if isReal ct
                              then
                                 let
                                    val d = target ()
                                 in
                                    if size ct = 4
                                       then ins ("movd", [R (rax, Size.L), X d])
                                    else ins ("movq", [R (rax, Size.Q), X d])
                                    ; done d
                                 end
                           else done rax)
               end
         in
            case prim of
               Array_cas (SOME ct) =>
                  cas (ct, 2, 3, fn () =>
                       let
                          val b = regOf (a 0, rcx, [rcx])
                          val i = regOf (a 1, rdx, [rdx])
                       in
                          Addr.T {base = SOME b, index = SOME (i, size ct),
                                  disp = 0}
                       end)
             | MLton_bogus =>
                  (* A select: dst = if a0 = a1 then a2 else a3.  Once the
                   * flags hold the condition, nothing may change them.
                   *)
                  let
                     val va = a 2
                     val vb = a 3
                     val w = valOf (constWord (a 1))
                     val c =
                        case selCond of
                           SOME c => if WordX.isZero w then Cond.negate c else c
                         | NONE => (compare (a 0, a 1, cty (a 0)); Cond.E)
                     val live = !flagsLive
                     val () = flagsLive := true
                     fun fin d = (done d; flagsLive := live)
                     fun q r = R (r, Size.Q)
                  in
                     if isReal dct
                        then
                           (* through the integer registers *)
                           let
                              val wct = if size dct = 4 then CType.Word32
                                        else CType.Word64
                              fun bits (z, r) =
                                 case opnd (z, [r, rdx], {imm = false}) of
                                    Opnd.Mem m => ld (wct, r, m)
                                  | x => ins ("movq", [x, q r])
                              val () = bits (vb, rax)
                              val () = bits (va, rcx)
                              val () = emit (I.Cmov (c, Size.Q, q rcx, rax))
                              val d = target ()
                           in
                              ins ("movq", [q rax, X d])
                              ; fin d
                           end
                     else
                        let
                           val d = target ()
                           (* an operand of a 64-bit conditional move *)
                           fun src z =
                              if size (cty z) = 8
                                 then (case opnd (z, [rcx, rdx],
                                                  {imm = false}) of
                                          Opnd.Reg (r, _) => q r
                                        | m => m)
                              else q (regOf (z, rcx, [rcx, rdx]))
                           fun set (c, b) =
                              (emit (I.Setcc (if b then c else Cond.negate c,
                                              d))
                               ; ins ("movzbl", [R (d, Size.B), R (d, Size.L)]))
                        in
                           (case (immOf va, immOf vb) of
                               (SOME 1, SOME 0) => set (c, true)
                             | (SOME 0, SOME 1) => set (c, false)
                             | _ =>
                                  if home vb = SOME d
                                     then emit (I.Cmov (c, Size.Q, src va, d))
                                  else if home va = SOME d
                                     then emit (I.Cmov (Cond.negate c, Size.Q,
                                                        src vb, d))
                                  else if not (reads' (2, d))
                                     then (load (vb, d, [rdx, rcx])
                                           ; emit (I.Cmov (c, Size.Q, src va,
                                                           d)))
                                  else (load (va, rcx, [rcx, rdx])
                                        ; load (vb, d, [rdx])
                                        ; emit (I.Cmov (c, Size.Q, q rcx, d))))
                           ; fin d
                        end
                  end
             | CPointer_add => bin ("add", true, ptr)
             | CPointer_diff => bin ("sub", false, ptr)
             | CPointer_equal => cmp (ptr, Cond.E, Cond.E)
             | CPointer_fromWord => move {dst = dst, src = a 0}
             | CPointer_lt => cmp (ptr, Cond.B, Cond.A)
             | CPointer_sub => bin ("sub", false, ptr)
             | CPointer_toWord => move {dst = dst, src = a 0}
             | Real_Math_sqrt rs => fun1 ("sqrt" ^ fs (rct rs))
             | Real_abs rs => signMask (rct rs, "andps", {sign = false})
             | Real_add rs => fbin ("add", true, rct rs)
             | Real_castToWord (rs, _) =>
                  let
                     val d = target ()
                  in
                     (case fopnd 0 of
                         Opnd.Mem m => ld (dct, d, m)
                       | z =>
                            if size (rct rs) = 4
                               then ins ("movd", [z, R (d, Size.L)])
                            else ins ("movq", [z, R (d, Size.Q)]))
                     ; done d
                  end
             | Real_div rs => fbin ("div", false, rct rs)
             | Real_equal rs =>
                  (* equal, and not unordered *)
                  if Operand.equals (a 0, a 1)
                     (* not a NaN *)
                     then (fcompare (0, 1, rct rs); cset Cond.NP)
                  else
                     let
                        val ct = rct rs
                     in
                        (* a mask of all ones, if so *)
                        load (a 0, xmm0, [rax, rcx])
                        ; ins ("cmpeq" ^ fs ct, [fopnd 1, X xmm0])
                        ; if size ct = 4
                             then (ins ("movd", [X xmm0, R (rax, Size.L)])
                                   ; ins ("testl", [R (rax, Size.L),
                                                    R (rax, Size.L)]))
                          else (ins ("movq", [X xmm0, R (rax, Size.Q)])
                                ; ins ("testq", [R (rax, Size.Q),
                                                 R (rax, Size.Q)]))
                        ; cset Cond.NE
                     end
             | Real_le rs => (fcompare (1, 0, rct rs); cset Cond.AE)
             | Real_lt rs => (fcompare (1, 0, rct rs); cset Cond.A)
             | Real_mul rs => fbin ("mul", true, rct rs)
             | Real_neg rs => signMask (rct rs, "xorps", {sign = true})
             | Real_qequal rs => (fcompare (0, 1, rct rs); cset Cond.E)
             | Real_rndToReal (rs, rs') =>
                  if RealSize.equals (rs, rs')
                     then move {dst = dst, src = a 0}
                  else
                     let
                        val d = target ()
                     in
                        (* break the dependency on the old value of d *)
                        if List.contains (reads (a 0), d, op =) then ()
                        else ins ("xorps", [X d, X d])
                        ; ins (if size (rct rs) = 4 then "cvtss2sd"
                               else "cvtsd2ss",
                               [fopnd 0, X d])
                        ; done d
                     end
             | Real_rndToWord (rs, ws, {signed}) =>
                  let
                     val n = bits ws
                     val cvt = "cvtt" ^ fs (rct rs) ^ "2si"
                  in
                     if signed andalso n <= 32
                        then (ins (cvt ^ "l", [fopnd 0, R (rax, Size.L)])
                              ; store (dst, rax, {clean = n = 32}))
                     else if signed orelse n <= 32
                        then (ins (cvt ^ "q", [fopnd 0, R (rax, Size.Q)])
                              ; store (dst, rax, {clean = n = 64}))
                     else
                        (* At 2^63 and above, convert x - 2^63 and add 2^63
                         * back: the first conversion then gives 2^63
                         * (0x8000000000000000), and what the second gives
                         * is "or"ed in.
                         *)
                        let
                           val ct = rct rs
                           val x = regOf (a 0, xmm0, [rax, rcx])
                           val two63 =
                              if size ct = 4 then 0x5F000000
                              else 0x43E0000000000000
                        in
                           ins (cvt ^ "q", [X x, R (rax, Size.Q)])
                           ; mov (xmm1, x, ct)
                           ; ins ("sub" ^ fs ct,
                                  [Opnd.Mem (literal (size ct, two63)),
                                   X xmm1])
                           ; ins (cvt ^ "q", [X xmm1, R (rdx, Size.Q)])
                           ; ins ("movq", [R (rax, Size.Q), R (rcx, Size.Q)])
                           ; ins ("sarq", [imm 63, R (rcx, Size.Q)])
                           ; ins ("andq", [R (rcx, Size.Q), R (rdx, Size.Q)])
                           ; ins ("orq", [R (rdx, Size.Q), R (rax, Size.Q)])
                           ; done rax
                        end
                  end
             | Real_sub rs => fbin ("sub", false, rct rs)
             | Ref_cas (SOME ct) =>
                  cas (ct, 1, 2, fn () =>
                       Emit.based (regOf (a 0, rcx, [rcx]), 0))
             | Word_add ws => bin ("add", true, wct ws)
             | Word_addCheckP z => chk (Add, z)
             | Word_andb ws => bin ("and", true, wct ws)
             | Word_castToReal (ws, _) =>
                  let
                     val d = target ()
                  in
                     (case opnd (a 0, [rax, rcx], {imm = false}) of
                         Opnd.Mem m => ld (dct, d, m)
                       | z =>
                            if bits ws = 32
                               then ins ("movd", [z, X d])
                            else ins ("movq", [z, X d]))
                     ; done d
                  end
             | Word_equal ws => cmp (wct ws, Cond.E, Cond.E)
             | Word_extdToWord (ws1, ws2, {signed}) =>
                  let
                     val n1 = bits ws1
                     val n2 = bits ws2
                     val d = target ()
                     fun src () = opnd (a 0, [rax, rcx], {imm = false})
                     (* the low bits of the source, at size s *)
                     fun low s =
                        case src () of
                           Opnd.Reg (r, _) => R (r, s)
                         | z => z
                  in
                     (if n1 = n2 orelse (n1 > n2 andalso n2 = 64)
                         then load (a 0, d, [rax, rcx])
                      else if n1 > n2
                         then
                            (* A truncation must clear the upper bits of
                             * the register, even when it is the source's.
                             *)
                            (case n2 of
                                8 => ins ("movzbl", [low Size.B, R (d, Size.L)])
                              | 16 => ins ("movzwl", [low Size.W, R (d, Size.L)])
                              | _ => ins ("movl", [low Size.L, R (d, Size.L)]))
                      else if signed
                         then
                            (case (n1, n2) of
                                (8, 16) =>
                                   (ins ("movsbl", [low Size.B, R (d, Size.L)])
                                    ; ins ("movzwl", [R (d, Size.W),
                                                      R (d, Size.L)]))
                              | (8, 32) =>
                                   ins ("movsbl", [low Size.B, R (d, Size.L)])
                              | (8, _) =>
                                   ins ("movsbq", [low Size.B, R (d, Size.Q)])
                              | (16, 32) =>
                                   ins ("movswl", [low Size.W, R (d, Size.L)])
                              | (16, _) =>
                                   ins ("movswq", [low Size.W, R (d, Size.Q)])
                              | _ =>
                                   ins ("movslq", [low Size.L, R (d, Size.Q)]))
                      else load (a 0, d, [rax, rcx]))
                     ; done d
                  end
             | Word_lshift ws => shift ("shl", wct ws)
             | Word_lt (ws, {signed}) =>
                  if signed then cmp (wct ws, Cond.L, Cond.G)
                  else cmp (wct ws, Cond.B, Cond.A)
             | Word_mul (ws, _) =>
                  let
                     val ct = wct ws
                     val s = sz ct
                     val n = bits ws
                     fun pow k =
                        let
                           val d = target ()
                        in
                           load (a 0, d, [rax, rcx])
                           ; if k > 0
                                then ins ("shl" ^ Size.suffix s,
                                          [imm k, R (d, s)])
                             else ()
                           ; done d
                        end
                  in
                     if n < 32
                        then (load (a 0, rax, [rax, rdx])
                              ; load (a 1, rcx, [rcx, rdx])
                              ; ins ("imull", [R (rcx, Size.L), R (rax, Size.L)])
                              ; store (dst, rax, {clean = false}))
                     else
                        case constWord (a 1) of
                           NONE => bin ("imul", true, ct)
                         | SOME w =>
                              let
                                 val c = WordX.toIntInf w
                              in
                                 case (log2 c, log2 (c - 1), home (a 0),
                                       immOf (a 1)) of
                                    (SOME k, _, _, _) => pow k
                                  | (_, SOME k, SOME r, _) =>
                                       if n >= 32 andalso 1 <= k andalso k <= 3
                                          then
                                             let
                                                val d = target ()
                                             in
                                                ins (if n = 64 then "leaq"
                                                     else "leal",
                                                     [Opnd.Mem
                                                      (Addr.T
                                                       {base = SOME r,
                                                        index =
                                                        SOME (r, IntInf.toInt
                                                                 (c - 1)),
                                                        disp = 0}),
                                                      R (d, s)])
                                                ; done d
                                             end
                                       else bin ("imul", true, ct)
                                  | (_, _, _, SOME c') =>
                                       let
                                          val d = target ()
                                       in
                                          ins ("imul" ^ Size.suffix s,
                                               [Opnd.Imm c',
                                                opnd (a 0, [rax, rcx],
                                                      {imm = false}),
                                                R (d, s)])
                                          ; done d
                                       end
                                  | _ => bin ("imul", true, ct)
                              end
                  end
             | Word_mulCheckP z => chk (Mul, z)
             | Word_neg ws => un ("neg", wct ws)
             | Word_negCheckP z => chk (Neg, z)
             | Word_notb ws => un ("not", wct ws)
             | Word_orb ws => bin ("or", true, wct ws)
             | Word_quot (ws, {signed}) => divide (ws, signed, false)
             | Word_rem (ws, {signed}) => divide (ws, signed, true)
             | Word_rndToReal (ws, rs, {signed}) =>
                  let
                     val n = bits ws
                     val ct = rct rs
                     val d = target ()
                     val cvt = "cvtsi2" ^ fs ct
                     val () = ins ("xorps", [X d, X d])
                  in
                     (if signed andalso n >= 32
                         then ins (cvt ^ (if n = 64 then "q" else "l"),
                                   [opnd (a 0, [rax, rcx], {imm = false}),
                                    X d])
                      else if signed
                         then (loadExt (0, rax, ws, true, [rax, rcx])
                               ; ins (cvt ^ "l", [R (rax, Size.L), X d]))
                      else if n < 64
                         (* it is zero-extended *)
                         then ins (cvt ^ "q",
                                   [R (regOf (a 0, rax, [rax, rcx]), Size.Q),
                                    X d])
                      else
                         (* With the top bit set: halve it (keeping the low
                          * bit, for the rounding), convert, and double.
                          *)
                         let
                            val r = regOf (a 0, rax, [rax, rcx])
                            fun q r = R (r, Size.Q)
                         in
                            ins ("testq", [q r, q r])
                            ; emit (I.Jcc (Cond.L, Tgt.Forward 1))
                            ; ins (cvt ^ "q", [q r, X d])
                            ; emit (I.Jmp (Tgt.Forward 2))
                            ; emit (I.LocalLabel 1)
                            ; ins ("movq", [q r, q rcx])
                            ; ins ("movl", [R (r, Size.L), R (rdx, Size.L)])
                            ; ins ("shrq", [imm 1, q rcx])
                            ; ins ("andl", [imm 1, R (rdx, Size.L)])
                            ; ins ("orq", [q rdx, q rcx])
                            ; ins (cvt ^ "q", [q rcx, X d])
                            ; ins ("add" ^ fs ct, [X d, X d])
                            ; emit (I.LocalLabel 2)
                         end)
                     ; done d
                  end
             | Word_rol ws => shift ("rol", wct ws)
             | Word_ror ws => shift ("ror", wct ws)
             | Word_rshift (ws, {signed}) =>
                  shift (if signed then "sar" else "shr", wct ws)
             | Word_sub ws => bin ("sub", false, wct ws)
             | Word_subCheckP z => chk (Sub, z)
             | Word_xorb ws => bin ("xor", true, wct ws)
             | _ => Error.bug (concat ["VibeAmd64Prim.primApp: ",
                                       Prim.toString prim])
         end
   in
      !fused
   end

fun selReal ({access, emit = e}: t, cmp: Items.app, sels: Items.app list) =
   let
      val {defined, dstReg, load, opnd, store, ...} = access
      val {ins, ...} = e
      fun arg ({args, ...}: Items.app, k) = Vector.sub (args, k)
      fun fs ct = if size ct = 4 then "ss" else "sd"
      (* the comparison: its mnemonic, and x and y such that it is x < y
       * (if strict) or x <= y
       *)
      val z =
         case #prim cmp of
            Prim.Real_lt rs => SOME ("cmplt", CType.real rs, true)
          | Prim.Real_le rs => SOME ("cmple", CType.real rs, false)
          | _ => NONE
   in
      case z of
         NONE => false
       | SOME (mn, ct, strict) =>
            let
               val x = arg (cmp, 0)
               val y = arg (cmp, 1)
               (* as p < q *)
               val (p, q) = if strict then (x, y) else (y, x)
               (* the values of a select, for when x ? y holds and for
                * when it doesn't
                *)
               fun values s =
                  case arg (s, 1) of
                     Operand.Const (Const.Word w) =>
                        if WordX.isZero w then (arg (s, 3), arg (s, 2))
                        else (arg (s, 2), arg (s, 3))
                   | _ => Error.bug "VibeAmd64Prim.selReal"
               (* the same, for when p < q holds and for when it doesn't *)
               fun values' s =
                  let
                     val (a, b) = values s
                  in
                     if strict then (a, b) else (b, a)
                  end
               fun same (z, z') = Operand.equals (z, z')
               (* Is the select a minimum or a maximum of p and q?  It
                * can't be output as one if an earlier select has changed
                * p or q.
                *)
               datatype kind = Min | Max | Blend
               val (kinds, _) =
                  List.fold
                  (sels, ([], []), fn (s, (ks, written)) =>
                   let
                      val (a, b) = values' s
                      val clean =
                         not (List.exists
                              (written, fn d =>
                               Operand.interfere (d, p)
                               orelse Operand.interfere (d, q)))
                      val k =
                         if clean andalso CType.equals (cty (#dst s), ct)
                            then if same (a, p) andalso same (b, q) then Min
                                 else if same (a, q) andalso same (b, p)
                                    then Max
                                 else Blend
                         else Blend
                   in
                      (k :: ks, #dst s :: written)
                   end)
               val kinds = List.rev kinds
               val ok =
                  List.forall
                  (sels, fn s =>
                   isReal (cty (#dst s))
                   andalso size (cty (#dst s)) <= size ct)
                  andalso
                  List.forall2
                  (sels, kinds, fn (s, k) =>
                   case k of
                      Blend => isSome (dstReg (#dst s))
                    | _ => true)
               fun rm z = opnd (z, [rax, rcx], {imm = false})
            in
               if not ok then false
               else
                  (if List.exists (kinds, fn k => k = Blend)
                      (* xmm0 = all ones if x ? y, else zero *)
                      then (load (x, xmm0, [rax, rcx])
                            ; ins (mn ^ fs ct, [rm y, X xmm0]))
                   else ()
                   ; defined (#dst cmp)
                   ; List.foreach2
                     (sels, kinds, fn (s, k) =>
                      let
                         val dst = #dst s
                      in
                         (case k of
                             (* min (a, b) is a if a < b, else b; max (a, b)
                              * is a if a > b, else b
                              *)
                             Min => (load (p, xmm1, [rax, rcx])
                                     ; ins ("min" ^ fs ct, [rm q, X xmm1])
                                     ; store (dst, xmm1, {clean = true}))
                           | Max => (load (q, xmm1, [rax, rcx])
                                     ; ins ("max" ^ fs ct, [rm p, X xmm1])
                                     ; store (dst, xmm1, {clean = true}))
                           | Blend =>
                                (* b xor ((a xor b) and mask) *)
                                let
                                   val (a, b) = values s
                                   val h = valOf (dstReg dst)
                                in
                                   load (a, xmm1, [rax, rcx])
                                   ; load (b, h, [rax, rcx])
                                   ; ins ("xorps", [X h, X xmm1])
                                   ; ins ("andps", [X xmm0, X xmm1])
                                   ; ins ("xorps", [X xmm1, X h])
                                   ; store (dst, h, {clean = true})
                                end)
                         ; defined dst
                      end)
                   ; true)
            end
   end

fun app (z, a) = ignore (primApp (z, a, {flags = false, selCond = NONE}))
fun appFlags (z, a) = primApp (z, a, {flags = true, selCond = NONE})
fun sel (z, a, c) = ignore (primApp (z, a, {flags = false, selCond = c}))

end

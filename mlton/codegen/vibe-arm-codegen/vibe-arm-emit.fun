(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor VibeArmEmit (S: VIBE_ARM_EMIT_STRUCTS): VIBE_ARM_EMIT =
struct

open S
open Machine

structure Addr = Arm64.Addr
structure I = Arm64.Instr
structure Lbl = Arm64.Label
structure Opnd = Arm64.Operand
structure R = Arm64.Reg
structure Tgt = Arm64.Target

fun isReal ct =
   case ct of CType.Real32 => true | CType.Real64 => true | _ => false
fun size ct = Bytes.toInt (CType.size ct)

(* Registers: 0-31 are x0-x30,sp; 32-63 are d0-d31. *)
fun x r = if r = 31 then R.SP else R.X r
(* register r, at the width of a value of type ct *)
fun reg (r, ct) =
   if r >= 32
      then (if size ct = 4 then R.S (r - 32) else R.D (r - 32))
   else if size ct = 8 then x r else R.W r
val regOp = Opnd.Reg
fun immOp n = Opnd.Imm (IntInf.fromInt n)
fun scratch (ct, k) = if isReal ct then 48 + k else k

fun pow2 k = IntInf.<< (1, Word.fromInt k)
(* k such that n = 2^k *)
fun log2 (n: IntInf.t): int option =
   let
      fun loop k =
         if k > 64 then NONE
         else if pow2 k = n then SOME k
         else loop (k + 1)
   in
      loop 0
   end
(* Can n be the immediate of a logical instruction on N-bit
 * registers?  It must be a repetition of an e-bit pattern that
 * is a rotation of a run of ones.
 *)
fun bitmask (n: IntInf.t, N: int): bool =
   List.exists
   ([2, 4, 8, 16, 32, 64], fn e =>
    e <= N
    andalso
    let
       val m = pow2 e - 1
       val p = IntInf.andb (n, m)
       fun rep (k, ac) =
          if k >= N then ac
          else rep (k + e,
                    IntInf.orb (ac, IntInf.<< (p, Word.fromInt k)))
       fun strip q =
          if IntInf.andb (q, 1) = 0
             then strip (IntInf.~>> (q, 0w1))
          else q
       fun run q = IntInf.andb (strip q, strip q + 1) = 0
    in
       rep (0, 0) = n andalso p <> 0 andalso p <> m
       andalso (run p orelse run (m - p))
    end)

type t = {emit: I.t -> unit,
          label: Lbl.t -> unit,
          branch: Lbl.t -> unit,
          ldr: CType.t * int * Addr.t -> unit,
          str: CType.t * int * Addr.t -> unit,
          mov: int * int * CType.t -> unit,
          movImm: int * IntInf.t -> unit,
          movInt: int * int -> unit,
          addImm: int * int * int -> unit,
          mem: int * int * CType.t -> Addr.t,
          symAddr: int * string * {got: bool} -> unit,
          instrs: unit -> I.t list}

fun new (): t =
   let
      (* the instructions of the chunk, the most recent first *)
      val code: I.t list ref = ref []
      fun emit ins = List.push (code, ins)
      fun label l = emit (I.Label l)
      fun branch l = emit (I.B (Tgt.Label l))
      (* load/store register r, as a value of type ct *)
      fun ldr (ct, r, a) =
         emit (I.Load (case size ct of 1 => I.LDRB | 2 => I.LDRH | _ => I.LDR,
                       reg (r, ct), a))
      fun str (ct, r, a) =
         emit (I.Store (case size ct of 1 => I.STRB | 2 => I.STRH | _ => I.STR,
                        reg (r, ct), a))
      fun mov (d, s, ct) =
         if d = s then ()
         else emit (I.Un (if d >= 32 then I.FMOV else I.MOV,
                          reg (d, ct), reg (s, ct)))

      fun movImm (r, n: IntInf.t) =
         let
            val top = IntInf.<< (1, 0w64)
            val n = if n < 0 then n + top else n
            fun chunk k =
               IntInf.toInt (IntInf.andb (IntInf.~>> (n, Word.fromInt (16 * k)),
                                          0xFFFF))
            fun mk (opc, k) = emit (opc (x r, chunk k, 16 * k))
         in
            if n >= top - 0x10000
               then emit (I.MovImm (x r, n - top))
            else (case List.keepAll ([0, 1, 2, 3], fn k => chunk k <> 0) of
                     [] => emit (I.MovImm (x r, 0))
                   | k :: ks => (mk (I.Movz, k)
                                 ; List.foreach (ks, fn k => mk (I.Movk, k))))
         end
      fun movInt (r, n) = movImm (r, IntInf.fromInt n)
      (* d = s + n; may clobber x17 *)
      fun addImm (d, s, n) =
         if n = 0 then (if d = s then () else emit (I.Un (I.MOV, x d, x s)))
         else
            let
               val opc = if n > 0 then I.ADD else I.SUB
               val n = Int.abs n
            in
               if n < 4096
                  then emit (I.Bin (opc, x d, x s, immOp n))
               else (movInt (17, n)
                     ; emit (I.Bin (opc, x d, x s, regOp (R.X 17))))
            end
      (* an addressing mode for [b + off]; may clobber x17 *)
      fun mem (b, off, ct) =
         if (~256 <= off andalso off < 256)
            orelse (off >= 0 andalso off mod size ct = 0
                    andalso off div size ct < 4096)
            then Addr.Offset (x b, off)
         else (movInt (17, off); Addr.Index (x b, R.X 17, 0))
      (* the address of a symbol, directly or from the global offset table *)
      fun symAddr (r, s, {got}) =
         let
            val s = Lbl.Sym s
         in
            emit (I.Adrp (x r, s, {got = got}))
            ; emit (if got
                       then I.LoadGot (x r, x r, s)
                    else I.AddLo (x r, x r, s))
         end
   in
      {emit = emit,
       label = label,
       branch = branch,
       ldr = ldr,
       str = str,
       mov = mov,
       movImm = movImm,
       movInt = movInt,
       addImm = addImm,
       mem = mem,
       symAddr = symAddr,
       instrs = fn () => List.rev (!code)}
   end

end

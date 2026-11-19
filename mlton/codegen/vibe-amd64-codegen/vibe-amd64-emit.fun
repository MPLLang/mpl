(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor VibeAmd64Emit (S: VIBE_AMD64_EMIT_STRUCTS): VIBE_AMD64_EMIT =
struct

open S
open Machine

structure Addr = Asm.Addr
structure I = Asm.Instr
structure Lbl = Asm.Label
structure Opnd = Asm.Operand
structure Size = Asm.Size
structure Tgt = Asm.Target

val rax = 0
val rcx = 1
val rdx = 2
val rsp = 4
val rsi = 6
val rdi = 7
val r10 = 10
val r11 = 11
val xmm0 = 16
val xmm1 = 17
val gcState = 13
val stackTop = 5
val frontier = 12

fun isReal ct =
   case ct of CType.Real32 => true | CType.Real64 => true | _ => false
fun size ct = Bytes.toInt (CType.size ct)
fun sz ct = Size.fromBytes (size ct)
fun reg (r, ct) = Opnd.Reg (r, if r >= 16 then Size.Q else sz ct)
fun based (b, disp) = Addr.T {base = SOME b, index = NONE, disp = disp}

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
fun simm32 (n: IntInf.t) = ~ (pow2 31) <= n andalso n < pow2 31

type t = {emit: I.t -> unit,
          ins: string * Opnd.t list -> unit,
          label: Lbl.t -> unit,
          jmp: Lbl.t -> unit,
          ld: CType.t * int * Addr.t -> unit,
          st: CType.t * int * Addr.t -> unit,
          mov: int * int * CType.t -> unit,
          movImm: int * IntInf.t -> unit,
          lea: Addr.t * int -> unit,
          flagsLive: bool ref,
          literal: int * IntInf.t -> Addr.t,
          symAddr: int * string * {got: bool} -> unit,
          instrs: unit -> I.t list}

fun new {name: string}: t =
   let
      (* the instructions of the chunk, the most recent first *)
      val code: I.t list ref = ref []
      fun emit ins = List.push (code, ins)
      fun ins z = emit (I.Ins z)
      fun label l = emit (I.Label l)
      fun jmp l = emit (I.Jmp (Tgt.Label l))
      val flagsLive = ref false
      fun ld (ct, r, a) =
         let
            val m = Opnd.Mem a
         in
            if r >= 16
               then ins (if size ct = 4 then "movss" else "movsd",
                         [m, Opnd.Reg (r, Size.Q)])
            else
               case size ct of
                  1 => ins ("movzbl", [m, Opnd.Reg (r, Size.L)])
                | 2 => ins ("movzwl", [m, Opnd.Reg (r, Size.L)])
                | 4 => ins ("movl", [m, Opnd.Reg (r, Size.L)])
                | _ => ins ("movq", [m, Opnd.Reg (r, Size.Q)])
         end
      fun st (ct, r, a) =
         let
            val m = Opnd.Mem a
         in
            if r >= 16
               then ins (if size ct = 4 then "movss" else "movsd",
                         [Opnd.Reg (r, Size.Q), m])
            else ins ("mov" ^ Size.suffix (sz ct), [Opnd.Reg (r, sz ct), m])
         end
      fun mov (d, s, ct) =
         if d = s then ()
         else if d >= 16
            then ins ("movaps", [Opnd.Reg (s, Size.Q), Opnd.Reg (d, Size.Q)])
         else if size ct = 8
            then ins ("movq", [Opnd.Reg (s, Size.Q), Opnd.Reg (d, Size.Q)])
         else ins ("movl", [Opnd.Reg (s, Size.L), Opnd.Reg (d, Size.L)])
      fun movImm (r, n: IntInf.t) =
         let
            val top = pow2 64
            val n = if n < 0 then n + top else n
         in
            if n = 0 andalso not (!flagsLive)
               then ins ("xorl", [Opnd.Reg (r, Size.L), Opnd.Reg (r, Size.L)])
            else if n < pow2 32
               then ins ("movl", [Opnd.Imm n, Opnd.Reg (r, Size.L)])
            else if n >= top - pow2 31
               then ins ("movq", [Opnd.Imm (n - top), Opnd.Reg (r, Size.Q)])
            else ins ("movabsq", [Opnd.Imm n, Opnd.Reg (r, Size.Q)])
         end
      fun lea (a, r) = ins ("leaq", [Opnd.Mem a, Opnd.Reg (r, Size.Q)])
      (* the literals: their labels, sizes, and bits *)
      val literals: (Lbl.t * int * IntInf.t) list ref = ref []
      fun literal (bytes, bits) =
         case List.peek (!literals, fn (_, bytes', bits') =>
                         bytes = bytes' andalso bits = bits') of
            SOME (l, _, _) => Addr.Rip (l, 0)
          | NONE =>
               let
                  val l = Lbl.Local (concat ["vibeLit_", name, "_",
                                             Int.toString
                                             (List.length (!literals))])
               in
                  List.push (literals, (l, bytes, bits))
                  ; Addr.Rip (l, 0)
               end
      fun symAddr (r, s, {got}) =
         if got
            then ins ("movq", [Opnd.Mem (Addr.Got (Lbl.Sym s)),
                               Opnd.Reg (r, Size.Q)])
         else lea (Addr.Rip (Lbl.Sym s, 0), r)
      fun instrs () =
         let
            fun lits (bytes, section, p2, mk) =
               case List.keepAll (List.rev (!literals), fn (_, b, _) =>
                                  b = bytes) of
                  [] => []
                | ls => I.Section section :: I.P2align p2
                        :: List.concatMap (ls, fn (l, _, bits) =>
                                           [I.Label l, mk bits])
         in
            List.rev (!code)
            @ lits (8, Asm.Section.Literal8, 3, I.QuadImm)
            @ lits (4, Asm.Section.Literal4, 2, I.LongImm)
         end
   in
      {emit = emit,
       ins = ins,
       label = label,
       jmp = jmp,
       ld = ld,
       st = st,
       mov = mov,
       movImm = movImm,
       lea = lea,
       flagsLive = flagsLive,
       literal = literal,
       symAddr = symAddr,
       instrs = instrs}
   end

end

(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor VibeAmd64Asm (S: VIBE_AMD64_ASM_STRUCTS): VIBE_AMD64_ASM =
struct

open S

fun int (n: int): string =
   if n >= 0 then Int.toString n else "-" ^ Int.toString (~ n)

fun intInf (n: IntInf.t): string =
   if n >= 0 then IntInf.toString n else "-" ^ IntInf.toString (~ n)

structure Size =
   struct
      datatype t = B | W | L | Q

      val fromBytes =
         fn 1 => B
          | 2 => W
          | 4 => L
          | 8 => Q
          | _ => Error.bug "VibeAmd64Asm.Size.fromBytes"

      val suffix =
         fn B => "b" | W => "w" | L => "l" | Q => "q"
   end

structure Reg =
   struct
      val names = Vector.fromList ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"]

      fun toString (r: int, s: Size.t): string =
         if r >= 16 then "%xmm" ^ int (r - 16)
         else if r >= 8
            then concat ["%r", int r,
                         case s of
                            Size.B => "b" | Size.W => "w"
                          | Size.L => "d" | Size.Q => ""]
         else
            let
               val n = Vector.sub (names, r)
            in
               case s of
                  Size.B => if r < 4 then concat ["%", String.substring (n, 0, 1), "l"]
                            else concat ["%", n, "l"]
                | Size.W => "%" ^ n
                | Size.L => "%e" ^ n
                | Size.Q => "%r" ^ n
            end
   end

structure Cond =
   struct
      datatype t =
         E | NE
       | B | AE
       | A | BE
       | L | GE
       | G | LE
       | O | NO
       | P | NP

      val negate =
         fn E => NE | NE => E
          | B => AE | AE => B
          | A => BE | BE => A
          | L => GE | GE => L
          | G => LE | LE => G
          | O => NO | NO => O
          | P => NP | NP => P

      val toString =
         fn E => "e" | NE => "ne"
          | B => "b" | AE => "ae"
          | A => "a" | BE => "be"
          | L => "l" | GE => "ge"
          | G => "g" | LE => "le"
          | O => "o" | NO => "no"
          | P => "p" | NP => "np"
   end

structure Label =
   struct
      datatype t =
         Sym of string
       | Local of string
       | Plain of string

      val equals: t * t -> bool = op =

      fun toString (l: t): string =
         case l of
            Sym s => if !Control.labelsHaveExtra_ then "_" ^ s else s
          | Local s =>
               (case !Control.Target.os of
                   MLton.Platform.OS.Darwin => "L" ^ s
                 | _ => ".L" ^ s)
          | Plain s => s
   end

structure Target =
   struct
      datatype t =
         Label of Label.t
       | Forward of int
       | Backward of int

      val toString =
         fn Label l => Label.toString l
          | Forward n => int n ^ "f"
          | Backward n => int n ^ "b"
   end

structure Addr =
   struct
      datatype t =
         T of {base: int option,
               index: (int * int) option,
               disp: int}
       | Rip of Label.t * int
       | Got of Label.t

      fun uses (a, r) =
         case a of
            T {base, index, ...} =>
               (case base of
                   SOME b => b = r
                 | NONE => false)
               orelse (case index of
                          SOME (i, _) => i = r
                        | NONE => false)
          | _ => false

      fun toString (a: t): string =
         case a of
            T {base, index, disp} =>
               concat [if disp = 0 andalso isSome base then "" else int disp,
                       "(",
                       case base of
                          SOME b => Reg.toString (b, Size.Q)
                        | NONE => "",
                       case index of
                          SOME (i, s) =>
                             concat [",", Reg.toString (i, Size.Q), ",", int s]
                        | NONE => "",
                       ")"]
          | Rip (l, disp) =>
               concat [Label.toString l,
                       if disp = 0 then ""
                       else if disp > 0 then "+" ^ int disp
                       else int disp,
                       "(%rip)"]
          | Got l => Label.toString l ^ "@GOTPCREL(%rip)"
   end

structure Operand =
   struct
      datatype t =
         Reg of int * Size.t
       | Imm of IntInf.t
       | Mem of Addr.t

      fun uses (z, r) =
         case z of
            Reg (r', _) => r = r'
          | Imm _ => false
          | Mem a => Addr.uses (a, r)

      val toString =
         fn Reg r => Reg.toString r
          | Imm n => "$" ^ intInf n
          | Mem a => Addr.toString a
   end

structure Section =
   struct
      datatype t =
         Text
       | RelRo
       | Literal4
       | Literal8
   end

structure Instr =
   struct
      datatype t =
         Label of Label.t
       | LocalLabel of int
       | Section of Section.t
       | P2align of int
       | Globl of Label.t
       | Hidden of Label.t
       | Long of Label.t * Label.t
       | Quad of Label.t option
       | LongImm of IntInf.t
       | QuadImm of IntInf.t
       | Ins of string * Operand.t list
       | Cmov of Cond.t * Size.t * Operand.t * int
       | Setcc of Cond.t * int
       | Jmp of Target.t
       | Jcc of Cond.t * Target.t
       | JmpInd of Operand.t
       | Call of Label.t
       | CallInd of Operand.t
       | Ret

      fun toString (ins: t): string =
         let
            val darwin = !Control.Target.os = MLton.Platform.OS.Darwin
            val l = Label.toString
            val opnd = Operand.toString
            fun i (m, zs) =
               case zs of
                  [] => m
                | _ => concat [m, " ",
                               String.concatWith (List.map (zs, opnd), ", ")]
         in
            case ins of
               Label lbl => l lbl ^ ":"
             | LocalLabel n => int n ^ ":"
             | Section Section.Text => ".text"
             | Section Section.RelRo =>
                  if darwin
                     then ".section __DATA,__const"
                  else ".section .data.rel.ro,\"aw\",@progbits"
             | Section Section.Literal4 =>
                  if darwin
                     then ".section __TEXT,__literal4,4byte_literals"
                  else ".section .rodata.cst4,\"aM\",@progbits,4"
             | Section Section.Literal8 =>
                  if darwin
                     then ".section __TEXT,__literal8,8byte_literals"
                  else ".section .rodata.cst8,\"aM\",@progbits,8"
             | P2align n => ".p2align " ^ int n
             | Globl lbl => ".globl " ^ l lbl
             | Hidden lbl =>
                  (if darwin then ".private_extern " else ".hidden ") ^ l lbl
             | Long (lbl, lbl') => concat [".long ", l lbl, " - ", l lbl']
             | Quad lo =>
                  ".quad " ^ (case lo of
                                 NONE => "0"
                               | SOME lbl => l lbl)
             | LongImm n => ".long " ^ intInf n
             | QuadImm n => ".quad " ^ intInf n
             | Ins (m, zs) => i (m, zs)
             | Cmov (c, s, z, d) =>
                  i (concat ["cmov", Cond.toString c, Size.suffix s],
                     [z, Operand.Reg (d, s)])
             | Setcc (c, d) =>
                  i ("set" ^ Cond.toString c, [Operand.Reg (d, Size.B)])
             | Jmp t => "jmp " ^ Target.toString t
             | Jcc (c, t) =>
                  concat ["j", Cond.toString c, " ", Target.toString t]
             | JmpInd z => "jmp *" ^ opnd z
             | Call lbl =>
                  concat ["call ", l lbl,
                          case (darwin, lbl) of
                             (false, Label.Sym _) => "@PLT"
                           | _ => ""]
             | CallInd z => "call *" ^ opnd z
             | Ret => "ret"
         end
   end

fun output (instrs: Instr.t list, print: string -> unit): unit =
   List.foreach
   (instrs, fn ins =>
    ((case ins of
         Instr.Label _ => ()
       | Instr.LocalLabel _ => ()
       | _ => print "\t")
     ; print (Instr.toString ins)
     ; print "\n"))

end

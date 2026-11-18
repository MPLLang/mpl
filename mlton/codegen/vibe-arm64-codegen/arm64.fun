(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Arm64 (S: ARM64_STRUCTS): ARM64 =
struct

open S

fun int (n: int): string =
   if n >= 0 then Int.toString n else "-" ^ Int.toString (~ n)

fun intInf (n: IntInf.t): string =
   if n >= 0 then IntInf.toString n else "-" ^ IntInf.toString (~ n)

fun hex (n: IntInf.t): string =
   let
      fun loop n =
         if n = 0 then ""
         else loop (n div 16)
              ^ String.fromChar
                (String.sub ("0123456789abcdef", IntInf.toInt (n mod 16)))
   in
      if n = 0 then "0x0" else "0x" ^ loop n
   end

structure Reg =
   struct
      datatype t =
         W of int
       | X of int
       | S of int
       | D of int
       | SP
       | WZR

      val toString =
         fn W n => "w" ^ int n
          | X n => "x" ^ int n
          | S n => "s" ^ int n
          | D n => "d" ^ int n
          | SP => "sp"
          | WZR => "wzr"
   end

structure Cond =
   struct
      datatype t =
         EQ | NE
       | CS | CC
       | HS | LO
       | MI | PL
       | VS | VC
       | HI | LS
       | GE | LT
       | GT | LE

      val negate =
         fn EQ => NE | NE => EQ
          | CS => CC | CC => CS
          | HS => LO | LO => HS
          | MI => PL | PL => MI
          | VS => VC | VC => VS
          | HI => LS | LS => HI
          | GE => LT | LT => GE
          | GT => LE | LE => GT

      val toString =
         fn EQ => "eq" | NE => "ne"
          | CS => "cs" | CC => "cc"
          | HS => "hs" | LO => "lo"
          | MI => "mi" | PL => "pl"
          | VS => "vs" | VC => "vc"
          | HI => "hi" | LS => "ls"
          | GE => "ge" | LT => "lt"
          | GT => "gt" | LE => "le"
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

structure Operand =
   struct
      datatype shift = LSL | LSR | ASR
      datatype extend = SXTB | SXTH | SXTW | UXTB | UXTH | UXTW
      datatype t =
         Reg of Reg.t
       | Imm of IntInf.t
       | Shifted of Reg.t * shift * int
       | Extended of Reg.t * extend

      val shiftToString =
         fn LSL => "lsl" | LSR => "lsr" | ASR => "asr"

      val extendToString =
         fn SXTB => "sxtb" | SXTH => "sxth" | SXTW => "sxtw"
          | UXTB => "uxtb" | UXTH => "uxth" | UXTW => "uxtw"

      (* The immediate of a logical instruction is a bit pattern: print it
       * in hex.
       *)
      fun toString (z: t, {hexImm: bool}): string =
         case z of
            Reg r => Reg.toString r
          | Imm n => "#" ^ (if hexImm then hex n else intInf n)
          | Shifted (r, s, n) =>
               concat [Reg.toString r, ", ", shiftToString s, " #", int n]
          | Extended (r, e) =>
               concat [Reg.toString r, ", ", extendToString e]
   end

structure Addr =
   struct
      datatype t =
         Base of Reg.t
       | Offset of Reg.t * int
       | Index of Reg.t * Reg.t * int
       | PreIndex of Reg.t * int
       | PostIndex of Reg.t * int

      val toString =
         fn Base b => concat ["[", Reg.toString b, "]"]
          | Offset (b, n) => concat ["[", Reg.toString b, ", #", int n, "]"]
          | Index (b, i, n) =>
               concat ["[", Reg.toString b, ", ", Reg.toString i,
                       if n = 0 then "" else ", lsl #" ^ int n,
                       "]"]
          | PreIndex (b, n) =>
               concat ["[", Reg.toString b, ", #", int n, "]!"]
          | PostIndex (b, n) => concat ["[", Reg.toString b, "], #", int n]
   end

structure Section =
   struct
      datatype t =
         Text
       | RelRo
   end

structure Instr =
   struct
      datatype unop =
         MOV | MVN | NEG | NEGS
       | SXTB | SXTH | SXTW
       | FMOV | FABS | FNEG | FSQRT
       | FCVT | FCVTZS | FCVTZU | SCVTF | UCVTF
       | FRINTM | FRINTP | FRINTX | FRINTZ
      datatype binop =
         ADD | ADDS | SUB | SUBS
       | MUL | SMULH | SMULL | UMULH | SDIV | UDIV
       | AND | ORR | EOR
       | LSL | LSR | ASR | ROR
       | FADD | FSUB | FMUL | FDIV
      datatype ternop = MSUB | FMADD | FNMSUB
      datatype cmpop = CMP | CMN | FCMP
      datatype selop = CSEL | CSINC | FCSEL
      datatype loadop =
         LDR | LDRB | LDRH | LDRSW
       | LDAXR | LDAXRB | LDAXRH
      datatype storeop = STR | STRB | STRH
      datatype storexop = STLXR | STLXRB | STLXRH
      datatype pairop = LDP | STP

      datatype t =
         Label of Label.t
       | LocalLabel of int
       | Section of Section.t
       | P2align of int
       | Globl of Label.t
       | Hidden of Label.t
       | Long of Label.t * Label.t
       | Quad of Label.t option
       | Un of unop * Reg.t * Reg.t
       | Bin of binop * Reg.t * Reg.t * Operand.t
       | Tern of ternop * Reg.t * Reg.t * Reg.t * Reg.t
       | Cmp of cmpop * Reg.t * Operand.t
       | Cset of Reg.t * Cond.t
       | Sel of selop * Reg.t * Reg.t * Reg.t * Cond.t
       | Ubfx of Reg.t * Reg.t * int * int
       | MovImm of Reg.t * IntInf.t
       | Movz of Reg.t * int * int
       | Movk of Reg.t * int * int
       | Adrp of Reg.t * Label.t * {got: bool}
       | AddLo of Reg.t * Reg.t * Label.t
       | LoadGot of Reg.t * Reg.t * Label.t
       | Adr of Reg.t * Label.t
       | Load of loadop * Reg.t * Addr.t
       | Store of storeop * Reg.t * Addr.t
       | StoreX of storexop * Reg.t * Reg.t * Addr.t
       | Pair of pairop * Reg.t * Reg.t * Addr.t
       | B of Target.t
       | Bcond of Cond.t * Target.t
       | Cbz of Reg.t * Target.t
       | Cbnz of Reg.t * Target.t
       | Bl of Label.t
       | Blr of Reg.t
       | Br of Reg.t
       | Ret

      val unop =
         fn MOV => "mov" | MVN => "mvn" | NEG => "neg" | NEGS => "negs"
          | SXTB => "sxtb" | SXTH => "sxth" | SXTW => "sxtw"
          | FMOV => "fmov" | FABS => "fabs" | FNEG => "fneg"
          | FSQRT => "fsqrt"
          | FCVT => "fcvt" | FCVTZS => "fcvtzs" | FCVTZU => "fcvtzu"
          | SCVTF => "scvtf" | UCVTF => "ucvtf"
          | FRINTM => "frintm" | FRINTP => "frintp" | FRINTX => "frintx"
          | FRINTZ => "frintz"

      val binop =
         fn ADD => "add" | ADDS => "adds" | SUB => "sub" | SUBS => "subs"
          | MUL => "mul" | SMULH => "smulh" | SMULL => "smull"
          | UMULH => "umulh" | SDIV => "sdiv" | UDIV => "udiv"
          | AND => "and" | ORR => "orr" | EOR => "eor"
          | LSL => "lsl" | LSR => "lsr" | ASR => "asr" | ROR => "ror"
          | FADD => "fadd" | FSUB => "fsub" | FMUL => "fmul" | FDIV => "fdiv"

      val isLogical =
         fn AND => true | ORR => true | EOR => true | _ => false

      val ternop =
         fn MSUB => "msub" | FMADD => "fmadd" | FNMSUB => "fnmsub"

      val cmpop =
         fn CMP => "cmp" | CMN => "cmn" | FCMP => "fcmp"

      val selop =
         fn CSEL => "csel" | CSINC => "csinc" | FCSEL => "fcsel"

      val loadop =
         fn LDR => "ldr" | LDRB => "ldrb" | LDRH => "ldrh" | LDRSW => "ldrsw"
          | LDAXR => "ldaxr" | LDAXRB => "ldaxrb" | LDAXRH => "ldaxrh"

      val storeop =
         fn STR => "str" | STRB => "strb" | STRH => "strh"

      val storexop =
         fn STLXR => "stlxr" | STLXRB => "stlxrb" | STLXRH => "stlxrh"

      val pairop =
         fn LDP => "ldp" | STP => "stp"

      fun toString (ins: t): string =
         let
            val darwin = !Control.Target.os = MLton.Platform.OS.Darwin
            val r = Reg.toString
            val l = Label.toString
            fun opnd z = Operand.toString (z, {hexImm = false})
            (* a mnemonic and its operands *)
            fun i (opc, args) =
               concat (opc :: " " :: List.separate (args, ", "))
         in
            case ins of
               Label lbl => l lbl ^ ":"
             | LocalLabel n => int n ^ ":"
             | Section Section.Text => ".text"
             | Section Section.RelRo =>
                  if darwin
                     then ".section __DATA,__const"
                  else ".section .data.rel.ro,\"aw\""
             | P2align n => ".p2align " ^ int n
             | Globl lbl => ".globl " ^ l lbl
             | Hidden lbl =>
                  (if darwin then ".private_extern " else ".hidden ") ^ l lbl
             | Long (lbl, lbl') => concat [".long ", l lbl, " - ", l lbl']
             | Quad lo =>
                  ".quad " ^ (case lo of
                                 NONE => "0"
                               | SOME lbl => l lbl)
             | Un (opc, d, s) => i (unop opc, [r d, r s])
             | Bin (opc, d, s, z) =>
                  i (binop opc,
                     [r d, r s,
                      Operand.toString (z, {hexImm = isLogical opc})])
             | Tern (opc, d, s1, s2, s3) =>
                  i (ternop opc, [r d, r s1, r s2, r s3])
             | Cmp (opc, s, z) => i (cmpop opc, [r s, opnd z])
             | Cset (d, c) => i ("cset", [r d, Cond.toString c])
             | Sel (opc, d, s1, s2, c) =>
                  i (selop opc, [r d, r s1, r s2, Cond.toString c])
             | Ubfx (d, s, lsb, width) =>
                  i ("ubfx", [r d, r s, "#" ^ int lsb, "#" ^ int width])
             | MovImm (d, n) => i ("mov", [r d, "#" ^ intInf n])
             | Movz (d, n, shift) =>
                  i ("movz", [r d, "#" ^ int n, "lsl #" ^ int shift])
             | Movk (d, n, shift) =>
                  i ("movk", [r d, "#" ^ int n, "lsl #" ^ int shift])
             | Adrp (d, lbl, {got}) =>
                  i ("adrp",
                     [r d,
                      case (darwin, got) of
                         (true, true) => l lbl ^ "@GOTPAGE"
                       | (true, false) => l lbl ^ "@PAGE"
                       | (false, true) => ":got:" ^ l lbl
                       | (false, false) => l lbl])
             | AddLo (d, s, lbl) =>
                  i ("add",
                     [r d, r s,
                      if darwin then l lbl ^ "@PAGEOFF" else ":lo12:" ^ l lbl])
             | LoadGot (d, s, lbl) =>
                  i ("ldr",
                     [r d,
                      concat ["[", r s, ", ",
                              if darwin
                                 then l lbl ^ "@GOTPAGEOFF"
                              else ":got_lo12:" ^ l lbl,
                              "]"]])
             | Adr (d, lbl) => i ("adr", [r d, l lbl])
             | Load (opc, d, a) => i (loadop opc, [r d, Addr.toString a])
             | Store (opc, s, a) => i (storeop opc, [r s, Addr.toString a])
             | StoreX (opc, status, s, a) =>
                  i (storexop opc, [r status, r s, Addr.toString a])
             | Pair (opc, r1, r2, a) =>
                  i (pairop opc, [r r1, r r2, Addr.toString a])
             | B t => "b " ^ Target.toString t
             | Bcond (c, t) =>
                  concat ["b.", Cond.toString c, " ", Target.toString t]
             | Cbz (s, t) => i ("cbz", [r s, Target.toString t])
             | Cbnz (s, t) => i ("cbnz", [r s, Target.toString t])
             | Bl lbl => "bl " ^ l lbl
             | Blr s => "blr " ^ r s
             | Br s => "br " ^ r s
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

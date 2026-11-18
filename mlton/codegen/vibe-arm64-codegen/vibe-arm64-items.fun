(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor VibeArm64Items (S: VIBE_ARM64_ITEMS_STRUCTS): VIBE_ARM64_ITEMS =
struct

open S
structure Machine = Analysis.Machine
open Machine

structure Region = Analysis.Region
structure Temp = Analysis.Temp
val dstTemp = Analysis.dstTemp
val foreachTemp = Analysis.foreachTemp
val stripCast = Analysis.stripCast
val switchCases = Analysis.switchCases

fun cty z = Type.toCType (Operand.ty z)

type app = {args: Operand.t vector,
            dst: Operand.t,
            prim: Type.t Prim.t,
            also: (Operand.t * bool) option}

datatype t =
   App of app
 | Move of {dst: Operand.t, src: Operand.t}
 | Defined of Operand.t
type item = t

(* The statements of a block, with each adjacent pair of an operation and
 * its overflow check (on the same arguments) combined.
 *)
fun items (ss: Statement.t list): item list =
   case ss of
      [] => []
    | Statement.Move z :: ss => Move z :: items ss
    | Statement.PrimApp {dst = NONE, ...} :: ss =>
         (* all primitives are pure *)
         items ss
    | Statement.PrimApp {args, dst = SOME dst, prim} :: ss =>
         let
            datatype z = datatype Prim.t
            fun single () =
               App {args = args, dst = dst, prim = prim, also = NONE}
               :: items ss
            (* is q the overflow check for p? *)
            fun checks (p, q) =
               case (p, q) of
                  (Word_add s, Word_addCheckP (s', _)) =>
                     WordSize.equals (s, s')
                | (Word_mul (s, _), Word_mulCheckP (s', _)) =>
                     WordSize.equals (s, s')
                | (Word_neg s, Word_negCheckP (s', _)) =>
                     WordSize.equals (s, s')
                | (Word_sub s, Word_subCheckP (s', _)) =>
                     WordSize.equals (s, s')
                | _ => false
            fun simple z =
               case z of
                  Operand.StackOffset _ => true
                | Operand.Temporary _ => true
                | _ => false
         in
            case ss of
               Statement.PrimApp {args = args', dst = SOME dst',
                                  prim = prim'} :: ss' =>
                  if Vector.equals (args, args', Operand.equals)
                     andalso simple dst andalso simple dst'
                     andalso not (Operand.interfere (dst, dst'))
                     andalso not (Vector.exists
                                  (args, fn z =>
                                   Operand.interfere (dst, z)
                                   orelse Operand.interfere (dst', z)))
                     then if checks (prim, prim')
                             then App {args = args, dst = dst',
                                       prim = prim',
                                       also = SOME (dst, false)}
                                  :: items ss'
                          else if checks (prim', prim)
                             then App {args = args, dst = dst,
                                       prim = prim,
                                       also = SOME (dst', true)}
                                  :: items ss'
                          else single ()
                  else single ()
             | _ => single ()
         end

fun ofBlock ({blockRegion, ifConvert, labelBlock, liveTemps, nearSuccessors,
              ...}: Analysis.t,
             Block.T {label, statements, transfer, ...}): t list =
   let
      val region = blockRegion label
      fun temp t = Region.temp (region, t)
      (* When the result of a primitive is only copied to a stack slot,
       * compute it into the slot (which may be in a register).
       *)
      fun direct (its: item list, transfer): item list =
         let
            fun same (t, t') =
               Temp.equals (temp t, temp t')
            fun mentions (z, t) =
               let
                  val r = ref false
               in
                  foreachTemp (z, fn t' => if same (t, t') then r := true else ())
                  ; !r
               end
            fun dead (t, rest) =
               not (List.exists
                    (rest, fn it =>
                     case it of
                        App {args, dst, also, ...} =>
                           mentions (dst, t)
                           orelse (case also of
                                      NONE => false
                                    | SOME (z, _) => mentions (z, t))
                           orelse Vector.exists (args, fn z => mentions (z, t))
                      | Defined _ => false
                      | Move {dst, src} =>
                           mentions (dst, t) orelse mentions (src, t)))
               andalso not (Transfer.foldOperands
                            (transfer, false, fn (z, b) =>
                             b orelse mentions (z, t)))
               andalso not (List.exists
                            (nearSuccessors transfer
                             @ (case ifConvert transfer of
                                   NONE => []
                                 | SOME {join, ...} => [join]),
                             fn l =>
                             List.exists (liveTemps (labelBlock l), fn e =>
                                          Temp.equals (e, temp t))))
            (* A shift right by k and then a mask of the low w bits is a
             * bitfield extract; the mask becomes a third argument of the
             * shift (see Word_rshift in primApp).
             *)
            fun extract (it, it', rest) =
               case (it, it') of
                  (App {args, dst = d, prim = Prim.Word_rshift (ws, {signed = false}),
                        also = NONE},
                   App {args = args', dst = d', prim = Prim.Word_andb ws',
                        also = NONE}) =>
                     (case (dstTemp d, Vector.sub (args, 1),
                            dstTemp (Vector.sub (args', 0)),
                            Vector.sub (args', 1)) of
                         (SOME t, Operand.Const (Const.Word k), SOME t',
                          mask as Operand.Const (Const.Word m)) =>
                            let
                               val n = Bits.toInt (WordSize.bits ws)
                               val k = WordX.toIntInf k
                               val m = WordX.toIntInf m
                               fun width (w: int) =
                                  if w > 64 then NONE
                                  else if IntInf.<< (1, Word.fromInt w) - 1 = m
                                     then SOME w
                                  else width (w + 1)
                               val keep =
                                  case dstTemp d' of
                                     SOME t'' => same (t, t'')
                                   | NONE => false
                            in
                               case width 1 of
                                  SOME w =>
                                     if n >= 32
                                        andalso WordSize.equals (ws, ws')
                                        andalso same (t, t')
                                        andalso k + IntInf.fromInt w
                                                <= IntInf.fromInt n
                                        andalso not (mentions (Vector.sub (args, 0), t))
                                        andalso (keep orelse dead (t, rest))
                                        then SOME ((if keep then [Defined d]
                                                    else [])
                                                   @ App {args = Vector.new3
                                                               (Vector.sub (args, 0),
                                                                Vector.sub (args, 1),
                                                                mask),
                                                        dst = d',
                                                        prim = Prim.Word_rshift
                                                               (ws, {signed = false}),
                                                        also = NONE}
                                                   :: (if keep then []
                                                       else [Defined d]))
                                     else NONE
                                | NONE => NONE
                            end
                       | _ => NONE)
                | _ => NONE
         in
            case its of
               (it as App {prim = Prim.Word_rshift _, ...})
               :: (it' as App _) :: rest =>
                  (case extract (it, it', rest) of
                      SOME its => its @ direct (rest, transfer)
                    | NONE => it :: direct (it' :: rest, transfer))
             | (it as App {args, dst = d, prim, also = NONE})
               :: (it' as Move {dst = d', src}) :: rest =>
                  (case (dstTemp d, stripCast d', dstTemp src) of
                      (SOME t, Operand.StackOffset _, SOME t') =>
                         if same (t, t')
                            andalso CType.equals (cty d, cty d')
                            andalso dead (t, rest)
                            then App {args = args, dst = d', prim = prim,
                                      also = NONE}
                                 :: Defined d :: direct (rest, transfer)
                         else it :: direct (it' :: rest, transfer)
                    | _ => it :: direct (it' :: rest, transfer))
             | it :: rest => it :: direct (rest, transfer)
             | [] => []
         end
   in
      direct (items (Vector.toList statements), transfer)
   end

fun fusable ({blockRegion, labelBlock, liveTemps, ...}: Analysis.t,
             Block.T {label, transfer, ...},
             its: t list) =
   let
      val region = blockRegion label
      fun temp t = Region.temp (region, t)
      fun fusable (its, transfer) =
         case (transfer, its) of
            (_, []) => NONE
          | (Transfer.Switch (Switch.T {cases, default,
                                        test = Operand.Temporary t, ...}),
             _) =>
               (case (List.splitLast its, switchCases (cases, default)) of
                   ((its, App (a as {dst = Operand.Temporary t', ...})),
                    ([(w, l)], l')) =>
                      let
                         val e = temp t
                         fun dead l =
                            not (List.exists (liveTemps (labelBlock l), fn e' =>
                                              Temp.equals (e, e')))
                      in
                         if Temporary.equals (t, t')
                            andalso dead l andalso dead l'
                            then if WordX.isOne w
                                    then SOME (its, a, l, l')
                                 else if WordX.isZero w
                                    then SOME (its, a, l', l)
                                 else NONE
                         else NONE
                      end
                 | _ => NONE)
          | _ => NONE
   in
      fusable (its, transfer)
   end

end

(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor VibeAmd64Items (S: VIBE_AMD64_ITEMS_STRUCTS): VIBE_AMD64_ITEMS =
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
 | Sels of app * app list
type item = t

(* The statements of a block, with each adjacent pair of an operation and
 * its overflow check (on the same arguments) combined.
 *)
fun items (ss: Statement.t list): item list =
   case ss of
      [] => []
    | Statement.Move z :: ss => Move z :: items ss
    | Statement.PrimApp {dst = NONE, ...} :: ss =>
         (* The analysis has given a destination to every primitive that
          * has an effect.
          *)
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

fun ofBlock ({blockRegion, labelBlock, liveTemps, nearSuccessors,
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
                           mentions (dst, t) orelse mentions (src, t)
                      | Sels _ => true))
               andalso not (Transfer.foldOperands
                            (transfer, false, fn (z, b) =>
                             b orelse mentions (z, t)))
               andalso not (List.exists
                            (nearSuccessors transfer, fn l =>
                             List.exists (liveTemps (labelBlock l), fn e =>
                                          Temp.equals (e, temp t))))
         in
            case its of
               (it as App {args, dst = d, prim, also = NONE})
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
      (* A comparison followed by selects that test its result, which is
       * dead after them.
       *)
      fun sels (its: item list): item list =
         let
            fun same (t, t') = Temp.equals (temp t, temp t')
            fun mentions (z, t) =
               let
                  val r = ref false
               in
                  foreachTemp (z, fn t' => if same (t, t') then r := true else ())
                  ; !r
               end
            fun appMentions ({args, dst, also, ...}: app, t) =
               mentions (dst, t)
               orelse (case also of
                          NONE => false
                        | SOME (z, _) => mentions (z, t))
               orelse Vector.exists (args, fn z => mentions (z, t))
            fun dead (t, rest) =
               not (List.exists
                    (rest, fn it =>
                     case it of
                        App a => appMentions (a, t)
                      | Defined _ => false
                      | Move {dst, src} =>
                           mentions (dst, t) orelse mentions (src, t)
                      | Sels (a, ss) =>
                           appMentions (a, t)
                           orelse List.exists (ss, fn s => appMentions (s, t))))
               andalso not (Transfer.foldOperands
                            (transfer, false, fn (z, b) =>
                             b orelse mentions (z, t)))
               andalso not (List.exists
                            (nearSuccessors transfer, fn l =>
                             List.exists (liveTemps (labelBlock l), fn e =>
                                          Temp.equals (e, temp t))))
            (* a select on t (which is 0 or 1) that doesn't otherwise
             * mention it
             *)
            fun selOn (it, t) =
               case it of
                  App (s as {args, dst, prim, ...}) =>
                     if Analysis.isSelect prim
                        andalso (case Vector.sub (args, 0) of
                                    Operand.Temporary t' => same (t, t')
                                  | _ => false)
                        andalso (case Vector.sub (args, 1) of
                                    Operand.Const (Const.Word w) =>
                                       WordX.isZero w orelse WordX.isOne w
                                  | _ => false)
                        andalso not (mentions (dst, t))
                        andalso not (mentions (Vector.sub (args, 2), t))
                        andalso not (mentions (Vector.sub (args, 3), t))
                        then SOME s
                     else NONE
                | _ => NONE
            fun split (its, t, ac) =
               case its of
                  it :: rest =>
                     (case selOn (it, t) of
                         SOME s => split (rest, t, s :: ac)
                       | NONE => (List.rev ac, its))
                | [] => (List.rev ac, [])
         in
            case its of
               (it as App (c as {dst = Operand.Temporary t, prim, also = NONE,
                                 ...}))
               :: rest =>
                  (case (if Analysis.isSelect prim then ([], rest)
                         else split (rest, t, [])) of
                      ([], _) => it :: sels rest
                    | (ss, rest') =>
                         if dead (t, rest')
                            then Sels (c, ss) :: sels rest'
                         else it :: sels rest)
             | it :: rest => it :: sels rest
             | [] => []
         end
   in
      sels (direct (items (Vector.toList statements), transfer))
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

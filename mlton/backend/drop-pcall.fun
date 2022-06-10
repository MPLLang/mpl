(* Copyright (C) 2022 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor DropPCall (S: RSSA_TRANSFORM_STRUCTS): RSSA_TRANSFORM =
struct

open S

fun dropPCallFunction f =
   let
      val {args, blocks, name, raises, returns, start} =
         Function.dest f
      val blocks =
         Vector.map
         (blocks, fn Block.T {args, kind, label, statements, transfer} =>
          let
             val kind =
                case kind of
                   Kind.PCallReturn {cont, ...} =>
                      if Label.equals (label, cont)
                         then Kind.Cont {handler = Handler.Dead}
                         else Kind.Jump
                 | _ => kind
             val transfer =
                case transfer of
                   Transfer.PCall {func, args, cont, ...} =>
                      Transfer.Call {func = func,
                                     args = args,
                                     return = Return.NonTail {cont = cont,
                                                              handler = Handler.Dead}}
                 | _ => transfer
          in
             Block.T {args = args,
                      kind = kind,
                      label = label,
                      statements = statements,
                      transfer = transfer}
          end)
   in
      Function.new {args = args,
                    blocks = blocks,
                    name = name,
                    raises = raises,
                    returns = returns,
                    start = start}
   end

fun transform p =
   let
      val Program.T {functions, handlesSignals, main, objectTypes, profileInfo, statics} = p
   in
      Program.T {functions = List.revMap (functions, dropPCallFunction),
                 handlesSignals = handlesSignals,
                 main = dropPCallFunction main,
                 objectTypes = objectTypes,
                 profileInfo = profileInfo,
                 statics = statics}
   end

end

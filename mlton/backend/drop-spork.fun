(* Copyright (C) 2024 Colin McDonald.
 * Copyright (C) 2022 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor DropSpork (S: RSSA_TRANSFORM_STRUCTS): RSSA_TRANSFORM =
struct

open S

fun dropSporkFunction f =
   let
      val {args, blocks, name, raises, returns, start} =
         Function.dest f
      val blocks =
         Vector.map
         (blocks, fn Block.T {args, kind, label, statements, transfer} =>
          let
             val kind =
                case kind of
                   Kind.SporkSpwn _ => Kind.Jump
                 | Kind.SpoinSync _ => Kind.Jump
                 | _ => kind
             val transfer =
                case transfer of
                   Transfer.Spork {spid, cont, spwn} =>
                      Transfer.Goto {args = Vector.new0 (),
                                     dst = cont}
                 | Transfer.Spoin {spid, seq, sync} =>
                      Transfer.Goto {args = Vector.new0 (),
                                     dst = seq}
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
      Program.T {functions = List.revMap (functions, dropSporkFunction),
                 handlesSignals = handlesSignals,
                 main = dropSporkFunction main,
                 objectTypes = objectTypes,
                 profileInfo = profileInfo,
                 statics = statics}
   end

end

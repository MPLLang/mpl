(* Copyright (C) 2022 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor DropPCall (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM =
struct

open S

fun dropPCallFunction f =
   let
      val {args, blocks, mayInline, name, raises, returns, start} =
         Function.dest f
      val blocks =
         Vector.map
         (blocks, fn Block.T {args, label, statements, transfer} =>
          let
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
                      label = label,
                      statements = statements,
                      transfer = transfer}
          end)
   in
      Function.new {args = args,
                    blocks = blocks,
                    mayInline = mayInline,
                    name = name,
                    raises = raises,
                    returns = returns,
                    start = start}
   end

fun transform (Program.T {datatypes, globals, functions, main}) =
   Program.T {datatypes = datatypes,
              globals = globals,
              functions = List.revMap (functions, dropPCallFunction),
              main = main}

end

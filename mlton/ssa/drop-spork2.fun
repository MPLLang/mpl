(* Copyright (C) 2022 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor DropSpork2 (S: SSA2_TRANSFORM_STRUCTS): SSA2_TRANSFORM =
struct

open S

fun dropSporkFunction f =
   let
      val {args, blocks, mayInline, name, raises, returns, start} =
         Function.dest f
      val blocks =
         Vector.map
         (blocks, fn Block.T {args, label, statements, transfer} =>
          let
            (* remove Prim.Spork_forkThreadAndSetData and Prim.Spork_getData *)
            val statements =
                Vector.keepAll
                  (statements,
                   fn Statement.Bind {exp, ty, var} =>
                      (case exp of
                          Exp.PrimApp {args, prim} =>
                             (case prim of
                                 (* once spork/spoin removed, these should never matter *)
                                 Prim.Spork_forkThreadAndSetData => false
                               (* We will rely on dead code elim to clean up Spork_getData *)
                               (*| Prim.Spork_getData => false*)
                               | _ => true)
                        | _ => true)
                   | _ => true)
            (* Changes sporks and spoins into gotos of the sequential case *)
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

fun transform2 (Program.T {datatypes, globals, functions, main}) =
   Program.T {datatypes = datatypes,
              globals = globals,
              functions = List.revMap (functions, dropSporkFunction),
              main = main}

end

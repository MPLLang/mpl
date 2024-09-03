(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Inline (S: INLINE_STRUCTS): INLINE = 
struct

open S
open Exp Transfer

structure Return =
   struct
      open Return

      fun isTail (z: t): bool =
         case z of
            Dead => false
          | NonTail _ => false
          | Tail => true
   end

structure Function =
   struct
      open Function

      fun containsCall (f: Function.t): bool =
         Exn.withEscape
         (fn escape =>
          (Vector.foreach
           (Function.blocks f, fn Block.T {transfer, ...} =>
            case transfer of
               Call _ => escape true
             | _ => ())
           ; false))
      fun containsLoop (f: Function.t): bool =
         let
            val {get, set, destroy} =
               Property.destGetSet (Label.plist, Property.initConst false)
         in
            Exn.withEscape
            (fn escape =>
             let
                val _ =
                   Function.dfs
                   (f, fn (Block.T {label, transfer, ...}) =>
                    (set (label, true)
                     ; (case transfer of
                           Goto {dst, ...} => if get dst then escape true else ()
                         | _ => ())
                     ; fn () => set (label, false)))
             in
                false
             end)
            before (destroy ())
         end
   end

local
   fun 'a make (dontInlineFunc: Function.t * 'a -> bool)
      (Program.T {functions, ...}, a: 'a): Func.t -> bool =
      let
         val {get = shouldInline: Func.t -> bool, 
              set = setShouldInline, ...} =
            Property.getSetOnce (Func.plist, Property.initConst false)
      in
         List.foreach
         (functions, fn f =>
          if not (InlineAttr.mayInline (Function.inline f))
             orelse dontInlineFunc (f, a)
             then ()
          else setShouldInline (Function.name f, true))
         ; Control.diagnostics
           (fn display =>
            let open Layout
            in List.foreach
               (functions, fn f => 
                let 
                   val name = Function.name f
                   val shouldInline = shouldInline name
                in 
                   display
                   (seq [Func.layout name, str ": ",
                         record [("shouldInline", Bool.layout shouldInline)]])
                end)
            end)
         ; shouldInline
      end
in
   val leafOnce = make (fn (f, {size}) =>
                        Option.isNone (Function.sizeMax (f, {max = size,
                                                             sizeExp = Exp.size,
                                                             sizeTransfer = Transfer.size}))
                        orelse Function.containsCall f)
   val leafOnceNoLoop = make (fn (f, {size}) =>
                              Option.isNone (Function.sizeMax (f, {max = size,
                                                                   sizeExp = Exp.size,
                                                                   sizeTransfer = Transfer.size}))
                              orelse Function.containsCall f
                              orelse Function.containsLoop f)
end

structure Graph = DirectedGraph
structure Node = Graph.Node

local
   fun make (dontInline: Function.t -> bool)
      (Program.T {functions, ...}, {size: int option}) =
      let
         val max = size
         type info = {function: Function.t,
                      node: unit Node.t,
                      shouldInline: bool ref,
                      size: int ref}
         val {get = funcInfo: Func.t -> info,
              set = setFuncInfo, ...} =
            Property.getSetOnce
            (Func.plist, Property.initRaise ("funcInfo", Func.layout))
         val {get = nodeFunc: unit Node.t -> Func.t,
              set = setNodeFunc, ...} = 
            Property.getSetOnce 
            (Node.plist, Property.initRaise ("nodeFunc", Node.layout))
         val graph = Graph.new ()
         (* initialize the info for each func *)
         val _ = 
            List.foreach
            (functions, fn f =>
             let 
                val name = Function.name f
                val n = Graph.newNode graph
             in
                setNodeFunc (n, name)
                ; setFuncInfo (name, {function = f,
                                      node = n,
                                      shouldInline = ref false,
                                      size = ref 0})
             end)
         (* Build the call graph. *)
         val _ =
            List.foreach
            (functions, fn f => 
             let 
                val {name, blocks, ...} = Function.dest f
                val {node, ...} = funcInfo name
             in
                Vector.foreach
                (blocks, fn Block.T {transfer, ...} =>
                 case transfer of
                    Call {func, ...} =>
                       (ignore o Graph.addEdge)
                       (graph, {from = node, to = #node (funcInfo func)})
                  | _ => ())
             end)
         (* Compute strongly-connected components.
          * Then start at the leaves of the call graph and work up.
          *)
         val _ = 
            List.foreach
            (rev (Graph.stronglyConnectedComponents graph),
             fn scc =>
             case scc of 
                [n] =>
                   let 
                      val {function, shouldInline, size, ...} = 
                         funcInfo (nodeFunc n)
                   in 
                      if InlineAttr.mayInline (Function.inline function)
                         andalso not (dontInline function)
                         then Exn.withEscape
                              (fn escape =>
                               let
                                  val res =
                                     Function.sizeMax
                                     (function,
                                      {max = max,
                                       sizeExp = Exp.size,
                                       sizeTransfer =
                                       fn t =>
                                       case t of
                                          Call {func, inline, ...} =>
                                             let
                                                val {shouldInline, size, ...} =
                                                   funcInfo func
                                             in
                                                if !shouldInline
                                                   andalso InlineAttr.mayInline inline
                                                   then !size
                                                   else escape ()
                                             end
                                        | _ => Transfer.size t})
                               in
                                  case res of
                                     NONE => ()
                                   | SOME n => (shouldInline := true
                                                ; size := n)
                               end)
                      else ()
                   end
              | _ => ())
         val _ =
            Control.diagnostics
            (fn display =>
             let open Layout
             in List.foreach
                (functions, fn f => 
                 let 
                    val name = Function.name f
                    val {shouldInline, size, ...} = funcInfo name
                    val shouldInline = !shouldInline
                    val size = !size
                 in 
                    display
                    (seq [Func.layout name, str ": ",
                          record [("shouldInline", Bool.layout shouldInline),
                                  ("size", Int.layout size)]])
                 end)
             end)
      in
         ! o #shouldInline o funcInfo
      end
in
   val leafRepeat = make (fn _ => false)
   val leafRepeatNoLoop = make (fn f => Function.containsLoop f)
end

fun nonRecursive (Program.T {functions, ...}, {small: int, product: int}) =
   let
      type info = {doesCallSelf: bool ref,
                   function: Function.t,
                   node: unit Node.t,
                   numCalls: int ref,
                   shouldInline: bool ref,
                   size: int ref}
      val {get = funcInfo: Func.t -> info,
           set = setFuncInfo, ...} =
         Property.getSetOnce
         (Func.plist, Property.initRaise ("funcInfo", Func.layout))
      val {get = nodeFunc: unit Node.t -> Func.t,
           set = setNodeFunc, ...} = 
         Property.getSetOnce 
         (Node.plist, Property.initRaise ("nodeFunc", Node.layout))
      val graph = Graph.new ()
      (* initialize the info for each func *)
      val _ = 
         List.foreach
         (functions, fn f =>
          let 
             val name = Function.name f
             val n = Graph.newNode graph
          in
             setNodeFunc (n, name)
             ; setFuncInfo (name, {doesCallSelf = ref false,
                                   function = f,
                                   node = n,
                                   numCalls = ref 0,
                                   shouldInline = ref false,
                                   size = ref 0})
          end)
      (* Update call counts. *)
      val _ = 
         List.foreach
         (functions, fn f =>
          let 
             val {name, blocks, ...} = Function.dest f
             val {doesCallSelf, ...} = funcInfo name
          in
             Vector.foreach
             (blocks, fn Block.T {transfer, ...} =>
              case transfer of
                 Call {func, inline, ...} =>
                    let
                       val {numCalls, ...} = funcInfo func
                    in
                       if Func.equals (name, func)
                          then doesCallSelf := true
                       else
                          if InlineAttr.mayInline inline
                             then Int.inc numCalls
                          else ()
                    end
               | _ => ())
          end)
      fun mayInline (setSize: bool,
                     {function, doesCallSelf, numCalls, size, ...}: info): bool =
         InlineAttr.mayInline (Function.inline function)
         andalso not (!doesCallSelf)
         andalso let
                    val n =
                       Function.size
                       (function,
                        {sizeExp = Exp.size,
                         sizeTransfer =
                         fn t as Call {func, inline, ...} =>
                               let
                                  val {shouldInline, size, ...} = funcInfo func
                               in
                                  if !shouldInline
                                     andalso InlineAttr.mayInline inline
                                     then !size
                                     else Transfer.size t
                               end
                          | t => Transfer.size t})
                 in
                    if setSize
                       then size := n
                    else ()
                    ; (!numCalls - 1) * (n - small) <= product
                 end
      (* Build the call graph.  Do not include functions that we already know
       * will not be inlined.
       *)
      val _ =
         List.foreach
         (functions, fn f => 
          let 
             val {name, blocks, ...} = Function.dest f
             val info as {node, ...} = funcInfo name
          in
             if mayInline (false, info)
                then Vector.foreach
                     (blocks, fn Block.T {transfer, ...} =>
                      case transfer of
                         Call {func, inline, ...} =>
                            if Func.equals (name, func)
                               orelse not (InlineAttr.mayInline inline)
                               then ()
                            else (ignore o Graph.addEdge)
                                 (graph, {from = node, to = #node (funcInfo func)})
                       | _ => ())
             else ()
          end)
      (* Compute strongly-connected components.
       * Then start at the leaves of the call graph and work up.
       *)
      val _ = 
         List.foreach
         (rev (Graph.stronglyConnectedComponents graph),
          fn [n] => let val info as {shouldInline, ...} = funcInfo (nodeFunc n)
                    in shouldInline := mayInline (true, info)
                    end
           | _ => ())
      val _ =
         Control.diagnostics
         (fn display =>
          let open Layout
          in List.foreach
             (functions, fn f => 
              let 
                 val name = Function.name f
                 val {numCalls, shouldInline, size, ...} = funcInfo name
                 val numCalls = !numCalls
                 val shouldInline = !shouldInline
                 val size = !size
              in 
                 display
                 (seq [Func.layout name, str ": ",
                       record [("numCalls", Int.layout numCalls),
                               ("shouldInline", Bool.layout shouldInline),
                               ("size", Int.layout size)]])
              end)
          end)
   in
      ! o #shouldInline o funcInfo
   end

fun transform {program as Program.T {datatypes, globals, functions, main},
               shouldInline: Func.t -> bool,
               inlineIntoMain: bool,
               forceAlways: bool} =
   let
      val {get = funcInfo: Func.t -> {function: Function.t,
                                      visited: bool ref},
           set = setFuncInfo, ...} =
         Property.getSetOnce
         (Func.plist, Property.initRaise ("Inline.funcInfo", Func.layout))
      local
         fun mk' sel = sel o funcInfo
         fun mk sel = let val f = mk' sel in (f, ! o f) end
      in
         val function = mk' #function
         val (visitedRef, visited) = mk #visited
      end
      val () = List.foreach (functions, fn f =>
                             setFuncInfo (Function.name f,
                                          {function = f,
                                           visited = ref false}))
      val shrink = shrinkFunction {globals = globals}
      val newFunctions = ref []
      fun visit func =
         let
            val visitedRef = visitedRef func
         in
            if !visitedRef
               then ()
            else let
                    val _ = visitedRef := true
                    val {args, blocks, inline, name, raises, returns, start} =
                       Function.dest (function func)
                    val start'' = Label.new start
                    val start' = Label.new start
                    val args' = Vector.map (args, fn (x, ty) => (Var.new x, ty))
                    val newBlocks =
                       Vector.new2
                       (Block.T {label = start'',
                                 args = Vector.new0 (),
                                 statements = Vector.new0 (),
                                 transfer = Transfer.Goto
                                            {dst = start',
                                             args = Vector.map (args', #1)}},
                        Block.T {label = start',
                                 args = args,
                                 statements = Vector.new0 (),
                                 transfer = Transfer.Goto {dst = start,
                                                           args = Vector.new0 ()}})
                    val blocks = doit (blocks,
                                       [(func,start',Return.Tail)],
                                       newBlocks)
                 in
                    List.push
                    (newFunctions,
                     shrink (Function.new {args = args',
                                           blocks = blocks,
                                           inline = inline,
                                           name = name,
                                           raises = raises,
                                           returns = returns,
                                           start = start''}))
                 end
         end
      and doit (blocks: Block.t vector,
                nest: (Func.t * Label.t * Return.t) list,
                newBlocks: Block.t vector) : Block.t vector =
         let
            val return = List.foldr (nest, Return.Tail, fn ((_,_,r'), r) =>
                                     Return.compose (r, r'))
            val newBlocks = ref [newBlocks]
            val blocks =
               Vector.map
               (blocks,
                fn block as Block.T {label, args, statements, transfer} =>
                let
                   fun new transfer =
                      Block.T {label = label,
                               args = args,
                               statements = statements,
                               transfer = transfer}
                in
                  case transfer of
                     Call {func, args, inline, return = return'} =>
                        let
                           fun doCall () =
                              (visit func
                               ; new (Call {func = func,
                                            args = args,
                                            inline = inline,
                                            return = Return.compose (return, return')}))
                           fun doInline () =
                              let
                                 val dst =
                                    let
                                       val {args, start, blocks, ...} =
                                          (Function.dest o Function.alphaRename)
                                          (function func)
                                       val start' =
                                          Label.newString (Func.originalName func)
                                       val blocks =
                                          doit
                                          (blocks,
                                           (func, start', return')::nest,
                                           Vector.new1
                                           (Block.T
                                            {label = start',
                                             args = args,
                                             statements = Vector.new0 (),
                                             transfer = Goto {dst = start,
                                                              args = Vector.new0 ()}}))
                                       val _ = List.push (newBlocks, blocks)
                                    in
                                       start'
                                    end
                              in
                                 new (Goto {dst = dst,
                                            args = args})
                              end
                           fun findLoop (nest, allTails) =
                              case nest of
                                 [] => NONE
                               | (func',start',return')::nest =>
                                    let
                                       val allTails' =
                                          allTails
                                          andalso
                                          Return.isTail return'
                                    in
                                       if Func.equals (func, func')
                                          then if allTails
                                                  then SOME (SOME start')
                                               else SOME NONE
                                       else findLoop (nest, allTails')
                                    end
                           val callSiteInline = inline
                           val calleeInline = #inline (Function.dest (function func))
                           val mayInline = fn () =>
                              InlineAttr.mayInline callSiteInline
                              andalso
                              InlineAttr.mayInline calleeInline
                           val mustInline = fn () =>
                              forceAlways
                              andalso
                              (InlineAttr.mustInline callSiteInline
                               orelse
                               InlineAttr.mustInline calleeInline)
                        in
                           case findLoop (nest, Return.isTail return') of
                              SOME (SOME start) => new (Transfer.Goto
                                                        {dst = start,
                                                         args = args})
                            | SOME NONE => if mayInline ()
                                              andalso
                                              mustInline ()
                                              then doInline ()
                                           else doCall ()
                            | NONE =>
                                 if mayInline ()
                                    andalso
                                    (shouldInline func
                                     orelse mustInline ())
                                    then doInline ()
                                 else doCall ()
                        end
                   | Raise xs =>
                        (case return of
                            Return.NonTail
                            {handler = Handler.Handle handler, ...} =>
                               new (Goto {dst = handler,
                                          args = xs})
                          | _ => block)
                   | Return xs =>
                        (case return of
                            Return.NonTail {cont, ...} =>
                               new (Goto {dst = cont, args = xs})
                          | _ => block)
                   | _ => block
                end)
         in
            Vector.concat (blocks::(!newBlocks))
         end
      val _ =
         if inlineIntoMain
            then visit main
         else (Vector.foreach
               (Function.blocks (function main), fn Block.T {transfer, ...} =>
                case transfer of
                   Transfer.Call {func, ...} => visit func
                 | _ => ())
               ; List.push (newFunctions, function main))
      val program =
         Program.T {datatypes = datatypes,
                    globals = globals,
                    functions = !newFunctions,
                    main = main}
      val _ = Program.clearTop program
   in
      program
   end

fun inlineLeaf (p, {loops, repeat, size}) =
   if size = SOME 0
      then p
   else transform {program = p,
                   shouldInline = 
                   case (loops, repeat) of
                      (false, false) => leafOnce (p, {size = size})
                    | (false, true) => leafRepeat (p, {size = size})
                    | (true, false) => leafOnceNoLoop (p, {size = size})
                    | (true, true) => leafRepeatNoLoop (p, {size = size}),
                   inlineIntoMain = true,
                   forceAlways = false}
fun inlineNonRecursive (p, arg) =
   transform {program = p,
              shouldInline = nonRecursive (p, arg),
              inlineIntoMain = !Control.inlineIntoMain,
              forceAlways = true}

end

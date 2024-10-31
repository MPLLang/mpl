(* Copyright (C) 2024 MaPLe Developers
 *
 * MaPLe is based on MLton. MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *
 * This pass duplicates some variables that are live down the SporkSpwn
 * path of a Spork. Any variable which is live on a SporkSpwn path has to be
 * stack-allocated, and we don't want to poison the fast path with this
 * additional cost. Ideally, the fast path should be able to keep as many
 * variables in register as possible.
 *
 * To avoid duplicating too much, variables which definitely have to be
 * stack-allocated anyway don't need to be bounced again. So, we classify
 * every variable as:
 *   DefinitelyStack, if it is live at a Cont or mayGC-CReturn, and
 *   HopefullyTemporary, otherwise.
 *
 * We also don't need to duplicate any variable which is live on a SporkSpwn,
 * but NOT live down the corresponding fast path.
 *
 * This pass must be run before implement-handlers, as it uses restore.
 *)

functor CopySporkSpwnVars(S: RSSA_TRANSFORM_STRUCTS): RSSA_TRANSFORM = 
struct

open S

datatype placement =
  HopefullyTemporary
| DefinitelyStack

type varinfo =
  { criticalSporks: Label.t list ref  (* all sporks where this var is live down both branches *)
  , placement: placement ref
  , copy: Var.t option ref
  }

fun newVarInfo () : varinfo = 
  { criticalSporks = ref []
  , placement = ref HopefullyTemporary
  , copy = ref NONE
  }

fun transform p =
   let
      val {get=varTy, set=setVarTy, ...} =
         Property.getSetOnce
         (Var.plist, Property.initRaise ("CopySporkSpwnVars.varTy", Var.layout))

      val {get=varInfo, set=setVarInfo, rem = remVarInfo, ...} =
         Property.getSet
         (Var.plist, Property.initFun (fn _ => newVarInfo ()))

      fun shouldCopyVar v =
        let
          val {criticalSporks, placement, ...} = varInfo v
        in
          not (List.isEmpty (!criticalSporks))
          andalso !placement = HopefullyTemporary
        end


      fun transformFunc func =
         let
            val _ = Function.foreachDef (func, setVarTy)

            val {args, blocks, name, raises, returns, start} = Function.dest func

            val liveInfo =
               let
                  fun extraEdges (Block.T {kind, ...}) =
                     case kind of
                        Kind.Cont {handler, ...} =>
                           (* Make sure that a cont's live vars
                            * includes variables live in its handler.
                            *)
                           Handler.foldLabel (handler, [], op::)
                      | _ => []
               in
                  Live.live (func, {extraEdges = extraEdges,
                                    shouldConsider = fn _ => true})
               end

            fun beginNoFormals label = #beginNoFormals ((#labelLive liveInfo) label)

           
            val {get=labelInfo, ...} = Property.get
               (Label.plist, Property.initFun
                  (fn l => {isSporkCont = ref false, block = ref NONE}))

            (* ===============================================================
             * Set label info, and also set criticalSporks for every var
             *)

            val _ = Vector.foreach (blocks, fn b as Block.T {label, transfer, ...} =>
              ( #block (labelInfo label) := SOME b
              ; case transfer of
                  Transfer.Spork {cont, spwn, ...} =>
                    let
                      val _ = #isSporkCont (labelInfo cont) := true

                      val contLive = beginNoFormals cont
                      val spwnLive = beginNoFormals spwn
                        
                      val bothLive = Vector.keepAll (spwnLive, fn v =>
                        Vector.contains (contLive, v, Var.equals))
                    in
                      Vector.foreach (bothLive, fn v =>
                        List.push (#criticalSporks (varInfo v), label))
                    end
                
                | _ => ()
              ))
            
            (* ===============================================================
             * placement info for every var
             *)

            (* Alternatively: look for Cont / CReturn reachability ignoring
             * spwn edges...
             *)

            (* val _ = Vector.foreach (blocks,
               fn b as Block.T {label, kind, ...} =>
                  let
                    fun markLiveAsDefinitelyStack () =
                      let
                        val live = beginNoFormals label
                      in
                        Vector.foreach (live, fn v =>
                          #placement (varInfo v) := DefinitelyStack)
                      end
                  in
                    case kind of
                      Kind.Cont _ =>
                        markLiveAsDefinitelyStack ()
                    | Kind.CReturn {func} =>
                        if CFunction.mayGC func then
                          markLiveAsDefinitelyStack ()
                        else
                          ()
                    | _ => ()
                  end) *)

            val _ = Control.diagnostics (fn show =>
              Function.foreachDef (func, fn (v, _) =>
                let
                  open Layout
                  val {criticalSporks, placement, copy} = varInfo v
                in
                  show (Var.layout v);
                  show (seq ([str "criticalSporks "] @ List.map (!criticalSporks, Label.layout)));
                  show (seq [str "placement ", str (case !placement of DefinitelyStack => "DefinitelyStack" | _ => "HopefullyTemporary")]);
                  case !copy of
                    NONE => ()
                  | SOME v' => show (seq [str "copy ", Var.layout v'])
                end))

            (* ===============================================================
             * Make a new version of variables that should be copied
             *)

            val _ = Function.foreachDef (func, (fn (v, _) =>
              if not (shouldCopyVar v) then () else
              let
                val {copy, ...} = varInfo v
                val v' = Var.new v
              in
                copy := SOME v'
              end))

            fun getVarCopy v =
              valOf (! (#copy (varInfo v)))

            (* ===============================================================
             * For every variable v that needs to be copied, create a copy
             * statement immediately after its definition:
             *   v = ...
             *   v' = v
             * Then, inside of every SporkSpwn block, we "uncopy":
             *   v = v'
             * This guarantees that v' (instead of v) is live at the SporkSpwn.
             * We rely on a Restore pass to fix up the SSA condition.
             *)

            fun makeCopyStatement (v, ty) =
              Statement.Bind
                { dst = (getVarCopy v, ty)
                , pinned = true
                , src = Operand.Var {var=v, ty=ty}
                }

            fun makeUncopyStatement (v, ty) =
              Statement.Bind
                { dst = (v, ty)
                , pinned = true
                , src = Operand.Var {var = getVarCopy v, ty = ty}
                }

            val funcArgsToCopy = Vector.keepAll (args, shouldCopyVar o #1)
            val funcArgCopyStmts = Vector.map (funcArgsToCopy, makeCopyStatement)
              
            fun processBlock (b as Block.T {args, kind, label, statements, transfer}) =
              let
                val argsToCopy = Vector.keepAll (args, shouldCopyVar o #1)
                val argCopyStmts = Vector.map (argsToCopy, makeCopyStatement)

                fun makeUncopies () =
                  let
                    val live = beginNoFormals label
                    val varsToUncopy = Vector.keepAll (live, shouldCopyVar)
                  in
                    Vector.map (varsToUncopy, fn v =>
                      makeUncopyStatement (v, varTy v))
                  end

                val slowBlock =
                  case kind of
                    Kind.SporkSpwn _ => true
                  | Kind.SpoinSync _ => true
                  | Kind.Cont _ => true
                  | Kind.CReturn {func} => CFunction.mayGC func
                  | _ => false

                val uncopyStmts =
                  if slowBlock then makeUncopies () else Vector.new0 ()

                val newStmts = ref []

                fun processStatement s =
                  ( List.push (newStmts, s)
                  ; Statement.foreachDef (s, fn (v, ty) => 
                      if shouldCopyVar v then
                        List.push (newStmts, makeCopyStatement (v, ty))
                      else ())
                  )

                val _ = Vector.foreach (statements, processStatement)
                val statements = Vector.concat
                  [ if Label.equals (label, start)
                    then funcArgCopyStmts
                    else Vector.new0 ()
                  , argCopyStmts
                  , uncopyStmts
                  , Vector.fromListRev (!newStmts)
                  ]
              in
                Block.T {args=args, kind=kind, label=label, statements=statements, transfer=transfer}
              end

            val newBlocks = Vector.map (blocks, processBlock)
         in
            Function.new
               {args=args, blocks=newBlocks,
                name=name, raises=raises,
                returns=returns, start=start}
         end

      val Program.T {functions, handlesSignals, main, objectTypes, profileInfo, statics} = p
      val main = transformFunc main
      val {main, restore} = restoreFunction {main = main, statics = statics}
      val () = Function.foreachDef (main, remVarInfo o #1)
      val {main, shrink} = shrinkFunction {main = main, statics = statics}
      val functions = List.revMap (functions, shrink o restore o transformFunc)
      val main = main ()
   in
      Program.T {functions = functions,
                 handlesSignals = handlesSignals,
                 main = main,
                 objectTypes = objectTypes,
                 profileInfo = profileInfo,
                 statics = statics}
   end
end

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

           
            (* ===============================================================
             * Set criticalSporks for every var
             *)

            val _ = Vector.foreach (blocks, fn b as Block.T {label, transfer, ...} =>
              case transfer of
                Transfer.Spork {cont, spwn, ...} =>
                   let
                      val contLive = beginNoFormals cont
                      val spwnLive = beginNoFormals spwn
                      
                      val bothLive = Vector.keepAll (spwnLive, fn v =>
                        Vector.contains (contLive, v, Var.equals))
                   in
                      Vector.foreach (bothLive, fn v =>
                         List.push (#criticalSporks (varInfo v), label))
                   end
              
              | _ => ())
            
            (* ===============================================================
             * placement info for every var
             *)

            val _ = Vector.foreach (blocks,
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
                  end)

            (* ===============================================================
             * TODO: Generate a duplicate for every var that should be copied:
             *   - set #copy (varInfo v) := v'
             *   - rewrite blocks to insert statement v' = v after every def v = ...
             *)

            (* ===============================================================
             * TODO: Insert renaming blocks for copied vars
             *)
         in
           raise Fail "BounceSporkSpwnVars: not yet implemented"
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

(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature DIRECT_EXP_STRUCTS =
  sig
     include SSA_TREE
  end

signature DIRECT_EXP =
  sig
     include DIRECT_EXP_STRUCTS

     structure DirectExp:
        sig
           type t

           datatype cases =
              Con of {con: Con.t,
                      args: (Var.t * Type.t) vector,
                      body: t} vector
            | Word of WordSize.t * (WordX.t * t) vector

           val bug: string -> t
           val call: {func: Func.t, args: t vector, inline: InlineAttr.t, ty: Type.t} -> t
           val casee: {test: t, 
                       cases: cases,
                       default: t option,
                       ty: Type.t} -> t
           val conApp: {con: Con.t, 
                        args: t vector,
                        ty: Type.t} -> t
           val const: Const.t -> t
           val detuple: {body: Var.t vector -> t,
                         length: int,
                         tuple: t} -> t
           val detupleBind: {body: t,
                             components: Var.t vector,
                             tuple: Var.t,
                             tupleTy: Type.t} -> t
           val eq: t * t * Type.t -> t
           val falsee: t
           val handlee: {try: t,
                         ty: Type.t,
                         catch: Var.t * Type.t,
                         handler: t} -> t
           val layout: t -> Layout.t
           val lett: {decs: {var: Var.t, exp: t} list,
                      body: t} -> t
           val linearize:
              t * Return.Handler.t -> Label.t * Block.t list
           val linearizeGoto:
              t * Return.Handler.t * Label.t -> Label.t * Block.t list
           val name: t * (Var.t -> t) -> t
           (* TODO: I don't think this needs to be messed with *)
           val spork: {spid: Spid.t, cont: t, spwn: t, ty: Type.t} -> t
           val spoin: {spid: Spid.t, seq: t, sync: t, ty: Type.t} -> t
           val primApp: {args: t vector,
                         prim: Type.t Prim.t,
                         targs: Type.t vector, 
                         ty: Type.t} -> t
           val profile: ProfileExp.t -> t
           val raisee: t -> t
           val select: {tuple: t, 
                        offset: int, 
                        ty: Type.t} -> t
           val truee: t
           (* try {exp, ty, valCont, exnCont} evaluates t;
            * if it yields a value, bind it to (#arg valCont) and evaluate (#body valCont);
            * if it raises an exn, bind it to (#arg exnCont) and evaluate (#body exnCont).
            * This is not the same as
            *   handlee {try = lett {decs = [{var = #1 (#arg valCont),
            *                                 exp = exp}],
            *                        body = #body valCont},
            *            ty = ty,
            *            catch = #arg exnCont,
            *            handler = #body exnCont}
            * because it doesn't evaluate (#body valCont) in the context of the
            * exnCont handler.
            * See "Exceptional Syntax" by Benton and Kennedy.
            *)
           val try: {exp: t,
                     ty: Type.t,
                     valCont: {arg: Var.t * Type.t,
                               body: t},
                     exnCont: {arg: Var.t * Type.t,
                               body: t}} -> t
           val tuple: {exps: t vector, ty: Type.t} -> t
           val unit: t
           val var: Var.t * Type.t -> t
           val word: WordX.t -> t
        end
  end

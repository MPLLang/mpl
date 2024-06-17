(* Copyright (C) 2024 Colin McDonald.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature SPORK_NESTING_STRUCTS =
sig
  include RSSA_TRANSFORM_STRUCTS
end

signature SPORK_NESTING =
sig
  include SPORK_NESTING_STRUCTS

val nesting: Function.t -> {maxSporkNestLength: int,
                            spidInfo: Spid.t -> {index: int,
                                                 spwn: Label.t},
                            sporkDataTy: Type.t option,
                            sporkNest: Label.t -> Spid.t vector}
end

functor SporkNesting (S : SPORK_NESTING_STRUCTS): SPORK_NESTING =
struct
  open S

  fun nesting (f: Function.t) : {maxSporkNestLength: int,
                                 spidInfo: Spid.t -> {index: int,
                                                      spwn: Label.t},
                                 sporkDataTy: Type.t option,
                                 sporkNest: Label.t -> Spid.t vector} =
     let
        val maxSporkNestLength = ref 0
        val sporkDataTy = ref NONE
        val {get = labelInfo, set = setLabelInfo, rem = remLabelInfo, ...} =
           Property.getSetOnce
           (Label.plist,
            Property.initRaise ("Rssa.SporkNesting.nesting.labelInfo", Label.layout))
        val {get = sporkNest, rem = remSporkNest, ...} =
           Property.getSetOnce
           (Label.plist,
            Property.initFun
            (fn l =>
             let
                val {sporkNest, ...} = labelInfo l
                val () = remLabelInfo l
             in
                Vector.fromList (valOf (!sporkNest))
             end))
        val {get = spidInfo, set = setSpidInfo, rem = remSpidInfo, ...} =
           Property.getSetOnce
           (Spid.plist,
            Property.initRaise ("Rssa.SporkNesting.nesting.spidInfo", Spid.layout))

        val {name, start, blocks, ...} = Function.dest f
        val _ = Vector.foreach (blocks, fn b =>
                                setLabelInfo (Block.label b,
                                              {block = b,
                                               sporkNest = ref NONE}))
        fun goto (l: Label.t, sporkNest: Spid.t list) =
           let
              val {block, sporkNest = sporkNestRef} = labelInfo l
           in
              case !sporkNestRef of
                 NONE =>
                    let
                       val _ = sporkNestRef := SOME sporkNest
                       val sporkNestLength = List.length sporkNest
                       val _ = maxSporkNestLength := Int.max (!maxSporkNestLength, sporkNestLength)
                       val Block.T {kind, args, transfer, ...} = block
                       val _ =
                          case kind of
                             Kind.SporkSpwn _ =>
                                sporkDataTy := SOME (#2 (Vector.first args))
                           | Kind.SpoinSync _ =>
                                sporkDataTy := SOME (#2 (Vector.first args))
                           | _ => ()
                       val _ =
                          case transfer of
                             Transfer.Spork {spid, cont, spwn} =>
                                (setSpidInfo (spid, {index = sporkNestLength,
                                                     spwn = spwn})
                                 ; goto (cont, spid::sporkNest)
                                 ; goto (spwn, []))
                           | Transfer.Spoin {spid, seq, sync} =>
                                (case sporkNest of
                                    [] => Error.bug "empty sporkNest at Spoin"
                                  | _::sporkNest =>
                                       (goto (seq, sporkNest)
                                        ; goto (sync, sporkNest)))
                           | _ =>
                                    Transfer.foreachLabel
                                    (transfer, fn l => goto (l, sporkNest))
                    in
                       ()
                    end
               |SOME _ => ()
           end
        val _ = goto (start, [])
        val _ = Vector.foreach (blocks, fn Block.T {label, ...} =>
                                ignore (sporkNest label))
     in
        {maxSporkNestLength = !maxSporkNestLength,
         spidInfo = spidInfo,
         sporkDataTy = !sporkDataTy,
         sporkNest = sporkNest}
     end
end

(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor VibeArm64Layout (S: VIBE_ARM64_LAYOUT_STRUCTS): VIBE_ARM64_LAYOUT =
struct

open S
structure Machine = Analysis.Machine
open Machine

val switchCases = Analysis.switchCases

fun layout {analysis = {blocks, ifConvert, labelBlock, loopDistance,
                        ...}: Analysis.t,
            chunkLabel: ChunkLabel.t,
            labelChunk: Label.t -> ChunkLabel.t,
            labelIndex: Label.t -> int}: Block.t list =
   let
      (* Block layout: greedily extend a trace with the successor that is
       * most likely to be next.  For a two-way switch, that is the target
       * that the backend expects, else the one that is deeper in the loop
       * nest, else the one that isn't cold, else the default.
       *)
      val order: Block.t list ref = ref []
      val pending: Label.t list ref = ref []
      val {get = placedRef: Label.t -> bool ref, ...} =
         Property.get (Label.plist, Property.initFun (fn _ => ref false))
      fun placed l = ! (placedRef l)
      fun cold (l, depth) =
         case labelBlock l of
            Block.T {transfer, ...} =>
               (case transfer of
                   Transfer.CCall {return = NONE, ...} => true
                 | Transfer.CCall {return = SOME {size = SOME _, ...}, ...} =>
                      true
                 | Transfer.Goto l => depth > 0 andalso cold (l, depth - 1)
                 | Transfer.Raise _ => true
                 | _ => false)
      fun trace from =
         if placed from then ()
         else
            let
               val l = from
               val b as Block.T {transfer, ...} = labelBlock l
               val () = placedRef l := true
               val () = List.push (order, b)
               (* the successors that could fall through, best first *)
               val succs =
                  case transfer of
                     Transfer.CCall {return = SOME {return, ...}, ...} =>
                        [return]
                   | Transfer.Call {label, ...} =>
                        if ChunkLabel.equals (labelChunk label, chunkLabel)
                           then [label]
                        else []
                   | Transfer.Goto l => [l]
                   | Transfer.Spork {cont, ...} => [cont]
                   | Transfer.Switch (Switch.T {cases, default, expect, ...}) =>
                        (case (ifConvert transfer,
                               switchCases (cases, default)) of
                            (SOME {join, ...}, _) => [join]
                          | (NONE, ([(w, l)], l')) =>
                               let
                                  fun byCold () =
                                     cold (l', 2) andalso not (cold (l, 2))
                                  (* prefer to stay in, or go deeper into,
                                   * a loop
                                   *)
                                  fun byLoop () =
                                     case (loopDistance (from, l),
                                           loopDistance (from, l')) of
                                        (SOME _, NONE) => true
                                      | (NONE, SOME _) => false
                                      | (SOME d, SOME d') =>
                                           if d = d' then byCold () else d > d'
                                      | (NONE, NONE) => byCold ()
                               in
                                  if (case expect of
                                         SOME w' => WordX.equals (w, w')
                                       | NONE => byLoop ())
                                     then [l, l']
                                  else [l', l]
                               end
                          | (NONE, (cases, l)) => l :: List.map (cases, #2))
                   | _ => []
            in
               case List.peek (succs, not o placed) of
                  NONE => ()
                | SOME l => (List.foreach (succs, fn l =>
                                           List.push (pending, l))
                             ; trace l)
            end
      fun drain () =
         case !pending of
            [] => ()
          | l :: ls => (pending := ls; trace l; drain ())
      val entries =
         List.insertionSort
         (Vector.fold (blocks, [], fn (Block.T {kind, label, ...}, es) =>
                       if Kind.isEntry kind
                          then (labelIndex label, label) :: es
                       else es),
          fn ((i1, _), (i2, _)) => i1 <= i2)
      val () = List.foreach (entries, fn (_, l) => (trace l; drain ()))
   in
      List.rev (!order)
   end

end

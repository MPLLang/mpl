(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Block layout for the vibe-arm codegen: the order in which to output the
 * blocks of a chunk, chosen so that the likely successor of a block comes
 * right after it and is reached by falling through.
 *)

signature VIBE_ARM_LAYOUT_STRUCTS =
   sig
      structure Analysis: VIBE_ARM_ANALYSIS
   end

signature VIBE_ARM_LAYOUT =
   sig
      include VIBE_ARM_LAYOUT_STRUCTS

      (* The blocks of the chunk that are reachable from its entry blocks,
       * in the order to output them.  (Blocks that aren't reachable, such
       * as the arms of if-converted switches, are not output.)
       *)
      val layout:
         {analysis: Analysis.t,
          chunkLabel: Analysis.Machine.ChunkLabel.t,
          labelChunk: Analysis.Machine.Label.t -> Analysis.Machine.ChunkLabel.t,
          labelIndex: Analysis.Machine.Label.t -> int}
         -> Analysis.Machine.Block.t list
   end

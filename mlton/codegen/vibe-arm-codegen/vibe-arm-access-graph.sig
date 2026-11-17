(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Access to the operands of the Machine IR (see vibe-arm-access.sig) under
 * the graph register allocator of the vibe-arm codegen.
 *)

signature VIBE_ARM_ACCESS_GRAPH_STRUCTS =
   sig
      structure Access: VIBE_ARM_ACCESS
   end

signature VIBE_ARM_ACCESS_GRAPH =
   sig
      include VIBE_ARM_ACCESS_GRAPH_STRUCTS

      val new: Access.args -> Access.t
   end

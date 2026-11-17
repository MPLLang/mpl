(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Access to the operands of the Machine IR (see vibe-arm-access.sig) under
 * the local register allocator of the vibe-arm codegen.
 *)

signature VIBE_ARM_ACCESS_LOCAL_STRUCTS =
   sig
      structure Access: VIBE_ARM_ACCESS
   end

signature VIBE_ARM_ACCESS_LOCAL =
   sig
      include VIBE_ARM_ACCESS_LOCAL_STRUCTS

      val new: Access.args -> Access.t
   end

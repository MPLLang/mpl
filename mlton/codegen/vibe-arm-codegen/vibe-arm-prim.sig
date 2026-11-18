(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* The primitives that the vibe-arm codegen implements: the instructions
 * for an application of one.  It gets at its arguments and its destination
 * through VibeArmAccess.
 *
 * Registers x0-x2 and d16-d18 hold the arguments, when they aren't in
 * registers already; x5-x7 and x16 are scratch.
 *)

signature VIBE_ARM_PRIM_STRUCTS =
   sig
      structure Access: VIBE_ARM_ACCESS
   end

signature VIBE_ARM_PRIM =
   sig
      include VIBE_ARM_PRIM_STRUCTS

      type t = {access: Access.t,
                emit: Access.Emit.t}

      (* Output a primitive application: compute it, and store the result
       * to dst (and, for an overflow check, the result of the operation
       * itself to "also").
       *)
      val app: t * Access.Items.app -> unit
      (* The same, for an application whose result is only branched on.  If
       * the primitive computes a condition (a comparison, an overflow
       * check), it is left in the flags, nothing is stored to dst, and the
       * result is the condition.  If not, this is as app, and the result
       * is NONE.
       *)
      val appFlags: t * Access.Items.app -> Access.Emit.Arm64.Cond.t option
   end

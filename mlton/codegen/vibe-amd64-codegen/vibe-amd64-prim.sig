(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* The primitives of the Machine IR, for the vibe-amd64 codegen. *)

signature VIBE_AMD64_PRIM_STRUCTS =
   sig
      structure Access: VIBE_AMD64_ACCESS
   end

signature VIBE_AMD64_PRIM =
   sig
      include VIBE_AMD64_PRIM_STRUCTS

      type t = {access: Access.t,
                emit: Access.Emit.t}

      (* Output a primitive application: compute it, and store the result
       * to dst (and, for an overflow check, the result of the operation
       * itself to "also").
       *)
      val app: t * Access.Items.app -> unit
      (* The same, for an application whose result is only branched on: if
       * the primitive computes a condition (a comparison, an overflow
       * check), it is left in the flags, nothing is stored to dst, and the
       * result is the condition.  If not, this is as app, and the result
       * is NONE.
       *)
      val appFlags: t * Access.Items.app -> Access.Emit.Asm.Cond.t option
      (* Output a select (see VibeAmd64Analysis.isSelect).  If there is a
       * condition, then the flags hold it, and it holds if the test of
       * the select is 1 (and doesn't if it is 0): the test isn't read.
       * The flags are left as they were.
       *)
      val sel: t * Access.Items.app * Access.Emit.Asm.Cond.t option -> unit
      (* Output a comparison of reals and the selects of reals that test
       * its result, without leaving the xmm registers: as a minimum or a
       * maximum where a select is one, and otherwise with a mask.  Also
       * does "defined" for each destination.  Returns false, having
       * output nothing, if it doesn't apply.
       *)
      val selReal: t * Access.Items.app * Access.Items.app list -> bool
   end

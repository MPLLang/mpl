(* MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* The statements of a block, as the vibe-arm64 codegen outputs them: with
 * statements that are better output together combined into one item.
 *
 *  - An operation and its overflow check, on the same arguments, are one
 *    item, so that the arithmetic is only done once.
 *  - A shift right and then a mask of the low bits are one item (a bitfield
 *    extract).
 *  - A primitive whose result is only copied to a stack slot computes into
 *    the slot.
 *)

signature VIBE_ARM64_ITEMS_STRUCTS =
   sig
      structure Analysis: VIBE_ARM64_ANALYSIS
   end

signature VIBE_ARM64_ITEMS =
   sig
      include VIBE_ARM64_ITEMS_STRUCTS

      type app = {args: Analysis.Machine.Operand.t vector,
                  dst: Analysis.Machine.Operand.t,
                  prim: Analysis.Machine.Type.t Analysis.Machine.Prim.t,
                  (* An overflow-checking primitive also stores the result
                   * of the operation itself to this operand; the bool is
                   * true if the check originally came before the
                   * operation.
                   *)
                  also: (Analysis.Machine.Operand.t * bool) option}

      datatype t =
         App of app
       | Move of {dst: Analysis.Machine.Operand.t,
                  src: Analysis.Machine.Operand.t}
         (* a definition that was folded into the previous item *)
       | Defined of Analysis.Machine.Operand.t

      (* the items of a block *)
      val ofBlock: Analysis.t * Analysis.Machine.Block.t -> t list
      (* Is this a block that ends by computing a boolean temporary that is
       * only used to choose between two successors?  If so, return the
       * other items, the final one, and the targets for true and false.
       *)
      val fusable:
         Analysis.t * Analysis.Machine.Block.t * t list
         -> (t list * app
             * Analysis.Machine.Label.t * Analysis.Machine.Label.t) option
   end

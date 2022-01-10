/* Copyright (C) 2020 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

/** We use the COUNTER field from the header, so these values need to
  * line up with those in object.h.
  *
  *                 +------+-------------------+----------+--------------+
  *  header fields  | mark |      counter      | type-tag | valid-header |
  *                 +------+-----+-------------+----------+--------------+
  *     sub-fields  |      | pin | unpin-depth |          |              |
  *                 +------+-----+-------------+----------+--------------+
  *                 ^      ^     ^             ^          ^              ^
  *        offsets  32     31    30            20         1              0
  *
  */
#define UNPIN_DEPTH_BITS  10
#define UNPIN_DEPTH_MASK  ((GC_header)0x3FF00000)
#define UNPIN_DEPTH_SHIFT 20
#define PIN_BITS          1
#define PIN_MASK          ((GC_header)0x40000000)
#define PIN_SHIFT         30

COMPILE_TIME_ASSERT(pinning_masks_compatible_with_counter_mask,
  (PIN_MASK | UNPIN_DEPTH_MASK) == COUNTER_MASK
);

COMPILE_TIME_ASSERT(pinning_bits_compatible_with_counter_bits,
  (PIN_BITS + UNPIN_DEPTH_BITS) == COUNTER_BITS
);

/* Pin this object, making it immovable (by GC) until it reaches
 * unpinDepth (or shallower). Returns TRUE if the object was
 * previously unpinned.
 *
 * Note that regardless of whether or not the object was previously
 * pinned, this does a writeMin on the unpinDepth of the object.
 */
bool pinObject(objptr op, uint32_t unpinDepth);

/* check if an object is pinned */
bool isPinned(objptr op);

/* Unpin an object by clearing the mark and counter bits in its header.
 * This is only safe if the object is not being concurrently pinned.
 * As long as we only call this on objects that are local, it's safe.
 */
void unpinObject(objptr op);

/* read the current unpin-depth of an object */
uint32_t unpinDepthOf(objptr op);

#endif

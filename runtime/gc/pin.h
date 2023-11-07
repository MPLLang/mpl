/* Copyright (C) 2020 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

/** We use the COUNTER field from the header, so these values need to
  * line up with those in object.h.
  *
  *                 +------+-------------------------+----------+--------------+
  *  header fields  | mark |      counter            | type-tag | valid-header |
  *                 +------+------------+-------------+----------+--------------+
  *     sub-fields  |      | sus | pin  | unpin-depth |          |              |
  *                 +------+-----+------+-------------+----------+--------------+
  *                 ^      ^     ^      ^             ^          ^              ^
  *        offsets  32     31    30     28            20         1              0
  *
  */
#define UNPIN_DEPTH_BITS  8
#define UNPIN_DEPTH_MASK ((GC_header)0xFF00000)
#define UNPIN_DEPTH_SHIFT 20
#define PIN_MASK ((GC_header)0x30000000)
#define PIN_SHIFT         28

enum PinType
{
  PIN_NONE,
  PIN_DOWN,
  PIN_ANY
};

/* Pin this object, making it immovable (by GC) until it reaches
 * unpinDepth (or shallower). Returns TRUE if the object was
 * previously unpinned.
 *
 * Note that regardless of whether or not the object was previously
 * pinned, this does a writeMin on the unpinDepth of the object.
 */
bool pinObject(GC_state s, objptr op, uint32_t unpinDepth, enum PinType pt);

objptr pinObjectInfo(
  GC_state s,
  objptr op,
  uint32_t unpinDepth,
  enum PinType pt,
  bool* headerChange,
  bool* pinChange);

/* check if an object is pinned */
bool isPinned(objptr op);

/* */
enum PinType pinType(GC_header header);

/* Unpin an object by clearing the mark and counter bits in its header.
 * This is only safe if the object is not being concurrently pinned.
 * As long as we only call this on objects that are local, it's safe.
 */
void unpinObject(objptr op);

/* read the current unpin-depth of an object */
uint32_t unpinDepthOf(objptr op);
uint32_t unpinDepthOfH(GC_header header);


/* unpin an object if its depth allows. Because the unpinDepth can change
 * concurrently, we want to make sure we use the logic in this function.
 * If unpin is successful, then it returns true. Otherwise, false.
 */
bool tryUnpinWithDepth(objptr op, uint32_t opDepth);
// bool tryPinDec(objptr op, uint32_t opDepth);

#endif

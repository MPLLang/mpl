/* Copyright (C) 2020 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

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

/* Copyright (C) 2020 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

bool pinObject(objptr op, uint32_t unpinDepth)
{
  pointer p = objptrToPointer(op, NULL);

  uint32_t maxUnpinDepth = TWOPOWER(UNPIN_DEPTH_BITS) - 1;
  if (unpinDepth > maxUnpinDepth) {
    DIE("unpinDepth %"PRIu32" exceeds max possible value %"PRIu32,
        unpinDepth,
        maxUnpinDepth);
    return FALSE;
  }
  assert(
    ((GC_header)unpinDepth) << UNPIN_DEPTH_SHIFT
    == (UNPIN_DEPTH_MASK & ((GC_header)unpinDepth) << UNPIN_DEPTH_SHIFT)
  );

  while (TRUE) {
    GC_header header = getHeader(p);

    bool notPinned = (0 == (header & PIN_MASK) >> PIN_SHIFT);
    uint32_t previousUnpinDepth =
      (header & UNPIN_DEPTH_MASK) >> UNPIN_DEPTH_SHIFT;

    GC_header newHeader =
        (header & (~UNPIN_DEPTH_MASK))                // clear unpin bits
      | ((GC_header)unpinDepth << UNPIN_DEPTH_SHIFT)  // put in new unpinDepth
      | PIN_MASK;                                     // set pin bit

    if (notPinned) {
      /* first, handle case where this object was not already pinned */
      if (__sync_bool_compare_and_swap(getHeaderp(p), header, newHeader))
        return TRUE;
    }
    else {
      /* if the object was previously pinned, we still need to do a writeMin */
      if ( (previousUnpinDepth <= unpinDepth)
           || __sync_bool_compare_and_swap(getHeaderp(p), header, newHeader) )
        return FALSE;
    }
  }

  DIE("should be impossible to reach here");
  return FALSE;
}

void unpinObject(objptr op) {
  pointer p = objptrToPointer(op, NULL);

  GC_header newHeader =
      getHeader(p)
    & (~UNPIN_DEPTH_MASK)  // clear counter bits
    & (~PIN_MASK);         // clear mark bit

  *(getHeaderp(p)) = newHeader;
}

bool isPinned(objptr op) {
  pointer p = objptrToPointer(op, NULL);
  GC_header h = getHeader(p);

  /* have to check first that the header is valid
   * (otherwise, there could be a forward pointer in this spot)
   * ...and then check the mark
   */
  return (1 == (h & GC_VALID_HEADER_MASK)) &&
         (1 == ((h & PIN_MASK) >> PIN_SHIFT));
}

uint32_t unpinDepthOf(objptr op) {
  pointer p = objptrToPointer(op, NULL);
  uint32_t d = (getHeader(p) & UNPIN_DEPTH_MASK) >> UNPIN_DEPTH_SHIFT;
  return d;
}

#endif

/* Copyright (C) 2020 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

enum PinType pinType(GC_header h) {
  if (0 == (h & GC_VALID_HEADER_MASK))
    return PIN_NONE;

  int t = ((h & PIN_MASK) >> PIN_SHIFT);
  if (t == 0)
    return PIN_NONE;
  else if (t == 2)
    return PIN_DOWN;
  else if (t == 3)
    return PIN_ANY;
  else
    DIE("NOT supposed to reach here!");
}

enum PinType maxPT(enum PinType pt1, enum PinType pt2) {
  if (pt1 == PIN_NONE)
    return pt2;
  else if (pt1 == PIN_ANY || pt2 == PIN_ANY)
    return PIN_ANY;
  else
    return PIN_DOWN;
}

static inline GC_header getRep(enum PinType pt)
{
  GC_header h;
  if (pt == PIN_NONE)
    h = 0;
  else if (pt == PIN_DOWN)
    h = 0x20000000;
  else
    h = 0x30000000;
  return h;
}

uint32_t unpinDepthOfH(GC_header h)
{
  return (h & UNPIN_DEPTH_MASK) >> UNPIN_DEPTH_SHIFT;
}

bool pinObject(objptr op, uint32_t unpinDepth, enum PinType pt)
{
  bool a, b;
  pinObjectInfo(op, unpinDepth, pt, &a, &b);
  return a;
}

objptr pinObjectInfo(objptr op, uint32_t unpinDepth, enum PinType pt,
                      bool *headerChange, bool *pinChange)
{
  pointer p = objptrToPointer(op, NULL);
  assert(pt != PIN_NONE);

  /*initialize with false*/
  *headerChange = false;
  *pinChange = false;

  uint32_t maxUnpinDepth = TWOPOWER(UNPIN_DEPTH_BITS) - 1;
  if (unpinDepth > maxUnpinDepth)
  {
    DIE("unpinDepth %" PRIu32 " exceeds max possible value %" PRIu32,
        unpinDepth,
        maxUnpinDepth);
    return op;
  }
  assert(
      ((GC_header)unpinDepth) << UNPIN_DEPTH_SHIFT == (UNPIN_DEPTH_MASK & ((GC_header)unpinDepth) << UNPIN_DEPTH_SHIFT));
  while (true)
  {
    GC_header header = getHeader(p);
    uint32_t newUnpinDepth;
    if (isFwdHeader(header))
    {
      assert(pt != PIN_DOWN);
      op = getFwdPtr(p);
      p = objptrToPointer(op, NULL);
      continue;
    }
    else if (pinType(header) != PIN_NONE)
    {
      uint32_t previousUnpinDepth = unpinDepthOfH(header);
      newUnpinDepth = min(previousUnpinDepth, unpinDepth);
    } else {
      newUnpinDepth = unpinDepth;
    }

    /* if we are changing the unpinDepth, then the new pinType (nt) is
     * equal to the function argument pt. Otherwise its the max. */
    enum PinType nt = newUnpinDepth < unpinDepthOfH(header) ? pt : maxPT(pt, pinType(header));
    GC_header unpinnedHeader = header & (~UNPIN_DEPTH_MASK) & (~PIN_MASK);

    GC_header newHeader =
        unpinnedHeader
        | ((GC_header)newUnpinDepth << UNPIN_DEPTH_SHIFT) // put in new unpinDepth
        | getRep(nt);                                     // setup the pin type

    if(newHeader == header) {
      assert (!hasFwdPtr(p));
      return op;
    }
    else {
      if (__sync_bool_compare_and_swap(getHeaderp(p), header, newHeader)) {
        *headerChange = true;
        *pinChange = (nt != pinType(header));
        assert (!hasFwdPtr(p));
        assert(pinType(newHeader) == nt);
        return op;
      }
    }
  }
  DIE("should be impossible to reach here");
  return op;
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
  bool result = (1 == (h & GC_VALID_HEADER_MASK)) &&
         (((h & PIN_MASK) >> PIN_SHIFT) > 0);
  assert (result == (pinType(h) != PIN_NONE));
  return result;
}

uint32_t unpinDepthOf(objptr op) {
  pointer p = objptrToPointer(op, NULL);
  uint32_t d = unpinDepthOfH(getHeader(p));
  return d;
}

bool tryUnpinWithDepth(objptr op, uint32_t opDepth) {

  pointer p = objptrToPointer(op, NULL);
  GC_header header = getHeader(p);
  uint32_t d = unpinDepthOfH(header);

  if (d >= opDepth) {
    GC_header newHeader =
        getHeader(p)
      & (~UNPIN_DEPTH_MASK)  // clear counter bits
      & (~PIN_MASK);         // clear mark bit

    return __sync_bool_compare_and_swap(getHeaderp(p), header, newHeader);
  }
  return false;
}


// bool tryPinDec(objptr op, uint32_t opDepth) {
//   pointer p = objptrToPointer(op, NULL);
//   GC_header header = getHeader(p);
//   uint32_t d = (header & UNPIN_DEPTH_MASK) >> UNPIN_DEPTH_SHIFT;

//   if (d >= opDepth && pinType(header) == PIN_ANY) {
//     GC_header newHeader =
//         getHeader(p)
//       & (~UNPIN_DEPTH_MASK)  // clear counter bits
//       & (~PIN_MASK);         // clear mark bit

//     return __sync_bool_compare_and_swap(getHeaderp(p), header, newHeader));
//   }

//   return false;
// }

#endif

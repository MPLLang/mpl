/* Copyright (C) 2020 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

/* SAM_NOTE: This code hasn't been tested much. I used it briefly for objptr
 * pinning, but then switched to a different implementation strategy.
 *
 * Use with caution.
 */
void fillGap(ARG_USED_FOR_ASSERT GC_state s, pointer start, pointer end) {
  assert(start < end);

  size_t gapSize = (size_t)(end - start);
  assert(gapSize >= GC_NORMAL_METADATA_SIZE);

  if (gapSize >= GC_SEQUENCE_METADATA_SIZE) {
    /* the gap is big enough to use a vector object to fill it */
    pointer fillObjStart = start + GC_SEQUENCE_METADATA_SIZE;
    size_t vectorLen = gapSize - GC_SEQUENCE_METADATA_SIZE;

    *(getHeaderp(fillObjStart)) = GC_WORD8_VECTOR_HEADER;
    *(getSequenceCounterp(fillObjStart)) = 0;
    *(getSequenceLengthp(fillObjStart)) = vectorLen;

    assert(fillObjStart + objectSize(s, fillObjStart) == end);
  }
  else if (gapSize == GC_NORMAL_METADATA_SIZE + 8) {
    /* gap is perfect for a FILL8 object */
    pointer fillObjStart = start + GC_NORMAL_METADATA_SIZE;
    *(getHeaderp(fillObjStart)) = GC_FILL8_NORMAL_HEADER;
    assert(fillObjStart + objectSize(s, fillObjStart) == end);
  }
  else {
    /* use as many FILL0 objects as needed to fill this gap. */
    assert(isAligned(gapSize, GC_NORMAL_METADATA_SIZE));
    pointer p = start;
    while (p < end) {
      p += GC_NORMAL_METADATA_SIZE;
      *(getHeaderp(p)) = GC_FILL0_NORMAL_HEADER;
    }
    assert(p == end);
  }
}

#endif /* MLTON_GC_INTERNAL_FUNCS */

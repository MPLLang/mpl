/* Copyright (C) 2016 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/******************************/
/* Static Function Prototypes */
/******************************/

/**
 * Initializes an array at 'frontier'
 *
 * @param s The GC_state to use
 * @param frontier The start of the array
 * @param arraySize The size of the array in bytes
 * @param numElements Number of elements in the array
 * @param header The array header
 * @param bytesNonObjptrs Number of non-objptr bytes per element
 * @param numObjptrs Number of objptrs per element
 *
 * @return The pointer to the start of the array object, after the headers
 */
static inline pointer arrayInitialize(ARG_USED_FOR_ASSERT GC_state s,
                                      pointer frontier,
                                      size_t arraySize,
                                      GC_arrayLength numElements,
                                      GC_header header,
                                      uint16_t bytesNonObjptrs,
                                      uint16_t numObjptrs);

pointer arrayAllocateInOldGen(GC_state s,
                              size_t arraySizeAligned,
                              size_t ensureBytesFree);

pointer arrayAllocateInGlobal(GC_state s,
                              size_t arraySizeAligned,
                              size_t ensureBytesFree);

pointer arrayAllocateInHH(GC_state s,
                          size_t arraySizeAligned,
                          size_t ensureBytesFree);
/************************/
/* Function Definitions */
/************************/

/* SAM_NOTE: deprecate and remove this function. */
pointer arrayAllocateInOldGen(GC_state s,
                              size_t arraySizeAligned,
                              size_t ensureBytesFree) {
  if (not hasHeapBytesFree (s, arraySizeAligned, ensureBytesFree)) {
    performGC (s, arraySizeAligned, ensureBytesFree, FALSE, TRUE);
  }
  assert (hasHeapBytesFree (s, arraySizeAligned, ensureBytesFree));
  pointer frontier = s->heap->start + s->heap->oldGenSize;
  assert (isFrontierAligned (s, frontier));

  /* SPOONHOWER_NOTE: This must be updated while holding the lock! */
  s->heap->oldGenSize += arraySizeAligned;
  assert (s->heap->start + s->heap->oldGenSize <= s->heap->nursery);
  s->cumulativeStatistics->bytesAllocated += arraySizeAligned;
  return frontier;
}

pointer arrayAllocateInGlobal(GC_state s,
                              size_t arraySizeAligned,
                              size_t ensureBytesFree) {
  ensureBytesFreeInGlobal(s, arraySizeAligned);
  pointer frontier = s->frontier;
  pointer newFrontier = frontier + arraySizeAligned;
  assert(isFrontierAligned(s, newFrontier));

  HM_chunk current = HM_getChunkOf(frontier);
  assert(current == HM_getChunkListLastChunk(s->globalHeap));

  if (inFirstBlockOfChunk(current, newFrontier) &&
      s->limitPlusSlop - newFrontier >= ensureBytesFree) {
    s->frontier = newFrontier;
  } else {
    HM_updateChunkValues(current, newFrontier);
    HM_chunk newChunk = HM_allocateChunk(s->globalHeap, ensureBytesFree);
    s->frontier = HM_getChunkFrontier(newChunk);
    s->limitPlusSlop = HM_getChunkLimit(newChunk);
    s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
  }

  assert(isFrontierAligned(s, s->frontier));
  assert(s->limitPlusSlop - s->frontier >= ensureBytesFree);
  return frontier;
}

pointer arrayAllocateInHH(GC_state s,
                          size_t arraySizeAligned,
                          size_t ensureBytesFree) {
  assert(ensureBytesFree <= s->controls->minChunkSize - sizeof(struct HM_chunk));
  size_t arrayChunkBytes = align(arraySizeAligned, s->controls->minChunkSize);
  size_t bytesRequested = arraySizeAligned + ensureBytesFree;
  bool giveWholeChunk = arraySizeAligned >= s->controls->minChunkSize / 2;
  if (giveWholeChunk) {
    bytesRequested = arrayChunkBytes + s->controls->minChunkSize;
  }

  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
  getThreadCurrent(s)->exnStack = s->exnStack;
  getThreadCurrent(s)->bytesNeeded = ensureBytesFree;
  /* ensure free bytes at the most up-to-date level */
  HM_ensureHierarchicalHeapAssurances(s, FALSE, bytesRequested, TRUE);
  struct HM_HierarchicalHeap *hh = HM_HH_getCurrent(s);

  assert((((size_t)(s->limitPlusSlop)) - ((size_t)(s->frontier))) >=
         bytesRequested);

  if (giveWholeChunk) {
    /* split the large chunk so that we have space for the array at the end;
     * this guarantees that the single chunk holding the array is not a
     * level-head which makes it easy to move it during a GC */
    assert(hh->lastAllocatedChunk->frontier == s->frontier);
    assert(hh->lastAllocatedChunk->limit == s->limitPlusSlop);
    HM_chunk arrayChunk = HM_splitChunk(hh->lastAllocatedChunk, arrayChunkBytes);
    assert(arrayChunk != NULL);
    pointer result = arrayChunk->frontier;
    arrayChunk->frontier += arraySizeAligned;
    arrayChunk->mightContainMultipleObjects = FALSE;

    assert(s->frontier == HM_HH_getFrontier(hh));
    s->limitPlusSlop = HM_HH_getLimit(hh);
    s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
    return result;
  }

  pointer result = s->frontier;
  pointer newFrontier = result + arraySizeAligned;
  assert (isFrontierAligned (s, newFrontier));
  s->frontier = newFrontier;

  if (!inSameBlock(result, s->limitPlusSlop - 1)) {
    /* force a new chunk to be created so that no new objects lie after this
     * array, which crossed a block boundary. */
    HM_HH_updateValues(hh, s->frontier);
    HM_HH_extend(hh, ensureBytesFree);
    s->frontier = HM_HH_getFrontier(hh);
    s->limitPlusSlop = HM_HH_getLimit(hh);
    s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
  }

  return result;
}

pointer GC_arrayAllocate (GC_state s,
                          size_t ensureBytesFree,
                          GC_arrayLength numElements,
                          GC_header header) {
  size_t arraySize, arraySizeAligned;
  size_t bytesPerElement;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
  pointer frontier;
  pointer result;

  splitHeader(s, header, NULL, NULL, &bytesNonObjptrs, &numObjptrs);

  LOG(LM_ALLOCATION, LL_INFO,
      "GC_arrayAllocate (%"PRIuMAX", "FMTARRLEN", "FMTHDR") [%d]",
      (uintmax_t)ensureBytesFree,
      numElements, header,
      Proc_processorNumber (s));

  bytesPerElement = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);

  Trace3(EVENT_ARRAY_ALLOCATE_ENTER,
         (EventInt)ensureBytesFree,
         (EventInt)numElements,
         (EventInt)bytesPerElement);

  /* Check for overflow when computing arraySize.
   */
  if (bytesPerElement > 0 and numElements > (SIZE_MAX / bytesPerElement)) {
    goto doOverflow;
  }

  arraySize = bytesPerElement * numElements;
  if (arraySize > SIZE_MAX - GC_ARRAY_METADATA_SIZE) {
    goto doOverflow;
  }

  arraySize += GC_ARRAY_METADATA_SIZE;
  arraySizeAligned = align (arraySize, s->alignment);
  if (arraySizeAligned < arraySize) {
    goto doOverflow;
  }

  LOG(LM_ALLOCATION, LL_DEBUG,
      "Array with "FMTARRLEN" elts of size %"PRIuMAX" and total size %s and "
      "total aligned size %s. Ensure %s bytes free.",
      numElements,
      (uintmax_t)bytesPerElement,
      uintmaxToCommaString(arraySize),
      uintmaxToCommaString(arraySizeAligned),
      uintmaxToCommaString(ensureBytesFree));

  if (HM_inGlobalHeap(s)) {
    frontier = arrayAllocateInGlobal(s, arraySizeAligned, ensureBytesFree);
  } else {
    assert(!HM_inGlobalHeap(s));
    frontier = arrayAllocateInHH(s, arraySizeAligned, ensureBytesFree);
  }

  result = arrayInitialize(s,
                           frontier,
                           arraySize,
                           numElements,
                           header,
                           bytesNonObjptrs,
                           numObjptrs);

  GC_profileAllocInc (s, arraySizeAligned);

  LOG(LM_ALLOCATION, LL_DEBUG,
      "GC_arrayAllocate done.  result = "FMTPTR"  frontier = "FMTPTR" [%d]",
      (uintptr_t)result,
      (uintptr_t)s->frontier,
      Proc_processorNumber (s));
  /* RAM_NOTE: Use LOG for displayGCState() */
  if (DEBUG_ARRAY) {
    displayGCState (s, stderr);
  }

#if ASSERT
  if (!HM_inGlobalHeap(s)) {
    if (s->frontier != s->limitPlusSlop) {
      assert(inSameBlock(s->frontier, s->limitPlusSlop-1));
      assert(((HM_chunk)blockOf(s->frontier))->magic == CHUNK_MAGIC);
    }
  }
  assert(ensureBytesFree <= (size_t)(s->limitPlusSlop - s->frontier));
  /* Unfortunately, the invariant isn't quite true here, because
   * unless we did the GC, we never set s->currentThread->stack->used
   * to reflect what the mutator did with stackTop.
   */
#endif

  Trace0(EVENT_ARRAY_ALLOCATE_LEAVE);

  return result;

doOverflow:
  DIE("Out of memory. Unable to allocate array with "FMTARRLEN" elements and "
      "elements of size %"PRIuMAX" bytes.",
      numElements,
      (uintmax_t)bytesPerElement);
}

/*******************************/
/* Static Function Definitions */
/*******************************/

static pointer arrayInitialize(ARG_USED_FOR_ASSERT GC_state s,
                               pointer frontier,
                               size_t arraySize,
                               GC_arrayLength numElements,
                               GC_header header,
                               uint16_t bytesNonObjptrs,
                               uint16_t numObjptrs) {
  pointer last = frontier + arraySize;

  *((GC_arrayCounter*)(frontier)) = 0;
  frontier = frontier + GC_ARRAY_COUNTER_SIZE;

  *((GC_arrayLength*)(frontier)) = numElements;
  frontier = frontier + GC_ARRAY_LENGTH_SIZE;

  *((GC_header*)(frontier)) = header;
  frontier = frontier + GC_HEADER_SIZE;

  // *((objptr*)(frontier)) = BOGUS_OBJPTR;
  // frontier = frontier + OBJPTR_SIZE;

  pointer result = frontier;
  assert (isAligned ((size_t)result, s->alignment));

  /* Initialize all pointers with BOGUS_OBJPTR. */
  if (1 <= numObjptrs and 0 < numElements) {
    pointer p;

    if (0 == bytesNonObjptrs)
      for (p = frontier; p < last; p += OBJPTR_SIZE)
        *((objptr*)p) = BOGUS_OBJPTR;
    else {
      /* Array with a mix of pointers and non-pointers. */
      size_t bytesObjptrs;

      bytesObjptrs = numObjptrs * OBJPTR_SIZE;

      for (p = frontier; p < last; ) {
        pointer next;

        p += bytesNonObjptrs;
        next = p + bytesObjptrs;
        assert (next <= last);
        for ( ; p < next; p += OBJPTR_SIZE)
          *((objptr*)p) = BOGUS_OBJPTR;
      }
    }
  }

  return result;
}

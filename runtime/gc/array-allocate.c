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

/************************/
/* Function Definitions */
/************************/

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
  bool holdLock = FALSE;

  splitHeader(s, header, NULL, NULL, &bytesNonObjptrs, &numObjptrs);

  LOG(LM_ALLOCATION, LL_INFO,
      "GC_arrayAllocate (%"PRIuMAX", "FMTARRLEN", "FMTHDR") [%d]",
      (uintmax_t)ensureBytesFree,
      numElements, header,
      Proc_processorNumber (s));

  Trace3(EVENT_ARRAY_ALLOCATE_ENTER,
         (EventInt)ensureBytesFree,
         (EventInt)numElements,
         (EventInt)header);

  /* Check for overflow when computing arraySize.
   */
  bytesPerElement = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
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
    /* Allocate in Global Heap */

    /* Determine whether we will perform this allocation locally or not */
    holdLock = arraySizeAligned >= s->controls->oldGenArraySize;

    if (holdLock) {
      /* Global alloc */
      s->syncReason = SYNC_OLD_GEN_ARRAY;
      ENTER0 (s);
      if (not hasHeapBytesFree (s, arraySizeAligned, ensureBytesFree)) {
        performGC (s, arraySizeAligned, ensureBytesFree, FALSE, TRUE);
      }
      assert (hasHeapBytesFree (s, arraySizeAligned, ensureBytesFree));
      frontier = s->heap->start + s->heap->oldGenSize;
      assert (isFrontierAligned (s, frontier));
      result = arrayInitialize(s,
                               frontier,
                               arraySize,
                               numElements,
                               header,
                               bytesNonObjptrs,
                               numObjptrs);

      /* SPOONHOWER_NOTE: This must be updated while holding the lock! */
      s->heap->oldGenSize += arraySizeAligned;
      assert (s->heap->start + s->heap->oldGenSize <= s->heap->nursery);
      s->cumulativeStatistics->bytesAllocated += arraySizeAligned;
      /* NB LEAVE appears below since no heap invariant holds while the
         oldGenSize has been updated but the array remains uninitialized. */
    } else {
      /* Local alloc */
      size_t bytesRequested;
      pointer newFrontier;

      bytesRequested = arraySizeAligned + ensureBytesFree;
      if (not hasHeapBytesFree (s, 0, bytesRequested)) {
        /* Local alloc may still require getting the lock, but we will release
           it before initialization. */
        ensureHasHeapBytesFreeAndOrInvariantForMutator (s, FALSE, FALSE, FALSE,
                                                        0, bytesRequested);
      }
      assert (hasHeapBytesFree (s, 0, bytesRequested));
      frontier = s->frontier;
      result = arrayInitialize(s,
                               frontier,
                               arraySize,
                               numElements,
                               header,
                               bytesNonObjptrs,
                               numObjptrs);

      newFrontier = frontier + arraySizeAligned;
      assert (isFrontierAligned (s, newFrontier));
      s->frontier = newFrontier;
    }
  } else {
    /* Allocate in Hierarchical Heap */
    size_t bytesRequested = arraySizeAligned + ensureBytesFree;
    /* RAM_NOTE: This should be wrapped in a function */
    /* used needs to be set because the mutator has changed s->stackTop. */
    getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
    getThreadCurrent(s)->exnStack = s->exnStack;
    getThreadCurrent(s)->bytesNeeded = bytesRequested;
    HM_ensureHierarchicalHeapAssurances(s, FALSE, bytesRequested, TRUE);

    assert((((size_t)(s->limitPlusSlop)) - ((size_t)(s->frontier))) >=
           bytesRequested);

    /*
     * at this point, the current chunk has enough space, and is at the right
     * level
     */

    frontier = s->frontier;
    result = arrayInitialize(s,
                             frontier,
                             arraySize,
                             numElements,
                             header,
                             bytesNonObjptrs,
                             numObjptrs);

      pointer newFrontier = frontier + arraySizeAligned;
      assert (isFrontierAligned (s, newFrontier));
      s->frontier = newFrontier;
  }

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

  assert (ensureBytesFree <= (size_t)(s->limitPlusSlop - s->frontier));
  /* Unfortunately, the invariant isn't quite true here, because
   * unless we did the GC, we never set s->currentThread->stack->used
   * to reflect what the mutator did with stackTop.
   */

  if (holdLock) {
    LEAVE1 (s, result);
  }

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

  *((objptr*)(frontier)) = BOGUS_OBJPTR;
  frontier = frontier + OBJPTR_SIZE;

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

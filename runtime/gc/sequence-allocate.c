/* Copyright (C) 2016 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/******************************/
/* Static Function Prototypes */
/******************************/

/**
 * Initializes a sequence at 'frontier'
 *
 * @param s The GC_state to use
 * @param frontier The start of the sequence
 * @param sequenceSize The size of the sequence in bytes
 * @param numElements Number of elements in the sequence
 * @param header The sequence header
 * @param bytesNonObjptrs Number of non-objptr bytes per element
 * @param numObjptrs Number of objptrs per element
 *
 * @return The pointer to the start of the sequence object, after the headers
 */
static inline pointer sequenceInitialize(ARG_USED_FOR_ASSERT GC_state s,
                                         pointer frontier,
                                         size_t sequenceSize,
                                         GC_sequenceLength numElements,
                                         GC_header header,
                                         uint16_t bytesNonObjptrs,
                                         uint16_t numObjptrs);

/************************/
/* Function Definitions */
/************************/

pointer GC_sequenceAllocate (GC_state s,
                             size_t ensureBytesFree,
                             GC_sequenceLength numElements,
                             GC_header header) {
  size_t sequenceSize, sequenceSizeAligned;
  size_t bytesPerElement;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
  pointer frontier;
  pointer result;
  bool holdLock = FALSE;

  splitHeader(s, header, NULL, NULL, &bytesNonObjptrs, &numObjptrs);

  LOG(LM_ALLOCATION, LL_INFO,
      "GC_sequenceAllocate (%"PRIuMAX", "FMTSEQLEN", "FMTHDR") [%d]",
      (uintmax_t)ensureBytesFree,
      numElements, header,
      Proc_processorNumber (s));

  Trace3(EVENT_ARRAY_ALLOCATE_ENTER,
         (EventInt)ensureBytesFree,
         (EventInt)numElements,
         (EventInt)header);

  /* Check for overflow when computing sequenceSize.
   */
  bytesPerElement = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
  if (bytesPerElement > 0 and numElements > (SIZE_MAX / bytesPerElement)) {
    goto doOverflow;
  }

  sequenceSize = bytesPerElement * numElements;
  if (sequenceSize > SIZE_MAX - GC_SEQUENCE_METADATA_SIZE) {
    goto doOverflow;
  }

  sequenceSize += GC_SEQUENCE_METADATA_SIZE;
  sequenceSizeAligned = align (sequenceSize, s->alignment);
  if (sequenceSizeAligned < sequenceSize) {
    goto doOverflow;
  }

  LOG(LM_ALLOCATION, LL_DEBUG,
      "Array with "FMTSEQLEN" elts of size %"PRIuMAX" and total size %s and "
      "total aligned size %s. Ensure %s bytes free.",
      numElements,
      (uintmax_t)bytesPerElement,
      uintmaxToCommaString(sequenceSize),
      uintmaxToCommaString(sequenceSizeAligned),
      uintmaxToCommaString(ensureBytesFree));

  if (HM_inGlobalHeap(s)) {
    /* Allocate in Global Heap */

    /* Determine whether we will perform this allocation locally or not */
    holdLock = sequenceSizeAligned >= s->controls->oldGenSequenceSize;

    if (holdLock) {
      /* Global alloc */
      s->syncReason = SYNC_OLD_GEN_ARRAY;
      ENTER0 (s);
      if (not hasHeapBytesFree (s, sequenceSizeAligned, ensureBytesFree)) {
        performGC (s, sequenceSizeAligned, ensureBytesFree, FALSE, TRUE);
      }
      assert (hasHeapBytesFree (s, sequenceSizeAligned, ensureBytesFree));
      frontier = s->heap->start + s->heap->oldGenSize;
      assert (isFrontierAligned (s, frontier));
      result = sequenceInitialize(s,
                                  frontier,
                                  sequenceSize,
                                  numElements,
                                  header,
                                  bytesNonObjptrs,
                                  numObjptrs);

      /* SPOONHOWER_NOTE: This must be updated while holding the lock! */
      s->heap->oldGenSize += sequenceSizeAligned;
      assert (s->heap->start + s->heap->oldGenSize <= s->heap->nursery);
      s->cumulativeStatistics->bytesAllocated += sequenceSizeAligned;
      /* NB LEAVE appears below since no heap invariant holds while the
         oldGenSize has been updated but the sequence remains uninitialized. */
    } else {
      /* Local alloc */
      size_t bytesRequested;
      pointer newFrontier;

      bytesRequested = sequenceSizeAligned + ensureBytesFree;
      if (not hasHeapBytesFree (s, 0, bytesRequested)) {
        /* Local alloc may still require getting the lock, but we will release
           it before initialization. */
        ensureHasHeapBytesFreeAndOrInvariantForMutator (s, FALSE, FALSE, FALSE,
                                                        0, bytesRequested);
      }
      assert (hasHeapBytesFree (s, 0, bytesRequested));
      frontier = s->frontier;
      result = sequenceInitialize(s,
                                  frontier,
                                  sequenceSize,
                                  numElements,
                                  header,
                                  bytesNonObjptrs,
                                  numObjptrs);

      newFrontier = frontier + sequenceSizeAligned;
      assert (isFrontierAligned (s, newFrontier));
      s->frontier = newFrontier;
    }
  } else {
    /* Allocate in Hierarchical Heap */
    size_t bytesRequested = sequenceSizeAligned + ensureBytesFree;
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
    result = sequenceInitialize(s,
                                frontier,
                                sequenceSize,
                                numElements,
                                header,
                                bytesNonObjptrs,
                                numObjptrs);

      pointer newFrontier = frontier + sequenceSizeAligned;
      assert (isFrontierAligned (s, newFrontier));
      s->frontier = newFrontier;
  }

  GC_profileAllocInc (s, sequenceSizeAligned);

  LOG(LM_ALLOCATION, LL_DEBUG,
      "GC_sequenceAllocate done.  result = "FMTPTR"  frontier = "FMTPTR" [%d]",
      (uintptr_t)result,
      (uintptr_t)s->frontier,
      Proc_processorNumber (s));
  /* RAM_NOTE: Use LOG for displayGCState() */
  if (DEBUG_SEQUENCE) {
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
  DIE("Out of memory. Unable to allocate sequence with "FMTSEQLEN" elements and "
      "elements of size %"PRIuMAX" bytes.",
      numElements,
      (uintmax_t)bytesPerElement);
}

/*******************************/
/* Static Function Definitions */
/*******************************/

static pointer sequenceInitialize(ARG_USED_FOR_ASSERT GC_state s,
                                  pointer frontier,
                                  size_t sequenceSize,
                                  GC_sequenceLength numElements,
                                  GC_header header,
                                  uint16_t bytesNonObjptrs,
                                  uint16_t numObjptrs) {
  pointer last = frontier + sequenceSize;

  *((GC_sequenceCounter*)(frontier)) = 0;
  frontier = frontier + GC_SEQUENCE_COUNTER_SIZE;

  *((GC_sequenceLength*)(frontier)) = numElements;
  frontier = frontier + GC_SEQUENCE_LENGTH_SIZE;

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

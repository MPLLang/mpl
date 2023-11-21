/* Copyright (C) 2021 Sam Westrick.
 * Copyright (C) 2016 Matthew Fluet.
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


/** A small sequence is one which doesn't deserve its own chunk. */
pointer allocateSmallSequence(
  GC_state s,
  size_t sequenceSizeAligned,
  size_t ensureBytesFree)
{
  assert(sequenceSizeAligned < s->controls->blockSize / 2);

  /** Very important to do this first! It might trigger a GC. If we instead
    * did something like:
    *
    *   result = ...
    *   HM_ensureHierarchicalHeapAssurances(..., ensureBytesFree, ...)
    *
    * then the potential GC could invalidate `result` !!
    */
  getThreadCurrent(s)->bytesNeeded = sequenceSizeAligned;
  HM_ensureHierarchicalHeapAssurances(s, FALSE, sequenceSizeAligned, FALSE);
  assert((size_t)s->limitPlusSlop - (size_t)s->frontier >= sequenceSizeAligned);

  GC_thread thread = getThreadCurrent(s);
  pointer result = HM_HH_getFrontier(thread);
  HM_HH_updateValues(thread, result + sequenceSizeAligned);

  thread->bytesNeeded = ensureBytesFree;
  if (HM_getChunkSizePastFrontier(thread->currentChunk) < ensureBytesFree ||
      HM_getChunkFrontier(thread->currentChunk) >= (pointer)thread->currentChunk + HM_BLOCK_SIZE - GC_SEQUENCE_METADATA_SIZE)
  {
    if (!HM_HH_extend(s, thread, ensureBytesFree)) {
      DIE("Ran out of space!");
    }
  }

  return result;
}


pointer allocateLargeSequence(
  GC_state s,
  size_t sequenceSizeAligned,
  size_t ensureBytesFree)
{
  assert(sequenceSizeAligned >= s->controls->blockSize / 2);

  /** Very important to do this first! It might trigger a GC. If we instead
    * did something like:
    *
    *   result = ...
    *   HM_ensureHierarchicalHeapAssurances(..., ensureBytesFree, ...)
    *
    * then the potential GC could invalidate `result` !!
    */
  getThreadCurrent(s)->bytesNeeded = ensureBytesFree;
  HM_ensureHierarchicalHeapAssurances(s, FALSE, ensureBytesFree, TRUE);
  assert((size_t)s->limitPlusSlop - (size_t)s->frontier >= ensureBytesFree);

  GC_thread thread = getThreadCurrent(s);
  HM_chunk prevChunk = thread->currentChunk;

  if (!HM_HH_extend(s, thread, sequenceSizeAligned)) {
    DIE("Ran out of space!");
  }

  pointer result = HM_HH_getFrontier(thread);
  HM_chunk newChunk = thread->currentChunk;
  assert(HM_getChunkStart(newChunk) == result);
  HM_HH_updateValues(thread, result + sequenceSizeAligned);
  assert(newChunk->mightContainMultipleObjects);
  newChunk->mightContainMultipleObjects = FALSE;

  /** Now we need to set the frontier of the thread to a safe value.
    * (We can't leave as is, because this chunk we just allocated is only
    * supposed to contain a single object.)
    */

  thread->currentChunk = prevChunk;
  assert(HM_getChunkSizePastFrontier(thread->currentChunk) >= ensureBytesFree);
  assert(HM_HH_getDepth(HM_getLevelHead(thread->currentChunk)) == thread->currentDepth);

  return result;
}


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

  splitHeader(s, header, NULL, NULL, &bytesNonObjptrs, &numObjptrs);

  LOG(LM_ALLOCATION, LL_INFO,
      "GC_sequenceAllocate (%"PRIuMAX", "FMTSEQLEN", "FMTHDR") [%d]",
      (uintmax_t)ensureBytesFree,
      numElements, header,
      Proc_processorNumber (s));

  bytesPerElement = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);

  Trace3(EVENT_ARRAY_ALLOCATE_ENTER,
         (EventInt)ensureBytesFree,
         (EventInt)numElements,
         (EventInt)bytesPerElement);

  /* Check for overflow when computing sequenceSize.
   */
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

  enter(s);

  if (sequenceSizeAligned < s->controls->blockSize / 2)
    frontier = allocateSmallSequence(s, sequenceSizeAligned, ensureBytesFree);
  else
    frontier = allocateLargeSequence(s, sequenceSizeAligned, ensureBytesFree);

  result = sequenceInitialize(s,
                              frontier,
                              sequenceSize,
                              numElements,
                              header,
                              bytesNonObjptrs,
                              numObjptrs);

  GC_profileAllocInc (s, sequenceSizeAligned);

  s->frontier = HM_HH_getFrontier(getThreadCurrent(s));
  s->limitPlusSlop = HM_HH_getLimit(getThreadCurrent(s));
  s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;

  LOG(LM_ALLOCATION, LL_DEBUG,
      "GC_sequenceAllocate done.  result = "FMTPTR"  frontier = "FMTPTR" [%d]",
      (uintptr_t)result,
      (uintptr_t)s->frontier,
      Proc_processorNumber (s));
  /* RAM_NOTE: Use LOG for displayGCState() */
  if (DEBUG_SEQUENCE) {
    displayGCState (s, stderr);
  }

#if ASSERT
  if (s->frontier != s->limitPlusSlop) {
    assert(inSameBlock(s->frontier, s->limitPlusSlop-1));
    assert(((HM_chunk)blockOf(s->frontier))->magic == CHUNK_MAGIC);
  }
  assert(ensureBytesFree <= (size_t)(s->limitPlusSlop - s->frontier));

  assert(invariantForMutatorFrontier (s));
  assert(invariantForMutatorStack (s));
#endif

  leave(s);

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

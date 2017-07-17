/* Copyright (C) 2012,2016 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* newObject (s, header, bytesRequested, allocInOldGen)
 *
 * Allocate a new object in the heap.
 * bytesRequested includes the size of the header.
 */
/* SPOONHOWER_NOTE: must hold the runtime lock if allocInOldGen is true! */
pointer newObject (GC_state s,
                   GC_header header,
                   size_t bytesRequested,
                   bool allocInOldGen) {
  pointer frontier;
  pointer result;

  assert (isAligned (bytesRequested, s->alignment));
  assert (allocInOldGen
          ? hasHeapBytesFree (s, bytesRequested, 0)
          : hasHeapBytesFree (s, 0, bytesRequested));
  if (allocInOldGen) {
    /* NB you must have exclusive access to the runtime state
       if you are allocating in the older generation! */
    assert(HM_inGlobalHeap(s));
    frontier = s->heap->start + s->heap->oldGenSize;
    s->heap->oldGenSize += bytesRequested;
    s->cumulativeStatistics->bytesAllocated += bytesRequested;
  } else {
    if (DEBUG_DETAILED)
      fprintf (stderr, "frontier changed from "FMTPTR" to "FMTPTR"\n",
               (uintptr_t)s->frontier,
               (uintptr_t)(s->frontier + bytesRequested));
    frontier = s->frontier;
    s->frontier += bytesRequested;
  }
  /* SPOONHOWER_NOTE: unprotected concurrent access */
  GC_profileAllocInc (s, bytesRequested);
  *((GC_header*)(frontier)) = header;
  frontier = frontier + GC_HEADER_SIZE;
  *((objptr*)(frontier)) = BOGUS_OBJPTR;
  frontier = frontier + OBJPTR_SIZE;
  result = frontier;
  assert (isAligned ((size_t)result, s->alignment));
  if (DEBUG)
    fprintf (stderr, FMTPTR " = newObject ("FMTHDR", %"PRIuMAX", %s)\n",
             (uintptr_t)result,
             header,
             (uintmax_t)bytesRequested,
             boolToString (allocInOldGen));
  return result;
}

GC_stack newStack (GC_state s,
                   size_t reserved,
                   bool allocInOldGen) {
  GC_stack stack;

  assert (isStackReservedAligned (s, reserved));
  if (reserved > s->cumulativeStatistics->maxStackSize)
    s->cumulativeStatistics->maxStackSize = reserved;
  stack = (GC_stack)(newObject (s, GC_STACK_HEADER,
                                sizeofStackWithMetaData (s, reserved),
                                allocInOldGen));
  stack->reserved = reserved;
  stack->used = 0;
  if (DEBUG_STACKS)
    fprintf (stderr, FMTPTR " = newStack (%"PRIuMAX")\n",
             (uintptr_t)stack,
             (uintmax_t)reserved);

  return stack;
}

GC_thread newThread (GC_state s, size_t reserved) {
  GC_stack stack;
  GC_thread thread;
  pointer res;

  assert (isStackReservedAligned (s, reserved));
  if (HM_inGlobalHeap(s)) {
    ensureHasHeapBytesFreeAndOrInvariantForMutator (
        s, FALSE, FALSE, FALSE,
        0, sizeofStackWithMetaData (s, reserved) + sizeofThread (s));
  } else {
    HM_ensureHierarchicalHeapAssurances(s,
                                        FALSE,
                                        sizeofStackWithMetaData (s, reserved) +
                                        sizeofThread (s),
                                        FALSE);
  }
  stack = newStack (s, reserved, FALSE);
  res = newObject (s, GC_THREAD_HEADER,
                   sizeofThread (s),
                   FALSE);
  thread = (GC_thread)(res + offsetofThread (s));
  thread->inGlobalHeapCounter = 0;
  thread->useHierarchicalHeap = FALSE;
  thread->bytesNeeded = 0;
  thread->exnStack = BOGUS_EXN_STACK;
  thread->stack = pointerToObjptr((pointer)stack, s->heap->start);
  thread->hierarchicalHeap = BOGUS_OBJPTR;
  if (DEBUG_THREADS)
    fprintf (stderr, FMTPTR" = newThreadOfSize (%"PRIuMAX")\n",
             (uintptr_t)thread, (uintmax_t)reserved);;
  LOG(LM_ALLOCATION, LL_INFO,
      FMTPTR" = newThreadOfSize (%"PRIuMAX")",
      (uintptr_t)thread,
      (uintmax_t)reserved);

  return thread;
}

pointer HM_newHierarchicalHeap (GC_state s) {
  /* allocate the object */
  ensureHasHeapBytesFreeAndOrInvariantForMutator(s,
                                                 FALSE,
                                                 FALSE,
                                                 FALSE,
                                                 0,
                                                 HM_HH_sizeof(s));
  pointer hhObject = newObject (s,
                                GC_HIERARCHICAL_HEAP_HEADER,
                                HM_HH_sizeof(s),
                                FALSE);
  struct HM_HierarchicalHeap* hh =
      ((struct HM_HierarchicalHeap*)(hhObject +
                                     HM_HH_offsetof(s)));

  hh->lastAllocatedChunk = NULL;
  hh->lock = HM_HH_LOCK_INITIALIZER;
  hh->state = LIVE;
  hh->level = 0;
  hh->stealLevel = HM_HH_INVALID_LEVEL;
  hh->id = 0;
  hh->levelList = NULL;
  hh->newLevelList = NULL;
  hh->locallyCollectibleSize = 0;
  hh->locallyCollectibleHeapSize = s->controls->hhConfig.initialLCHS;
  hh->retVal = NULL;
  hh->parentHH = BOGUS_OBJPTR;
  hh->nextChildHH = BOGUS_OBJPTR;
  hh->childHHList = BOGUS_OBJPTR;
  hh->thread = BOGUS_OBJPTR;

  if (DEBUG_HEAP_MANAGEMENT) {
    fprintf (stderr, "%p = newHierarchicalHeap ()\n", ((void*)(hh)));
  }

  return hhObject;
}

static inline void setFrontier (GC_state s, pointer p,
                                ARG_USED_FOR_ASSERT size_t bytes) {
  p = alignFrontier (s, p);
  assert ((size_t)(p - s->frontier) <= bytes);
  GC_profileAllocInc (s, (size_t)(p - s->frontier));
  /* SPOONHOWER_NOTE: unsafe concurrent access */
  s->cumulativeStatistics->bytesAllocated += (size_t)(p - s->frontier);
  s->frontier = p;
  assert (s->frontier <= s->limitPlusSlop);
  assert (s->start <= s->frontier);
}

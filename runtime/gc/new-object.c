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

  /* SAM_NOTE: old gen has been removed entirely now.
   * TODO: remove this argument */
  assert(!allocInOldGen);

  assert(isAligned(bytesRequested, s->alignment));
  // assert (allocInOldGen
  //         ? hasHeapBytesFree (s, bytesRequested, 0)
  //         : hasHeapBytesFree (s, 0, bytesRequested));
  assert(s->limitPlusSlop - s->frontier >= bytesRequested);

  frontier = s->frontier;

  /* If the allocation crosses a block boundary, then we have to allocate a new
   * chunk to preserve the chunk/block invariant that the front of every object
   * begins in the first block of a chunk. */
  /* SAM_NOTE: this could be inefficient if the next allocation happens to be
   * a large object. */
  /* SAM_NOTE: TODO: unify these two branches (they do almost exactly the
   * same thing) */
  if (HM_inGlobalHeap(s)) {
    HM_chunk current = HM_getChunkOf(frontier);
    assert(current == HM_getChunkListLastChunk(s->globalHeap));
    s->frontier += bytesRequested; // this has to come after HM_inGlobalHeap
    if (!inFirstBlockOfChunk(current, s->frontier)) {
      HM_updateChunkValues(current, s->frontier);
      // requesting `GC_HEAP_LIMIT_SLOP` is arbitrary; we just need a new chunk.
      HM_chunk newChunk = HM_allocateChunk(s->globalHeap, GC_HEAP_LIMIT_SLOP);
      s->frontier = HM_getChunkFrontier(newChunk);
      s->limitPlusSlop = HM_getChunkLimit(newChunk);
      s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
    }
  } else {
    s->frontier += bytesRequested;
    if (!inSameBlock(frontier, s->frontier)) {
      /* force a new chunk to be created so that no new objects lie after this
       * array, which crossed a block boundary. */
      struct HM_HierarchicalHeap* hh = HM_HH_getCurrent(s);
      HM_HH_updateValues(hh, s->frontier);
      // requesting `GC_HEAP_LIMIT_SLOP` is arbitrary; we just need a new chunk.
      HM_HH_extend(hh, GC_HEAP_LIMIT_SLOP);
      s->frontier = HM_HH_getFrontier(hh);
      s->limitPlusSlop = HM_HH_getLimit(hh);
      s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
    }
  }

  /* SPOONHOWER_NOTE: unprotected concurrent access */
  GC_profileAllocInc (s, bytesRequested);
  *((GC_header*)(frontier)) = header;
  frontier = frontier + GC_HEADER_SIZE;
  // *((objptr*)(frontier)) = BOGUS_OBJPTR;
  // frontier = frontier + OBJPTR_SIZE;
  result = frontier;
  assert (isAligned ((size_t)result, s->alignment));
  assert (!hasFwdPtr(result));
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

  if (!isPointerInGlobalHeap(s, (pointer)stack))
    LOG(LM_ALLOCATION, LL_INFO,
      "Allocated stack in HH at "FMTPTR"",
      (uintptr_t)stack);

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
    ensureBytesFreeInGlobal(s, sizeofStackWithMetaData(s, reserved) + sizeofThread(s));
    // ensureHasHeapBytesFreeAndOrInvariantForMutator (
    //     s, FALSE, FALSE, FALSE,
    //     0, sizeofStackWithMetaData (s, reserved) + sizeofThread (s));
  } else {
    HM_ensureHierarchicalHeapAssurances(s,
                                        FALSE,
                                        sizeofStackWithMetaData (s, reserved) +
                                        sizeofThread (s),
                                        FALSE);
    assert((pointer)HM_getChunkOf(s->frontier) == blockOf(s->frontier));
  }
  stack = newStack (s, reserved, FALSE);
  assert(isPointerInGlobalHeap(s, s->frontier) || (pointer)HM_getChunkOf(s->frontier) == blockOf(s->frontier));
  /* SAM_NOTE: this is broken? if the stack is just the right size to force the
   * thread object to lie beyond the limit... */
  res = newObject (s, GC_THREAD_HEADER,
                   sizeofThread (s),
                   FALSE);
  thread = (GC_thread)(res + offsetofThread (s));
  thread->inGlobalHeapCounter = 0;
  thread->useHierarchicalHeap = FALSE;
  // thread->currentProcNum = -1; // TODO: define a constant INVALID_PROC_NUM... where should it go?
  thread->bytesNeeded = 0;
  thread->exnStack = BOGUS_EXN_STACK;
  thread->stack = pointerToObjptr((pointer)stack, NULL);
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
  assert(HM_inGlobalHeap(s));
  ensureBytesFreeInGlobal(s, HM_HH_sizeof(s));
  // ensureHasHeapBytesFreeAndOrInvariantForMutator(s,
  //                                                FALSE,
  //                                                FALSE,
  //                                                FALSE,
  //                                                0,
  //                                                HM_HH_sizeof(s));
  pointer hhObject = newObject (s,
                                GC_HIERARCHICAL_HEAP_HEADER,
                                HM_HH_sizeof(s),
                                FALSE);
  struct HM_HierarchicalHeap* hh =
      ((struct HM_HierarchicalHeap*)(hhObject +
                                     HM_HH_offsetof(s)));

  for (int i = 0; i < HM_MAX_NUM_LEVELS; i++) {
    HM_HH_LEVEL(hh, i) = NULL;
  }
  for (int i = 0; i < HM_MAX_NUM_LEVELS; i++) {
    HM_HH_LEVEL_CAPACITY(hh, i) = 0;
  }
  // hh->freeList = HM_newChunkList(hh, CHUNK_INVALID_LEVEL);
  hh->lastAllocatedChunk = NULL;
  hh->ignoreThis = 15210;
  // rwlock_init(&hh->lock);
  hh->state = LIVE;
  hh->level = 1; // level 0 is reserved for global heap
  hh->stealLevel = HM_HH_INVALID_LEVEL;
  hh->id = 0;
  // hh->levelList = NULL;
  // hh->newLevelList = NULL;
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

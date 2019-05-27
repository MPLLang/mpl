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
  assert(!HM_inGlobalHeap(s));
  s->frontier += bytesRequested;
  if (!inFirstBlockOfChunk(HM_getChunkOf(frontier), s->frontier)) {
    /* force a new chunk to be created so that no new objects lie after this
     * array, which crossed a block boundary. */
    struct HM_HierarchicalHeap* hh = HM_HH_getCurrent(s);
    assert(HM_getChunkOf(frontier) == hh->lastAllocatedChunk);
    HM_HH_updateValues(hh, s->frontier);
    // requesting `GC_HEAP_LIMIT_SLOP` is arbitrary; we just need a new chunk.
    HM_HH_extend(hh, GC_HEAP_LIMIT_SLOP);
    s->frontier = HM_HH_getFrontier(hh);
    s->limitPlusSlop = HM_HH_getLimit(hh);
    s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
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
  assert(!HM_inGlobalHeap(s));

  HM_ensureHierarchicalHeapAssurances(s,
                                      FALSE,
                                      sizeofStackWithMetaData (s, reserved) +
                                      sizeofThread (s),
                                      FALSE);
  assert((pointer)HM_getChunkOf(s->frontier) == blockOf(s->frontier));

  stack = newStack (s, reserved, FALSE);
  assert(isPointerInGlobalHeap(s, s->frontier) || (pointer)HM_getChunkOf(s->frontier) == blockOf(s->frontier));
  /* SAM_NOTE: this is broken? if the stack is just the right size to force the
   * thread object to lie beyond the limit... */
  res = newObject (s, GC_THREAD_HEADER,
                   sizeofThread (s),
                   FALSE);
  thread = (GC_thread)(res + offsetofThread (s));
  thread->inGlobalHeapCounter = 0;
  /* a fresh thread is not currently being executed by any processor */
  thread->currentProcNum = -1;
  thread->bytesNeeded = 0;
  thread->exnStack = BOGUS_EXN_STACK;
  thread->stack = pointerToObjptr((pointer)stack, NULL);
  thread->hierarchicalHeap = NULL;
  if (DEBUG_THREADS)
    fprintf (stderr, FMTPTR" = newThreadOfSize (%"PRIuMAX")\n",
             (uintptr_t)thread, (uintmax_t)reserved);;
  LOG(LM_ALLOCATION, LL_INFO,
      FMTPTR" = newThreadOfSize (%"PRIuMAX")",
      (uintptr_t)thread,
      (uintmax_t)reserved);

  return thread;
}

GC_thread newThreadWithHeap(GC_state s, size_t reserved, uint32_t level) {
  size_t stackSize = sizeofStackWithMetaData(s, reserved);
  size_t threadSize = sizeofThread(s);
  size_t totalSize = stackSize + threadSize;

  struct HM_HierarchicalHeap *hh = HM_HH_new(s);
  hh->level = level;
  assert(HM_HH_getLevel(s, hh) == level);
  HM_HH_extend(hh, totalSize);

  pointer frontier = HM_HH_getFrontier(hh);

  assert(HM_HH_getLimit(hh) - frontier >= totalSize);
  assert(inFirstBlockOfChunk(HM_getChunkOf(frontier), frontier+totalSize));

  *((GC_header*)(frontier)) = GC_THREAD_HEADER;
  GC_thread thread = (GC_thread)(frontier + GC_HEADER_SIZE + offsetofThread(s));

  *((GC_header*)(frontier + threadSize)) = GC_STACK_HEADER;
  GC_stack stack = (GC_stack)(frontier + threadSize + GC_HEADER_SIZE);

  stack->reserved = reserved;
  stack->used = 0;

  thread->inGlobalHeapCounter = 0;
  thread->currentProcNum = -1;
  thread->bytesNeeded = 0;
  thread->exnStack = BOGUS_EXN_STACK;
  thread->stack = pointerToObjptr((pointer)stack, NULL);
  thread->hierarchicalHeap = hh;

  HM_HH_updateValues(hh, frontier + totalSize);

  return thread;
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

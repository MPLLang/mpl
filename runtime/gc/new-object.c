/* Copyright (C) 2012,2016 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/* newObject (s, header, bytesRequested)
 *
 * Allocate a new object in the heap.
 * bytesRequested includes the size of the header.
 */
pointer newObject(GC_state s,
                  GC_header header,
                  size_t bytesRequested) {
  pointer frontier;
  pointer result;

  assert(isAligned(bytesRequested, s->alignment));
  assert((size_t)(s->limitPlusSlop - s->frontier) >= bytesRequested);

  frontier = s->frontier;

  /* If the allocation crosses a block boundary, then we have to allocate a new
   * chunk to preserve the chunk/block invariant that the front of every object
   * begins in the first block of a chunk. */
  /* SAM_NOTE: this could be inefficient if the next allocation happens to be
   * a large object. */
  assert(threadAndHeapOkay(s));
  s->frontier += bytesRequested;
  if (!inFirstBlockOfChunk(HM_getChunkOf(frontier), s->frontier)) {
    /* force a new chunk to be created so that no new objects lie after this
     * array, which crossed a block boundary. */
    GC_thread thread = getThreadCurrent(s);
    assert(HM_getChunkOf(frontier) == thread->currentChunk);
    HM_HH_updateValues(thread, s->frontier);
    // requesting `GC_HEAP_LIMIT_SLOP` is arbitrary; we just need a new chunk.
    HM_HH_extend(s, thread, GC_HEAP_LIMIT_SLOP);
    s->frontier = HM_HH_getFrontier(thread);
    s->limitPlusSlop = HM_HH_getLimit(thread);
    s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
  }

  /* SPOONHOWER_NOTE: unprotected concurrent access */
  GC_profileAllocInc (s, bytesRequested);
  *((GC_header*)(frontier)) = header;
  frontier = frontier + GC_HEADER_SIZE;
  // *((objptr*)(frontier)) = BOGUS_OBJPTR;
  // frontier = frontier + OBJPTR_SIZE;
  result = frontier;
  assert(isAligned((size_t)result, s->alignment));
  assert(!hasFwdPtr(result));
  if (DEBUG)
    fprintf(stderr, FMTPTR " = newObject ("FMTHDR", %"PRIuMAX")\n",
            (uintptr_t)result,
            header,
            (uintmax_t)bytesRequested);
  return result;
}

GC_stack newStack(GC_state s, size_t reserved) {
  GC_stack stack;

  assert (isStackReservedAligned (s, reserved));
  if (reserved > s->cumulativeStatistics->maxStackSize)
    s->cumulativeStatistics->maxStackSize = reserved;
  stack = (GC_stack)(newObject(s, GC_STACK_HEADER,
                               sizeofStackWithMetaData(s, reserved)));

  stack->reserved = reserved;
  stack->used = 0;
  if (DEBUG_STACKS)
    fprintf (stderr, FMTPTR " = newStack (%"PRIuMAX")\n",
             (uintptr_t)stack,
             (uintmax_t)reserved);

  return stack;
}

GC_thread newThread(GC_state s, size_t reserved) {
  GC_stack stack;
  GC_thread thread;
  pointer res;

  assert(isStackReservedAligned(s, reserved));
  assert(threadAndHeapOkay(s));

  HM_ensureHierarchicalHeapAssurances(s,
                                      FALSE,
                                      sizeofStackWithMetaData(s, reserved) +
                                      sizeofThread(s),
                                      FALSE);
  assert((pointer)HM_getChunkOf(s->frontier) == blockOf(s->frontier));

  stack = newStack(s, reserved);
  assert((pointer)HM_getChunkOf(s->frontier) == blockOf(s->frontier));
  /* SAM_NOTE: this is broken? if the stack is just the right size to force the
   * thread object to lie beyond the limit... */
  res = newObject(s, GC_THREAD_HEADER, sizeofThread(s));
  thread = (GC_thread)(res + offsetofThread(s));
  /* a fresh thread is not currently being executed by any processor */
  thread->spareHeartbeats = 0;
  thread->currentProcNum = -1;
  thread->bytesNeeded = 0;
  thread->exnStack = BOGUS_EXN_STACK;
  thread->currentDepth = HM_HH_INVALID_DEPTH;
  thread->minLocalCollectionDepth = s->controls->hhConfig.minLocalDepth;
  thread->bytesAllocatedSinceLastCollection = 0;
  thread->bytesSurvivedLastCollection = 0;
  thread->hierarchicalHeap = NULL;
  thread->currentChunk = NULL;
  thread->stack = pointerToObjptr((pointer)stack, NULL);
  thread->disentangledDepth = INT32_MAX;

#ifdef DETECT_ENTANGLEMENT
  thread->decheckState = DECHECK_BOGUS_TID;
  memset(&(thread->decheckSyncDepths[0]), 0, sizeof(uint32_t) * DECHECK_DEPTHS_LEN);
#endif

  if (DEBUG_THREADS)
    fprintf (stderr, FMTPTR" = newThreadOfSize (%"PRIuMAX")\n",
             (uintptr_t)thread, (uintmax_t)reserved);;
  LOG(LM_ALLOCATION, LL_INFO,
      FMTPTR" = newThreadOfSize (%"PRIuMAX")",
      (uintptr_t)thread,
      (uintmax_t)reserved);

  return thread;
}

GC_thread newThreadWithHeap(
  GC_state s,
  size_t reserved,
  uint32_t depth,
  ARG_USED_FOR_DETECT_ENTANGLEMENT bool existsCurrentThread)
{
  size_t stackSize = sizeofStackWithMetaData(s, reserved);
  size_t threadSize = sizeofThread(s);
  size_t totalSize = stackSize + threadSize;

  if (reserved > s->cumulativeStatistics->maxStackSize)
    s->cumulativeStatistics->maxStackSize = reserved;

  /* Allocate and initialize the heap that will be assigned to this thread.
   * Can't just use HM_HH_extend, because the corresponding thread doesn't exist
   * yet. */
  HM_HierarchicalHeap hh = HM_HH_new(s, depth);

  HM_chunk tChunk = HM_allocateChunk(HM_HH_getChunkList(hh), threadSize);
  HM_chunk sChunk = HM_allocateChunk(HM_HH_getChunkList(hh), stackSize);
  if (NULL == sChunk || NULL == tChunk) {
    DIE("Ran out of space for thread+stack allocation!");
  }
  tChunk->levelHead = HM_HH_getUFNode(hh);
  sChunk->levelHead = HM_HH_getUFNode(hh);
  sChunk->mightContainMultipleObjects = FALSE;

#ifdef DETECT_ENTANGLEMENT
  decheck_tid_t decheckState =
    (existsCurrentThread ?
    getThreadCurrent(s)->decheckState : DECHECK_BOGUS_TID);
#else
  decheck_tid_t decheckState = DECHECK_BOGUS_TID;
#endif

  tChunk->decheckState = decheckState;
  sChunk->decheckState = decheckState;

  assert(threadSize < HM_getChunkSizePastFrontier(tChunk));
  assert(stackSize < HM_getChunkSizePastFrontier(sChunk));

  pointer tFrontier = HM_getChunkFrontier(tChunk);
  pointer sFrontier = HM_getChunkFrontier(sChunk);

  /* Next, allocate+init the thread within the heap that we just created. */
  *((GC_header*)tFrontier) = GC_THREAD_HEADER;
  GC_thread thread = (GC_thread)(tFrontier + GC_HEADER_SIZE + offsetofThread(s));

  *((GC_header*)sFrontier) = GC_STACK_HEADER;
  GC_stack stack = (GC_stack)(sFrontier + GC_HEADER_SIZE);

  stack->reserved = reserved;
  stack->used = 0;

  thread->spareHeartbeats = 0;
  thread->currentProcNum = -1;
  thread->bytesNeeded = 0;
  thread->exnStack = BOGUS_EXN_STACK;
  thread->currentDepth = depth;
  thread->minLocalCollectionDepth = s->controls->hhConfig.minLocalDepth;
  thread->bytesAllocatedSinceLastCollection = totalSize;
  thread->bytesSurvivedLastCollection = 0;
  thread->hierarchicalHeap = hh;
  thread->currentChunk = tChunk;
  thread->stack = pointerToObjptr((pointer)stack, NULL);
  thread->disentangledDepth = INT32_MAX;

#ifdef DETECT_ENTANGLEMENT
  thread->decheckState = DECHECK_BOGUS_TID;
  memset(&(thread->decheckSyncDepths[0]), 0, sizeof(uint32_t) * DECHECK_DEPTHS_LEN);
#endif

  HM_updateChunkFrontierInList(
    HM_HH_getChunkList(hh),
    tChunk,
    tFrontier + threadSize);

  HM_updateChunkFrontierInList(
    HM_HH_getChunkList(hh),
    sChunk,
    sFrontier + stackSize);

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
}

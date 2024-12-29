/* Copyright (C) 2020 Sam Westrick.
 * Copyright (C) 2009-2010,2012,2016 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#include "hierarchical-heap.h"

// extern int64_t CheckActivationStack(void);

void growStackCurrent(GC_state s) {
  size_t newReserved;
  size_t stackSize;
  GC_stack stack;

  newReserved = sizeofStackGrowReserved(s, getStackCurrent(s));
  size_t newPromoStackReserved = desiredPromoStackReserved(s, newReserved);
  assert(isStackReservedAligned(s, newReserved));
  stackSize = sizeofStackWithMetaData(s, newReserved, newPromoStackReserved);
  if (DEBUG_STACKS or s->controls->messages)
    fprintf (stderr,
             "[GC: Growing stack of size %s bytes to size %s bytes, using %s bytes.]\n",
             uintmaxToCommaString(getStackCurrent(s)->reserved),
             uintmaxToCommaString(newReserved),
             uintmaxToCommaString(getStackCurrent(s)->used));
  if (newReserved > s->cumulativeStatistics->maxStackSize)
    s->cumulativeStatistics->maxStackSize = newReserved;

  HM_chunk chunk = HM_getChunkOf((pointer)getStackCurrent(s));
  HM_HierarchicalHeap hh = HM_getLevelHeadPathCompress(chunk);

  if (chunk->mightContainMultipleObjects) {
    DIE("Tried to grow a stack without its own chunk.");
  }

  assert(HM_getChunkFrontier(chunk) == HM_getChunkStart(chunk) +
    sizeofStackWithMetaData(
      s,
      getStackCurrent(s)->reserved,
      getStackCurrent(s)->promoStackReserved));

  /* the fast case: plenty of space in the stack's chunk to just grow the
   * stack in place. */
  if (stackSize <= (size_t)(HM_getChunkLimit(chunk) - HM_getChunkStart(chunk))) {
    GC_stack this = getStackCurrent(s);
    assert(newReserved >= this->reserved);

    size_t promoStackUsed = this->promoStackTop - this->promoStackBot;
    pointer newPromoStackBot = this->promoStackBot + (newReserved - this->reserved);
    pointer newPromoStackTop = newPromoStackBot + promoStackUsed;
    GC_memmove(
      this->promoStackBot,
      newPromoStackBot,
      promoStackUsed);

    this->reserved = newReserved;
    this->promoStackBot = newPromoStackBot;
    this->promoStackTop = newPromoStackTop;
    this->promoStackReserved = newPromoStackReserved;

    HM_updateChunkFrontierInList(
      HM_HH_getChunkList(hh),
      chunk,
      HM_getChunkStart(chunk) + stackSize);
    return;
  }

  HM_HierarchicalHeap newhh =
    HM_HH_getHeapAtDepth(s, getThreadCurrent(s), HM_HH_getDepth(hh));

  /* in this case, the new stack needs more space, so allocate a new chunk,
   * copy the stack, and throw away the old chunk. */
  HM_chunk newChunk = HM_allocateChunkWithPurpose(
    HM_HH_getChunkList(newhh),
    stackSize,
    BLOCK_FOR_HEAP_CHUNK);
    
  if (NULL == newChunk) {
    DIE("Ran out of space to grow stack!");
  }
  assert(stackSize < HM_getChunkSizePastFrontier(newChunk));
  newChunk->mightContainMultipleObjects = FALSE;
  newChunk->levelHead = HM_HH_getUFNode(newhh);
  newChunk->decheckState = chunk->decheckState;

  pointer frontier = HM_getChunkFrontier(newChunk);
  assert(frontier == HM_getChunkStart(newChunk));
  assert(GC_STACK_METADATA_SIZE == GC_HEADER_SIZE);
  *((GC_header*)frontier) = GC_STACK_HEADER;
  stack = (GC_stack)(frontier + GC_HEADER_SIZE);
  stack->reserved = newReserved;
  stack->promoStackReserved = newPromoStackReserved;
  stack->used = 0;
  HM_updateChunkFrontierInList(
    HM_HH_getChunkList(newhh),
    newChunk,
    frontier + stackSize);

  copyStack(s, getStackCurrent(s), stack);
  getThreadCurrent(s)->stack = pointerToObjptr((pointer)stack, NULL);

  assert(getThreadCurrent(s)->currentChunk != chunk);
  // HM_unlinkChunk(HM_HH_getChunkList(hh), chunk);
  // HM_freeChunk(s, chunk);
}

void GC_collect (GC_state s, size_t bytesRequested, bool force) {
  enter(s);
  maybeSample(s, s->blockUsageSampler);

  // HM_HierarchicalHeap h = getThreadCurrent(s)->hierarchicalHeap;
  // while (h->nextAncestor != NULL) h = h->nextAncestor;
  // if (HM_HH_getDepth(h) == 0 && HM_getChunkListSize(HM_HH_getChunkList(h)) > 8192) {
  //   size_t gsize = HM_getChunkListSize(HM_HH_getChunkList(h));
  //   size_t gusize = HM_getChunkListUsedSize(HM_HH_getChunkList(h));
  //   printf("[BIG GLOBAL %d] size: %zu, used: %zu (%.01lf%%)\n",
  //     s->procNumber,
  //     gsize,
  //     gusize,
  //     100.0 * ((double)gusize / (double)gsize));
  // }

  /* adjust bytesRequested */
  /*
   * When the mutator requests zero bytes, it may actually need as
   * much as GC_HEAP_LIMIT_SLOP.
   */
  bytesRequested += GC_HEAP_LIMIT_SLOP;

  assert(bytesRequested + sizeof(struct HM_chunk) <= s->controls->blockSize);

  getThreadCurrent(s)->bytesNeeded = bytesRequested;
  switchToSignalHandlerThreadIfNonAtomicAndSignalPending(s);

  /* SAM_NOTE: don't use HM_HH_getFrontier here, because invariant is possibly
   * violated (might have frontier == limitPlusSlop)
   */
  s->frontier = HM_getChunkFrontier(getThreadCurrent(s)->currentChunk);
  s->limitPlusSlop = HM_getChunkLimit(getThreadCurrent(s)->currentChunk);
  s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;

  /* SAM_NOTE: shouldn't this be
   *   getThreadCurrent(s)->bytesNeeded
   * instead of bytesRequested? */
  HM_ensureHierarchicalHeapAssurances(
    s,
    force,
    getThreadCurrent(s)->bytesNeeded,
    FALSE);

  leave(s);
}

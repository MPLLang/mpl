/* Copyright (C) 2018-2020 Sam Westrick
 * Copyright (C) 2014,2015 Ram Raghunathan.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file hierarchical-heap.c
 *
 * @author Ram Raghunathan
 *
 * This file implements the utility functions for the HierarchicalHeap object
 * described in hierarchical-heap.h.
 */

#include "hierarchical-heap.h"

/******************************/
/* Static Function Prototypes */
/******************************/

static void assertInvariants(GC_thread thread);

/************************/
/* Function Definitions */
/************************/
#if (defined (MLTON_GC_INTERNAL_FUNCS))

HM_HierarchicalHeap HM_HH_zip(HM_HierarchicalHeap hh1, HM_HierarchicalHeap hh2)
{

#if ASSERT
  assert(NULL != hh1 || NULL != hh2);
  /* do some setup to remember what the state of the world was before the zip.
   * then after the zip, check what we can to make sure it worked correctly. */
  uint32_t maxd1 = (NULL == hh1 ? 0 : HM_HH_getDepth(hh1));
  uint32_t maxd2 = (NULL == hh2 ? 0 : HM_HH_getDepth(hh2));
  uint32_t maxd = max(maxd1, maxd2);
  HM_HierarchicalHeap heaps1[maxd+1];
  HM_HierarchicalHeap heaps2[maxd+1];
  for (uint32_t i = 0; i <= maxd; i++) { heaps1[i] = NULL; heaps2[i] = NULL; }
  for (HM_HierarchicalHeap h = hh1; NULL != h; h = h->nextAncestor)
    heaps1[HM_HH_getDepth(h)] = h;
  for (HM_HierarchicalHeap h = hh2; NULL != h; h = h->nextAncestor)
    heaps2[HM_HH_getDepth(h)] = h;
#endif

  HM_HierarchicalHeap result = NULL;

  HM_HierarchicalHeap *cursor = &result;
  while (NULL != hh1 && NULL != hh2)
  {
    uint32_t depth1 = HM_HH_getDepth(hh1);
    uint32_t depth2 = HM_HH_getDepth(hh2);

    if (depth1 == depth2)
    {
      HM_appendChunkList(HM_HH_getChunkList(hh1), HM_HH_getChunkList(hh2));
      HM_appendChunkList(HM_HH_getRemSet(hh1), HM_HH_getRemSet(hh2));

      hh2->representative = hh1;

      *cursor = hh1;
      cursor = &(hh1->nextAncestor);

      hh1 = hh1->nextAncestor;
      hh2 = hh2->nextAncestor;
    }
    else if (depth1 > depth2)
    {
      *cursor = hh1;
      cursor = &(hh1->nextAncestor);

      hh1 = hh1->nextAncestor;
    }
    else /* depth1 < depth2 */
    {
      *cursor = hh2;
      cursor = &(hh2->nextAncestor);

      hh2 = hh2->nextAncestor;
    }
  }
  if (NULL != hh1) *cursor = hh1;
  if (NULL != hh2) *cursor = hh2;

#if ASSERT
  assert(NULL != result);
  assert(HM_HH_getDepth(result) == maxd);
  HM_HierarchicalHeap heapsResult[maxd+1];
  for (uint32_t i = 0; i <= maxd; i++) { heapsResult[i] = NULL; }
  for (HM_HierarchicalHeap h = result; NULL != h; h = h->nextAncestor)
    heapsResult[HM_HH_getDepth(h)] = h;

  HM_HierarchicalHeap prev = NULL;
  for (uint32_t i = 0; i <= maxd; i++)
  {
    /* check that the result is a proper linked list, sorted by depth */
    if (NULL != heapsResult[i])
    {
      assert(heapsResult[i]->nextAncestor == prev);
      prev = heapsResult[i];
    }

    /* check that the result contains exactly the heaps of the two inputs */
    if (NULL != heaps1[i] && NULL != heaps2[i])
    {
      assert(heapsResult[i] == heaps1[i]);
      assert(HM_HH_getChunkList(heapsResult[i]) == HM_HH_getChunkList(heaps1[i]));
      assert(heaps2[i]->representative == heaps1[i]);
    }
    else if (NULL != heaps1[i])
    {
      assert(heapsResult[i] == heaps1[i]);
      assert(HM_HH_getChunkList(heapsResult[i]) == HM_HH_getChunkList(heaps1[i]));
    }
    else if (NULL != heaps2[i])
    {
      assert(heapsResult[i] == heaps2[i]);
      assert(HM_HH_getChunkList(heapsResult[i]) == HM_HH_getChunkList(heaps2[i]));
    }
    else
    {
      assert(heapsResult[i] == NULL);
    }
  }
#endif

  return result;
}

void HM_HH_merge(
  __attribute__((unused)) GC_state s,
  GC_thread parentThread,
  GC_thread childThread)
{
  assert(parentThread->hierarchicalHeap != NULL);
  assert(childThread->hierarchicalHeap != NULL);

  HM_HierarchicalHeap parentHH = parentThread->hierarchicalHeap;
  HM_HierarchicalHeap childHH = childThread->hierarchicalHeap;

  assertInvariants(parentThread);
  assertInvariants(childThread);
  /* can only merge at join point! */
  assert(childThread->currentDepth == parentThread->currentDepth);
  assert(childThread->currentDepth >= 1);

  /* Merge levels. */
  parentThread->hierarchicalHeap = HM_HH_zip(parentHH, childHH);

  parentThread->bytesSurvivedLastCollection +=
    childThread->bytesSurvivedLastCollection;
  parentThread->bytesAllocatedSinceLastCollection +=
    childThread->bytesAllocatedSinceLastCollection;

  assertInvariants(parentThread);

  Trace2(EVENT_MERGED_HEAP, (EventInt)parentHH, (EventInt)childHH);

  // free(childHH);
}

void HM_HH_promoteChunks(
  __attribute__((unused)) GC_state s,
  GC_thread thread)
{
  HM_HierarchicalHeap hh = thread->hierarchicalHeap;

  if (HM_HH_getDepth(hh) < thread->currentDepth)
  {
    /* no need to do anything; this function only guarantees that the
     * current depth has been completely evacuated. */
    return;
  }

  uint32_t currentDepth = thread->currentDepth;
  assert(HM_HH_getDepth(hh) == currentDepth);

  if (NULL == hh->nextAncestor ||
      HM_HH_getDepth(hh->nextAncestor) < currentDepth-1)
  {
    /* There is no heap immediately above the leaf, so we can leave the current
     * structure intact and just decrement the recorded depth. */
    hh->depth--;
  }
  else
  {
    /* There is a heap immediately above the leaf, so merge into that heap. */
    assert(NULL != hh->nextAncestor);
    assert(HM_HH_getDepth(hh->nextAncestor) == currentDepth-1);
    HM_appendChunkList(HM_HH_getChunkList(hh->nextAncestor), HM_HH_getChunkList(hh));
    HM_appendChunkList(HM_HH_getRemSet(hh->nextAncestor), HM_HH_getRemSet(hh));

    hh->representative = hh->nextAncestor;

    /* ...and then shortcut. */
    thread->hierarchicalHeap = hh->nextAncestor;
  }

  assert(HM_HH_getDepth(thread->hierarchicalHeap) < thread->currentDepth);
  assertInvariants(thread);
}

bool HM_HH_isLevelHead(HM_HierarchicalHeap hh)
{
  return (NULL != hh) && (NULL == hh->representative);
}

HM_HierarchicalHeap HM_HH_new(
  GC_state s,
  uint32_t depth,
  decheck_tid_t decheckState)
{
  size_t bytesNeeded = sizeof(struct HM_HierarchicalHeap);
  HM_chunk chunk = HM_getFreeChunk(s, bytesNeeded);
  pointer start = HM_shiftChunkStart(chunk, bytesNeeded);

  HM_HierarchicalHeap hh = (HM_HierarchicalHeap)start;

  hh->representative = NULL;
  hh->depth = depth;
  hh->nextAncestor = NULL;

  HM_initChunkList(HM_HH_getChunkList(hh));
  HM_initChunkList(HM_HH_getRemSet(hh));

  HM_appendChunk(HM_HH_getChunkList(hh), chunk);
  chunk->levelHead = hh;
  chunk->decheckState = decheckState;

  return hh;
}

uint32_t HM_HH_getDepth(HM_HierarchicalHeap hh)
{
  return hh->depth;
}

HM_HierarchicalHeap HM_HH_getHeapAtDepth(
  GC_state s,
  GC_thread thread,
  uint32_t depth)
{
  assert(depth <= thread->currentDepth);

  /* walk up while (a) not end of list and (b) still deeper than desired */
  HM_HierarchicalHeap *cursor = &(thread->hierarchicalHeap);
  while (NULL != *cursor &&
         HM_HH_getDepth(*cursor) > depth)
  {
    cursor = &((*cursor)->nextAncestor);
  }

  /* check if found it and return */
  HM_HierarchicalHeap hh = *cursor;
  if (NULL != hh && HM_HH_getDepth(hh) == depth)
    return hh;

  /* otherwise, either missed it or at end of list. */
  assert(NULL == hh || HM_HH_getDepth(hh) < depth);

  /* SAM_NOTE: TODO: how to compute an appropriate thread id for this
   * missing ancestor?? */
  HM_HierarchicalHeap newhh = HM_HH_new(s, depth, DECHECK_BOGUS_TID);
  newhh->nextAncestor = hh;
  *cursor = newhh;
  return newhh;
}

void HM_HH_ensureNotEmpty(GC_state s, GC_thread thread) {
  if (NULL != thread->currentChunk) return;

#if ASSERT
  assert(NULL != thread->hierarchicalHeap);
  for (HM_HierarchicalHeap cursor = thread->hierarchicalHeap;
       NULL != cursor;
       cursor = cursor->nextAncestor)
  {
    assert(NULL != HM_HH_getChunkList(cursor));
    assert(NULL == HM_HH_getChunkList(cursor)->firstChunk);
  }
#endif

  /* add in one chunk */
  if (!HM_HH_extend(s, thread, GC_HEAP_LIMIT_SLOP)) {
    DIE("Ran out of space for Hierarchical Heap!");
  }
}

bool HM_HH_extend(GC_state s, GC_thread thread, size_t bytesRequested)
{
  HM_HierarchicalHeap hh = thread->hierarchicalHeap;
  uint32_t currentDepth = thread->currentDepth;

  assert(NULL != hh);
  assert(NULL != HM_HH_getChunkList(hh));
  assert(HM_HH_getDepth(hh) <= currentDepth);

  HM_chunk chunk;

  if (HM_HH_getDepth(hh) < currentDepth)
  {
    HM_HierarchicalHeap newhh = HM_HH_new(s, currentDepth, thread->decheckState);
    newhh->nextAncestor = hh;
    thread->hierarchicalHeap = newhh;

    hh = newhh;

    /* note that new heaps are initialized with one free chunk */
    chunk = HM_getChunkListFirstChunk(HM_HH_getChunkList(hh));
    if (((size_t)HM_getChunkLimit(chunk) - (size_t)HM_getChunkFrontier(chunk))
        >= bytesRequested)
    {
      thread->currentChunk = chunk;
      HM_HH_addRecentBytesAllocated(thread, HM_getChunkSize(chunk));
      return TRUE;
    }
    /* otherwise, we need to allocate a new chunk, so fall through to standard
     * logic below. */
  }

  chunk = HM_allocateChunk(HM_HH_getChunkList(hh), bytesRequested);

  if (NULL == chunk) {
    return FALSE;
  }

  chunk->decheckState = thread->decheckState;
  chunk->levelHead = hh;

  thread->currentChunk = chunk;
  HM_HH_addRecentBytesAllocated(thread, HM_getChunkSize(chunk));

  return TRUE;
}

HM_HierarchicalHeap HM_HH_getCurrent(GC_state s) {
  return getThreadCurrent(s)->hierarchicalHeap;
}

pointer HM_HH_getFrontier(GC_thread thread) {
  assert(blockOf(HM_getChunkFrontier(thread->currentChunk)) == (pointer)thread->currentChunk);
  return HM_getChunkFrontier(thread->currentChunk);
}

pointer HM_HH_getLimit(GC_thread thread) {
  return HM_getChunkLimit(thread->currentChunk);
}

void HM_HH_updateValues(GC_thread thread, pointer frontier) {
  HM_updateChunkValues(thread->currentChunk, frontier);
}

size_t HM_HH_nextCollectionThreshold(GC_state s, size_t survivingSize) {
  size_t threshold =
    (size_t)((double)survivingSize * s->controls->hhConfig.collectionThresholdRatio);
  if (threshold < s->controls->hhConfig.minCollectionSize) {
    threshold = s->controls->hhConfig.minCollectionSize;
  }
  return threshold;
}

size_t HM_HH_addRecentBytesAllocated(GC_thread thread, size_t bytes) {
  thread->bytesAllocatedSinceLastCollection += bytes;
  return thread->bytesAllocatedSinceLastCollection;
}

uint32_t HM_HH_desiredCollectionScope(GC_state s, GC_thread thread)
{
  struct HM_HierarchicalHeap* hh = thread->hierarchicalHeap;

  if (s->wsQueueTop == BOGUS_OBJPTR)
    return thread->currentDepth+1; /* don't collect */

  if (thread->bytesAllocatedSinceLastCollection <
      (s->controls->hhConfig.collectionThresholdRatio * thread->bytesSurvivedLastCollection))
  {
    return thread->currentDepth+1; /* don't collect */
  }

  uint64_t topval = *(uint64_t*)objptrToPointer(s->wsQueueTop, NULL);
  uint32_t potentialLocalScope = UNPACK_IDX(topval);

  size_t budget = 4 * thread->bytesAllocatedSinceLastCollection;

  if (budget < s->controls->hhConfig.minCollectionSize ||
      potentialLocalScope > thread->currentDepth ||
      potentialLocalScope > HM_HH_getDepth(hh) ||
      HM_getChunkListSize(HM_HH_getChunkList(hh)) > budget)
  {
    return thread->currentDepth+1; /* don't collect */
  }

  HM_HierarchicalHeap cursor = hh;
  size_t sz = HM_getChunkListSize(HM_HH_getChunkList(hh));
  while (NULL != cursor->nextAncestor &&
         HM_HH_getDepth(cursor->nextAncestor) >= potentialLocalScope &&
         HM_getChunkListSize(HM_HH_getChunkList(cursor->nextAncestor)) + sz < budget)
  {
    cursor = cursor->nextAncestor;
    sz += HM_getChunkListSize(HM_HH_getChunkList(cursor));
  }

#if ASSERT
  uint32_t minDepthOkayForBudget = HM_HH_getDepth(cursor);
#endif

  if (sz < s->controls->hhConfig.minCollectionSize)
    return thread->currentDepth+1; /* don't collect if too small */

  /* It's likely that the shallower levels are mostly empty, so let's see if
   * we can skip some of them without ignoring too much data. */
  size_t newBudget = 0.75 * sz;
  cursor = hh;
  sz = HM_getChunkListSize(HM_HH_getChunkList(hh));
  while (NULL != cursor->nextAncestor &&
         HM_getChunkListSize(HM_HH_getChunkList(cursor->nextAncestor)) + sz < newBudget)
  {
    cursor = cursor->nextAncestor;
    sz += HM_getChunkListSize(HM_HH_getChunkList(cursor));
  }
  uint32_t desiredMinDepth = HM_HH_getDepth(cursor);

  assert(desiredMinDepth >= minDepthOkayForBudget);
  assert(desiredMinDepth >= potentialLocalScope);
  assert(desiredMinDepth <= thread->currentDepth);

  return desiredMinDepth;
}

#endif /* MLTON_GC_INTERNAL_FUNCS */

/*******************************/
/* Static Function Definitions */
/*******************************/

#if ASSERT

void assertInvariants(GC_thread thread)
{
  HM_HierarchicalHeap hh = thread->hierarchicalHeap;
  assert(NULL != hh);

  for (HM_HierarchicalHeap cursor = hh;
       cursor != NULL;
       cursor = cursor->nextAncestor)
  {
    HM_HH_isLevelHead(cursor);
    HM_chunkList list = HM_HH_getChunkList(cursor);
    assert(NULL != list);
    HM_assertChunkListInvariants(list);

    /* verify levelHeads */
    for (HM_chunk chunk = list->firstChunk;
         NULL != chunk;
         chunk = chunk->nextChunk)
    {
      assert(HM_getLevelHead(chunk) == cursor);
    }
  }

  /* check sorted by depth */
  uint32_t lastDepth = UINT32_MAX;
  for (HM_HierarchicalHeap cursor = hh;
       cursor != NULL;
       cursor = cursor->nextAncestor)
  {
    uint32_t thisDepth = HM_HH_getDepth(cursor);
    assert(thisDepth < lastDepth);
    lastDepth = thisDepth;
  }

  /* Check that we haven't exceeded the currentDepth */
  uint32_t heapDepth = HM_HH_getDepth(hh);
  assert(heapDepth <= thread->currentDepth);

  /* make sure that the current chunk is owned by this thread. */
  if (NULL != thread->currentChunk) {
    HM_HierarchicalHeap levelHead = HM_getLevelHead(thread->currentChunk);
    bool foundChunk = FALSE;
    for (HM_chunk chunk = HM_HH_getChunkList(levelHead)->firstChunk;
         chunk != NULL;
         chunk = chunk->nextChunk)
    {
      if (chunk == thread->currentChunk) {
        foundChunk = TRUE;
        break;
      }
    }
    assert(foundChunk);
  }
}

#else

void assertInvariants(ARG_USED_FOR_ASSERT GC_thread thread) {
  return;
}

#endif /* ASSERT */

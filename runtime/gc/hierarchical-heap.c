/* Copyright (C) 2014,2015 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
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

static void assertInvariants(GC_state s, GC_thread thread);

/************************/
/* Function Definitions */
/************************/
#if (defined (MLTON_GC_INTERNAL_FUNCS))

void HM_HH_merge(GC_state s, GC_thread parentThread, GC_thread childThread)
{
  assert(parentThread->hierarchicalHeap != NULL);
  assert(childThread->hierarchicalHeap != NULL);
  struct HM_HierarchicalHeap* parentHH = parentThread->hierarchicalHeap;
  struct HM_HierarchicalHeap* hh = childThread->hierarchicalHeap;

  assertInvariants(s, parentThread);
  assertInvariants(s, childThread);
  /* can only merge at join point! */
  assert(childThread->currentDepth == parentThread->currentDepth);
  assert(childThread->currentDepth >= 1);

  /* Merge levels. */
  FOR_LEVEL_IN_RANGE(level, i, hh, 0, HM_MAX_NUM_LEVELS, {
    HM_chunkList mirrorLevel = HM_HH_LEVEL(parentHH, i);
    if (mirrorLevel == NULL) {
      HM_HH_LEVEL(parentHH, i) = level;
      level->containingHH = parentHH;
    } else {
      HM_appendChunkList(mirrorLevel, level);
    }
    HM_HH_LEVEL(hh, i) = NULL;
  });

  parentThread->bytesSurvivedLastCollection +=
    childThread->bytesSurvivedLastCollection;
  parentThread->bytesAllocatedSinceLastCollection +=
    childThread->bytesAllocatedSinceLastCollection;

  assertInvariants(s, parentThread);

  Trace2(EVENT_MERGED_HEAP, (EventInt)parentHH, (EventInt)hh);

  // free(hh);
}

void HM_HH_promoteChunks(GC_state s, GC_thread thread)
{
  struct HM_HierarchicalHeap* hh = thread->hierarchicalHeap;

  HM_chunkList level = HM_HH_LEVEL(hh, thread->currentDepth);
  if (level != NULL) {
    assert(thread->currentDepth > 0);
    HM_chunkList parentLevel = HM_HH_LEVEL(hh, thread->currentDepth-1);
    if (parentLevel != NULL) {
      HM_appendChunkList(parentLevel, level);
    } else {
      HM_HH_LEVEL(hh, thread->currentDepth-1) = level;
      /* SAM_NOTE: this naming convention is bad. Should rename the integer to
       * `depth`, and leave `level` to refer to the actual list itself */
      level->level = thread->currentDepth-1;
    }
    HM_HH_LEVEL(hh, thread->currentDepth) = NULL;
  }

  assertInvariants(s, thread);
}

void HM_HH_display (struct HM_HierarchicalHeap* hh, FILE* stream) {
  fprintf (stream,
           "\tlastAllocatedChunk = %p\n",
           (void*)hh->lastAllocatedChunk);
}

struct HM_HierarchicalHeap* HM_HH_new(GC_state s) {

  /* SAM_NOTE: TODO: switch to arena allocation if this is a bottleneck? */
  // struct HM_HierarchicalHeap* hh =
  //   (struct HM_HierarchicalHeap*)malloc(sizeof(struct HM_HierarchicalHeap));
  // if (hh == NULL) {
  //   DIE("Out of memory. Could not allocate new HH object.");
  //   return NULL;
  // }

  size_t bytesNeeded = sizeof(struct HM_HierarchicalHeap);
  HM_chunk sourceChunk = HM_getChunkListLastChunk(s->extraSmallObjects);
  if (NULL == sourceChunk ||
      (size_t)(sourceChunk->limit - sourceChunk->frontier) < bytesNeeded) {
    sourceChunk = HM_allocateChunk(s->extraSmallObjects, bytesNeeded);
  }
  pointer frontier = HM_getChunkFrontier(sourceChunk);
  HM_updateChunkValues(sourceChunk, frontier+bytesNeeded);
  struct HM_HierarchicalHeap* hh = (struct HM_HierarchicalHeap *)frontier;

  for (int i = 0; i < HM_MAX_NUM_LEVELS; i++) {
    HM_HH_LEVEL(hh, i) = NULL;
  }
  hh->lastAllocatedChunk = NULL;

  return hh;
}

void HM_HH_ensureNotEmpty(GC_thread thread) {
  struct HM_HierarchicalHeap* hh = thread->hierarchicalHeap;

  if (NULL != hh->lastAllocatedChunk) return;

#if ASSERT
  FOR_LEVEL_IN_RANGE(level, i, hh, 0, HM_MAX_NUM_LEVELS, {
    assert(level->firstChunk == NULL);
  });
#endif

  /* add in one chunk */
  if (!HM_HH_extend(thread, GC_HEAP_LIMIT_SLOP)) {
    DIE("Ran out of space for Hierarchical Heap!");
  }
}

bool HM_HH_extend(GC_thread thread, size_t bytesRequested)
{
  struct HM_HierarchicalHeap* hh = thread->hierarchicalHeap;
  uint32_t d = thread->currentDepth;

  HM_chunkList levelHead = HM_HH_LEVEL(hh, d);
  if (NULL == levelHead) {
    levelHead = HM_newChunkList(hh, d);
    HM_HH_LEVEL(hh, d) = levelHead;
  }

  HM_chunk chunk = HM_allocateChunk(levelHead, bytesRequested);

  if (NULL == chunk) {
    return FALSE;
  }

  hh->lastAllocatedChunk = chunk;
  HM_HH_addRecentBytesAllocated(thread, HM_getChunkSize(chunk));

  return TRUE;
}

struct HM_HierarchicalHeap* HM_HH_getCurrent(GC_state s) {
  return getThreadCurrent(s)->hierarchicalHeap;
}

pointer HM_HH_getFrontier(struct HM_HierarchicalHeap* hh) {
  assert(blockOf(HM_getChunkFrontier(hh->lastAllocatedChunk)) == (pointer)hh->lastAllocatedChunk);
  return HM_getChunkFrontier(hh->lastAllocatedChunk);
}

pointer HM_HH_getLimit(struct HM_HierarchicalHeap* hh) {
  return HM_getChunkLimit(hh->lastAllocatedChunk);
}

void HM_HH_updateValues(struct HM_HierarchicalHeap* hh,
                        pointer frontier) {
  HM_updateChunkValues(hh->lastAllocatedChunk, frontier);
}

size_t HM_HH_size(struct HM_HierarchicalHeap* hh, uint32_t currentLevel) {
  size_t sz = 0;
  FOR_LEVEL_IN_RANGE(level, i, hh, 0, currentLevel+1, {
    sz += HM_getChunkListSize(level);
  });
  return sz;
}

size_t HM_HH_nextCollectionThreshold(GC_state s, size_t survivingSize) {
  size_t threshold =
    (size_t)((double)survivingSize * s->controls->hhConfig.liveLCRatio);
  if (threshold < s->controls->hhConfig.initialLCHS) {
    threshold = s->controls->hhConfig.initialLCHS;
  }
  return threshold;
}

size_t HM_HH_addRecentBytesAllocated(GC_thread thread, size_t bytes) {
  thread->bytesAllocatedSinceLastCollection += bytes;
  return thread->bytesAllocatedSinceLastCollection;
}

size_t HM_HH_levelSize(struct HM_HierarchicalHeap *hh, uint32_t level) {
  HM_chunkList lev = HM_HH_LEVEL(hh, level);
  if (NULL == lev) return 0;
  return HM_getChunkListSize(lev);
}

uint32_t HM_HH_desiredCollectionScope(GC_state s, GC_thread thread)
{
  struct HM_HierarchicalHeap* hh = thread->hierarchicalHeap;

  if (s->wsQueueTop == BOGUS_OBJPTR)
    return thread->currentDepth+1; /* don't collect */

  if (thread->bytesAllocatedSinceLastCollection <
      (s->controls->hhConfig.liveLCRatio * thread->bytesSurvivedLastCollection))
  {
    return thread->currentDepth+1; /* don't collect */
  }

  uint64_t topval = *(uint64_t*)objptrToPointer(s->wsQueueTop, NULL);
  uint32_t potentialLocalScope = UNPACK_IDX(topval);

  size_t budget = 4 * thread->bytesAllocatedSinceLastCollection;

  if (budget < (1024L * 1024L) || potentialLocalScope > thread->currentDepth)
    return thread->currentDepth+1; /* don't collect */

  size_t sz = 0;
  uint32_t d = thread->currentDepth+1;
  while (d > potentialLocalScope && sz + HM_HH_levelSize(hh, d-1) < budget) {
    sz += HM_HH_levelSize(hh, d-1);
    d--;
  }

  if (sz < (1024L * 1024L))
    return thread->currentDepth+1; /* don't collect if too small */

  /* It's likely that the shallower levels are mostly empty, so let's see if
   * we can skip some of them without ignoring too much data. */
  size_t szMin = 0.75 * sz;
  while (d < thread->currentDepth && sz - HM_HH_levelSize(hh, d) > szMin) {
    sz -= HM_HH_levelSize(hh, d);
    d++;
  }

  return d;
}

#endif /* MLTON_GC_INTERNAL_FUNCS */

/*******************************/
/* Static Function Definitions */
/*******************************/

#if ASSERT
void assertInvariants(__attribute__((unused)) GC_state s,
                      GC_thread thread)
{
  struct HM_HierarchicalHeap* hh = thread->hierarchicalHeap;

  if (NULL != hh->lastAllocatedChunk) {
    HM_chunkList levelHead = HM_getLevelHead(hh->lastAllocatedChunk);
    assert(levelHead->containingHH == hh);
    bool foundChunk = FALSE;
    for (HM_chunk chunk = levelHead->firstChunk; chunk != NULL; chunk = chunk->nextChunk) {
      if (chunk == hh->lastAllocatedChunk) {
        foundChunk = TRUE;
        break;
      }
    }
    assert(foundChunk);
  } else {
    FOR_LEVEL_IN_RANGE(level, i, hh, 0, HM_MAX_NUM_LEVELS, {
      assert(level->firstChunk == NULL);
      assert(level->size == 0);
    });
  }
  HM_assertLevelListInvariants(hh, false);

  /* Check that all chunk lists are levelHeads */
  for (uint32_t i = 0; i < HM_MAX_NUM_LEVELS; i++) {
    HM_chunkList list = HM_HH_LEVEL(hh, i);
    if (list != NULL) {
      assert(HM_isLevelHead(list));
    }
  }

  /* Check that the levels past the recorded level are empty */
  size_t extraSize = 0;
  FOR_LEVEL_IN_RANGE(level, i, hh, thread->currentDepth+1, HM_MAX_NUM_LEVELS, {
    extraSize += HM_getChunkListSize(level);
  });
  assert(0 == extraSize);
}
#else
void assertInvariants(GC_state s, GC_thread thread) {
  ((void)(s));
  ((void)(thread));
}
#endif /* ASSERT */

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

/**
 * Asserts all of the invariants assumed for the struct HM_HierarchicalHeap.
 *
 * @attention
 * If an assertion fails, this function aborts the program, as per the assert()
 * macro.
 *
 * @param s The GC_state to use
 * @param hh The struct HM_HierarchicalHeap to assert invariants for
 * @param state The expected state of the hh
 */
static void assertInvariants(GC_state s,
                             struct HM_HierarchicalHeap* hh);

/************************/
/* Function Definitions */
/************************/
#if (defined (MLTON_GC_INTERNAL_FUNCS))

#pragma message "Remember to do enter/leave correctly for new HH primitives."
void HM_HH_merge(GC_state s, struct HM_HierarchicalHeap* parentHH, struct HM_HierarchicalHeap* hh) {
  /* SAM_NOTE: will need to move this to the appropriate places... */
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
  getThreadCurrent(s)->exnStack = s->exnStack;
  beginAtomic (s);

  assert(threadAndHeapOkay(s));

  /* SAM_NOTE: Why do we need to ensure here?? Is it just to ensure current
   * level? */
  HM_ensureHierarchicalHeapAssurances(s, false, GC_HEAP_LIMIT_SLOP, true);

  endAtomic(s);

  /*
   * This should be true, otherwise our call to
   * HM_ensureHierarchicalHeapAssurances() above was on the wrong heap!
   */
  assert(getHierarchicalHeapCurrent(s) == parentHH);

  assertInvariants(s, parentHH);
  assertInvariants(s, hh);
  /* can only merge at join point! */
  assert(hh->level == parentHH->level);

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

  parentHH->bytesAllocatedSinceLastCollection +=
    hh->bytesAllocatedSinceLastCollection;

  assertInvariants(s, parentHH);

  Trace2(EVENT_MERGED_HEAP, (EventInt)parentHH, (EventInt)hh);

  free(hh);
}

void HM_HH_promoteChunks(GC_state s, struct HM_HierarchicalHeap* hh) {

  HM_chunkList level = HM_HH_LEVEL(hh, hh->level);
  if (level != NULL) {
    assert(hh->level > 0);
    HM_chunkList parentLevel = HM_HH_LEVEL(hh, hh->level-1);
    if (parentLevel != NULL) {
      HM_appendChunkList(parentLevel, level);
    } else {
      HM_HH_LEVEL(hh, hh->level-1) = level;
      /* SAM_NOTE: this naming convention is bad. Should rename the integer to
       * `depth`, and leave `level` to refer to the actual list itself */
      level->level = hh->level-1;
    }
    HM_HH_LEVEL(hh, hh->level) = NULL;
  }

  assertInvariants(s, hh);
}

/* SAM_NOTE: TODO: hijack this function with ensureBytesFree */
void HM_HH_setLevel(__attribute__((unused)) GC_state s,
                    struct HM_HierarchicalHeap* hh,
                    uint32_t level)
{
  hh->level = level;

  if (hh->level >= HM_MAX_NUM_LEVELS) {
    DIE("Exceeded maximum fork depth (%d)", HM_MAX_NUM_LEVELS);
  }

  /* SAM_NOTE: TODO: This still appears to be broken; debugging needed. */
  // if (!(s->controls->mayUseAncestorChunk)) {
  //   uint32_t allocLevel = HM_getHighestLevel(hh->levelList);
  //   assert(getLevelHead(hh->lastAllocatedChunk)->level == allocLevel);
  //   assert(allocLevel <= level);
  //   if (allocLevel != level) {
  //     HM_HH_updateValues(hh, s->frontier);
  //     HM_HH_extend(hh, GC_HEAP_LIMIT_SLOP);
  //     s->frontier = HM_HH_getFrontier(hh);
  //     s->limitPlusSlop = HM_HH_getLimit(hh);
  //     s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
  //   }

  //   /* This is an alternative implementation which might have slightly higher
  //    * overhead. */
  //   // getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
  //   // getThreadCurrent(s)->exnStack = s->exnStack;
  //   // getThreadCurrent(s)->bytesNeeded = 0;
  //   // HM_ensureHierarchicalHeapAssurances(s, FALSE, GC_HEAP_LIMIT_SLOP, TRUE);
  // }

  assert(inSameBlock(s->frontier, s->limitPlusSlop-1));
  assert(((HM_chunk)blockOf(s->frontier))->magic == CHUNK_MAGIC);
}

void HM_HH_display (struct HM_HierarchicalHeap* hh, FILE* stream) {
  fprintf (stream,
           "\tlastAllocatedChunk = %p\n"
           "\tlevel = %u\n",
           (void*)hh->lastAllocatedChunk,
           hh->level);
}

struct HM_HierarchicalHeap* HM_HH_new(__attribute__((unused)) GC_state s) {

  /* SAM_NOTE: TODO: switch to arena allocation if this is a bottleneck? */
  struct HM_HierarchicalHeap* hh =
    (struct HM_HierarchicalHeap*)malloc(sizeof(struct HM_HierarchicalHeap));
  if (hh == NULL) {
    DIE("Out of memory. Could not allocate new HH object.");
    return NULL;
  }

  for (int i = 0; i < HM_MAX_NUM_LEVELS; i++) {
    HM_HH_LEVEL(hh, i) = NULL;
  }
  hh->lastAllocatedChunk = NULL;
  hh->level = 0;
  hh->bytesAllocatedSinceLastCollection = 0;

  return hh;
}

void HM_HH_ensureNotEmpty(struct HM_HierarchicalHeap* hh) {
  if (NULL != hh->lastAllocatedChunk) return;

#if ASSERT
  FOR_LEVEL_IN_RANGE(level, i, hh, 0, HM_MAX_NUM_LEVELS, {
    assert(level->firstChunk == NULL);
  });
#endif

  /* add in one chunk */
  if (!HM_HH_extend(hh, GC_HEAP_LIMIT_SLOP)) {
    DIE("Ran out of space for Hierarchical Heap!");
  }
}

bool HM_HH_extend(struct HM_HierarchicalHeap* hh, size_t bytesRequested) {
  HM_chunkList levelHead = HM_HH_LEVEL(hh, hh->level);
  if (NULL == levelHead) {
    levelHead = HM_newChunkList(hh, hh->level);
    HM_HH_LEVEL(hh, hh->level) = levelHead;
  }

  HM_chunk chunk = HM_allocateChunk(levelHead, bytesRequested);

  if (NULL == chunk) {
    return FALSE;
  }

  hh->lastAllocatedChunk = chunk;
  HM_HH_addRecentBytesAllocated(hh, HM_getChunkSize(chunk));

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

size_t HM_HH_size(struct HM_HierarchicalHeap* hh) {
  size_t sz = 0;
  FOR_LEVEL_IN_RANGE(level, i, hh, 0, hh->level+1, {
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

size_t HM_HH_addRecentBytesAllocated(struct HM_HierarchicalHeap* hh, size_t bytes) {
  hh->bytesAllocatedSinceLastCollection += bytes;
  return hh->bytesAllocatedSinceLastCollection;
}

size_t HM_HH_levelSize(struct HM_HierarchicalHeap *hh, uint32_t level) {
  HM_chunkList lev = HM_HH_LEVEL(hh, level);
  if (NULL == lev) return 0;
  return HM_getChunkListSize(lev);
}

uint32_t HM_HH_desiredCollectionScope(
  GC_state s,
  struct HM_HierarchicalHeap* hh)
{
  if (s->wsQueueTop == BOGUS_OBJPTR)
    return hh->level+1; /* don't collect */

  uint64_t topval = *(uint64_t*)objptrToPointer(s->wsQueueTop, NULL);
  uint32_t potentialLocalScope = UNPACK_IDX(topval);

  size_t budget = hh->bytesAllocatedSinceLastCollection;

  if (budget <= (16L * 1024L * 1024L) || potentialLocalScope > hh->level)
    return hh->level+1; /* don't collect */

  size_t sz = 0;
  uint32_t level = hh->level+1;
  while (level > potentialLocalScope && sz + HM_HH_levelSize(hh, level-1) < budget) {
    sz += HM_HH_levelSize(hh, level-1);
    level--;
  }

  /* It's likely that the shallower levels are mostly empty, so let's see if
   * we can skip some of them without ignoring too much data. */
  size_t szMin = 0.75 * sz;
  while (level < hh->level && sz - HM_HH_levelSize(hh, level+1) > szMin) {
    sz -= HM_HH_levelSize(hh, level+1);
    level++;
  }

  return level;
}

#endif /* MLTON_GC_INTERNAL_FUNCS */

/*******************************/
/* Static Function Definitions */
/*******************************/

#if ASSERT
void assertInvariants(__attribute__((unused)) GC_state s,
                      struct HM_HierarchicalHeap* hh) {
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
  FOR_LEVEL_IN_RANGE(level, i, hh, hh->level+1, HM_MAX_NUM_LEVELS, {
    extraSize += HM_getChunkListSize(level);
  });
  assert(0 == extraSize);
}
#else
void assertInvariants(GC_state s,
                      struct HM_HierarchicalHeap* hh) {
  ((void)(s));
  ((void)(hh));
}
#endif /* ASSERT */

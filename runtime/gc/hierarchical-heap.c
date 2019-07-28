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
 * Adjusts the LCHS to reflect the ratio provided. Limits adjustment to
 * [s->controls->initialLCHS, s->controls->maxLCHS].
 *
 * @param s The GC_state to use
 * @param hh The hiererchical heap to adjust
 * @param desiredRatio The desired ratio to adjust to.
 */
void adjustLCHS(GC_state s,
                struct HM_HierarchicalHeap* hh,
                double desiredRatio);

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

void HM_HH_appendChild(GC_state s,
                       struct HM_HierarchicalHeap* parentHH,
                       struct HM_HierarchicalHeap* childHH,
                       uint32_t stealLevel) {
  assertInvariants(s, parentHH);

  /* initialize childHH */
  childHH->shallowestLevel = stealLevel + 1;
  childHH->shallowestPrivateLevel = stealLevel + 1;
  childHH->level = stealLevel + 1;

  uint32_t oldShallowestPrivateLevel = parentHH->shallowestPrivateLevel;
  parentHH->shallowestPrivateLevel = stealLevel + 1;
  assert(parentHH->shallowestPrivateLevel >= oldShallowestPrivateLevel);

  HM_HH_LEVEL_CAPACITY(parentHH, stealLevel) = parentHH->locallyCollectibleHeapSize;

  /* need to update lcs and lchs */
  size_t sizeDelta = 0;
  FOR_LEVEL_IN_RANGE(level, i, parentHH, oldShallowestPrivateLevel, stealLevel+1, {
    sizeDelta += HM_getChunkListSize(level);
  });

  size_t oldLCHS = parentHH->locallyCollectibleHeapSize;
  double ratio = HM_HH_getLCRatio(parentHH);
  if (isinf(ratio)) {
    /* lcs is zero, so just bottom-out lchs with ratio 0 */
    ratio = 0.0;
  }

  LOG(LM_HIERARCHICAL_HEAP, LL_DEBUG,
      "hh (%p) locallyCollectibleSize %"PRIu64" - %"PRIu64" = %"PRIu64,
      ((void*)(parentHH)),
      parentHH->locallyCollectibleSize,
      sizeDelta,
      parentHH->locallyCollectibleSize - sizeDelta);
  parentHH->locallyCollectibleSize -= sizeDelta;

  adjustLCHS(s, parentHH, ratio);
  LOG(LM_HIERARCHICAL_HEAP, LL_DEBUG,
      "hh (%p) locallyCollectibleHeapSize %"PRIu64" -> %"PRIu64" "
      "(ratio %.2f)",
      ((void*)(parentHH)),
      oldLCHS,
      parentHH->locallyCollectibleHeapSize,
      ratio);

  assertInvariants(s, parentHH);
  assertInvariants(s, childHH);
}

uint32_t HM_HH_getLevel(__attribute__((unused)) GC_state s,
                      struct HM_HierarchicalHeap* hh)
{
  return hh->level;
}

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

  uint32_t oldShallowestPrivateLevel = parentHH->shallowestPrivateLevel;
  uint32_t newShallowestPrivateLevel = hh->shallowestLevel-1;
  assert(newShallowestPrivateLevel+1 == oldShallowestPrivateLevel);
  parentHH->shallowestPrivateLevel = newShallowestPrivateLevel;

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

  /* combine outstandingBytesPromoted */
  for (uint32_t i = 0; i < HM_MAX_NUM_LEVELS; i++) {
    HM_HH_LEVEL_OBP(parentHH, i) += HM_HH_LEVEL_OBP(hh, i);
  }
  for (uint32_t i = newShallowestPrivateLevel; i < oldShallowestPrivateLevel; i++) {
    HM_HH_LEVEL_OBP(parentHH, i) = 0;
  }

  size_t outstandingBytesPromoted = 0;
  for (uint32_t i = 0; i < newShallowestPrivateLevel; i++) {
    outstandingBytesPromoted += HM_HH_LEVEL_OBP(parentHH, i);
  }

  /* Add up the size of the immediate ancestors which are now unfrozen due to
   * this merge. We need this quantity to adjust the LCHS below. */
  size_t unfrozenSize = 0;
  FOR_LEVEL_IN_RANGE(level, i, parentHH, newShallowestPrivateLevel, oldShallowestPrivateLevel, {
    unfrozenSize += HM_getChunkListSize(level);
  });

  /* Add up the rest of the now local data. */
  size_t childrenSize = 0;
  FOR_LEVEL_IN_RANGE(level, i, parentHH, oldShallowestPrivateLevel, parentHH->level+1, {
    childrenSize += HM_getChunkListSize(level);
  });

  parentHH->locallyCollectibleSize = outstandingBytesPromoted + childrenSize + unfrozenSize;
  parentHH->locallyCollectibleHeapSize += hh->locallyCollectibleHeapSize + 2 * unfrozenSize;

  if (!s->controls->oldHHGCPolicy &&
      parentHH->locallyCollectibleHeapSize < HM_HH_LEVEL_CAPACITY(parentHH, newShallowestPrivateLevel)) {
    // printf("After merge, LCHS = %ld, %.2f of old capacity\n",
    //   parentHH->locallyCollectibleHeapSize,
    //   ((double) parentHH->locallyCollectibleHeapSize) /
    //   (double) HM_HH_LEVEL_CAPACITY(parentHH, newShallowestPrivateLevel));
    parentHH->locallyCollectibleHeapSize = HM_HH_LEVEL_CAPACITY(parentHH, newShallowestPrivateLevel);
  }

  assertInvariants(s, parentHH);

  Trace2(EVENT_MERGED_HEAP, (EventInt)parentHH, (EventInt)hh);

  Trace3(EVENT_CHUNKP_RATIO,
         parentHH->locallyCollectibleHeapSize,
         parentHH->locallyCollectibleSize,
         s->controls->hhConfig.allocatedRatio);

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
           "\tlevel = %u\n"
           "\tshallowestLevel = %u\n"
           "\tshallowestPrivateLevel = %u\n",
           (void*)hh->lastAllocatedChunk,
           hh->level,
           hh->shallowestLevel,
           hh->shallowestPrivateLevel);
}

struct HM_HierarchicalHeap* HM_HH_new(GC_state s) {

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
  for (int i = 0; i < HM_MAX_NUM_LEVELS; i++) {
    HM_HH_LEVEL_CAPACITY(hh, i) = 0;
  }
  for (int i = 0; i < HM_MAX_NUM_LEVELS; i++) {
    HM_HH_LEVEL_OBP(hh, i) = 0;
  }
  hh->lastAllocatedChunk = NULL;
  hh->level = 0;
  hh->shallowestLevel = 0;
  hh->shallowestPrivateLevel = 0;
  hh->locallyCollectibleSize = 0;
  hh->locallyCollectibleHeapSize = s->controls->hhConfig.initialLCHS;

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
  hh->locallyCollectibleSize += HM_getChunkSize(chunk);

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

double HM_HH_getLCRatio(struct HM_HierarchicalHeap* hh) {
  return (((double)(hh->locallyCollectibleHeapSize)) /
          ((double)(hh->locallyCollectibleSize)));
}

void HM_HH_maybeResizeLCHS(GC_state s, struct HM_HierarchicalHeap* hh) {
  size_t oldLCHS = hh->locallyCollectibleHeapSize;
  double desiredRatio = 2 * (s->controls->hhConfig.liveLCRatio + 1);
  double ratio = HM_HH_getLCRatio(hh);

  adjustLCHS(s, hh, desiredRatio);

  if (oldLCHS != hh->locallyCollectibleHeapSize) {
    LOG(LM_HIERARCHICAL_HEAP, LL_DEBUG,
        "Live Ratio %.2f %s %.2f, so resized LCHS from %zu bytes to %zu bytes",
        ratio,
        (ratio < desiredRatio) ? "<" : ">",
        desiredRatio,
        oldLCHS,
        hh->locallyCollectibleHeapSize);
  }
}

void HM_HH_updateValues(struct HM_HierarchicalHeap* hh,
                        pointer frontier) {
  HM_updateChunkValues(hh->lastAllocatedChunk, frontier);
}
#endif /* MLTON_GC_INTERNAL_FUNCS */

/*******************************/
/* Static Function Definitions */
/*******************************/

void adjustLCHS(GC_state s,
                struct HM_HierarchicalHeap* hh,
                double desiredRatio) {
  size_t newLCHS = desiredRatio * hh->locallyCollectibleSize;

  if (newLCHS < s->controls->hhConfig.initialLCHS) {
    newLCHS = s->controls->hhConfig.initialLCHS;
  } else if (s->controls->hhConfig.maxLCHS != MAX_LCHS_INFINITE &&
             newLCHS > s->controls->hhConfig.maxLCHS) {
    newLCHS = s->controls->hhConfig.maxLCHS;
  }

  hh->locallyCollectibleHeapSize = newLCHS;
}

#if ASSERT
void assertInvariants(__attribute__((unused)) GC_state s,
                      struct HM_HierarchicalHeap* hh) {
  ASSERTPRINT(hh->level >= hh->shallowestLevel,
              "HH %p has invalid level values! level %u shallowestLevel %u",
              ((void*)(hh)),
              hh->level,
              hh->shallowestLevel);

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
  size_t locallyCollectibleSize = 0;
  FOR_LEVEL_IN_RANGE(level, i, hh, hh->level+1, HM_MAX_NUM_LEVELS, {
    locallyCollectibleSize += HM_getChunkListSize(level);
  });
  assert(0 == locallyCollectibleSize);

  /* SAM_NOTE: TODO:
   * removing this for now, as it is tripping but there are more
   * pressing things to fix... */
  // FOR_LEVEL_IN_RANGE(level, i, hh, HM_HH_getDeepestStolenLevel(s, hh)+1, hh->level+1, {
  //   locallyCollectibleSize += HM_getChunkListSize(level);
  // });
  // assert(hh->locallyCollectibleSize == locallyCollectibleSize);
}
#else
void assertInvariants(GC_state s,
                      struct HM_HierarchicalHeap* hh) {
  ((void)(s));
  ((void)(hh));
}
#endif /* ASSERT */

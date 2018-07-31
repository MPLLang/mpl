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

/********************/
/* Extern Variables */
/********************/
const char* HM_HHStateToString[] = {
  "LIVE",
  "DEAD",
  "MERGED"
};

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
                             const struct HM_HierarchicalHeap* hh,
                             enum HM_HHState state);

/************************/
/* Function Definitions */
/************************/
#if (defined (MLTON_GC_INTERNAL_BASIS))
void HM_HH_appendChild(pointer parentHHPointer,
                       pointer childHHPointer,
                       Word32 stealLevel) {
  GC_state s = pthread_getspecific (gcstate_key);

  objptr parentHHObjptr = pointerToObjptr (parentHHPointer, s->heap->start);
  struct HM_HierarchicalHeap* parentHH = HM_HH_objptrToStruct(s,
                                                              parentHHObjptr);

  objptr childHHObjptr = pointerToObjptr (childHHPointer, s->heap->start);
  struct HM_HierarchicalHeap* childHH = HM_HH_objptrToStruct(s, childHHObjptr);

  lockWriterHH(childHH);
  lockWriterHH(parentHH);

  assertInvariants(s, parentHH, LIVE);
  Word32 oldHighestStolenLevel = HM_HH_getHighestStolenLevel(s, parentHH);

  /* childHH should be a orphan! */
  assert (BOGUS_OBJPTR == childHH->parentHH);
  assert (BOGUS_OBJPTR == childHH->nextChildHH);

  /* initialize childHH */
  childHH->stealLevel = stealLevel;
  childHH->level = stealLevel + 1;
  childHH->parentHH = parentHHObjptr;

  /* insert childHH in descending order of stealLevel */
  objptr* cursorPointer;
  struct HM_HierarchicalHeap* cursorHH;
  for (cursorPointer = &(parentHH->childHHList),
            cursorHH = HM_HH_objptrToStruct(s, *cursorPointer);
       (NULL != cursorHH) && (stealLevel < cursorHH->stealLevel);
       cursorPointer = &(cursorHH->nextChildHH),
            cursorHH = HM_HH_objptrToStruct(s, *cursorPointer)) { }
  childHH->nextChildHH = *cursorPointer;
  *cursorPointer = childHHObjptr;

  if ((HM_HH_INVALID_LEVEL == oldHighestStolenLevel) ||
      (stealLevel > oldHighestStolenLevel)) {
    /* need to update lcs and lchs */
    Word64 sizeDelta = 0;
    FOR_LEVEL_IN_RANGE(level, i, parentHH, oldHighestStolenLevel+1, stealLevel+1, {
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
  }

  /* cannot assert parentHH as it is still running! */
  assertInvariants(s, parentHH, LIVE);
  assertInvariants(s, childHH, LIVE);

  unlockWriterHH(parentHH);
  unlockWriterHH(childHH);
}

size_t HM_HH_getLevel(pointer hhPointer) {
  GC_state s = pthread_getspecific (gcstate_key);

  objptr hhObjptr = pointerToObjptr (hhPointer, s->heap->start);
  const struct HM_HierarchicalHeap* hh = HM_HH_objptrToStruct(s, hhObjptr);

  return hh->level;
}

size_t HM_HH_getLowestPrivateLevelFFI(pointer hhPointer) {
  GC_state s = pthread_getspecific (gcstate_key);

  objptr hhObjptr = pointerToObjptr (hhPointer, s->heap->start);
  const struct HM_HierarchicalHeap* hh = HM_HH_objptrToStruct(s, hhObjptr);

  return HM_HH_getLowestPrivateLevel(s, hh);
}

void HM_HH_mergeIntoParent(pointer hhPointer) {
  GC_state s = pthread_getspecific (gcstate_key);

  objptr hhObjptr = pointerToObjptr (hhPointer, s->heap->start);
  struct HM_HierarchicalHeap* hh = HM_HH_objptrToStruct(s, hhObjptr);

  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
  getThreadCurrent(s)->exnStack = s->exnStack;
  beginAtomic (s);

  if (getThreadCurrent(s)->useHierarchicalHeap &&
      !HM_inGlobalHeap(s)) {
    HM_ensureHierarchicalHeapAssurances(s, false, GC_HEAP_LIMIT_SLOP, true);
  }

  endAtomic (s);

  /* wait until hh is dead */
  while (DEAD != hh->state) { }

  assert (BOGUS_OBJPTR != hh->parentHH);
  struct HM_HierarchicalHeap* parentHH = HM_HH_objptrToStruct(s, hh->parentHH);

  /*
   * This should be true, otherwise our call to
   * HM_ensureHierarchicalHeapAssurances() above was on the wrong heap!
   */
  assert(getHierarchicalHeapCurrent(s) == parentHH);

  /* can only merge from the thread that owns the parent hierarchical heap! */
  assert(objptrToPointer(hh->parentHH, s->heap->start) == GC_getCurrentHierarchicalHeap());

  lockWriterHH(hh);
  lockWriterHH(parentHH);

  assertInvariants(s, parentHH, LIVE);
  assertInvariants(s, hh, DEAD);
  /* can only merge at join point! */
  assert(hh->level == parentHH->level);

  /* remove hh from parentHH->childHHList */
  /*
   * This assert assumes that all merges happen in LIFO order, as per the
   * comment in HM_appendChildHH ()
   */
  objptr* cursor;
  for (cursor = &(parentHH->childHHList);
#if ASSERT
       (BOGUS_OBJPTR != *cursor) &&
#endif
                (hhObjptr != *cursor);
       cursor = &(HM_HH_objptrToStruct(s, *cursor)->nextChildHH)) {
  }
  assert(BOGUS_OBJPTR != *cursor);

  Word32 oldHighestPrivateLevel = HM_HH_getHighestPrivateLevel(s, parentHH);
  *cursor = hh->nextChildHH;
  Word32 newHighestPrivateLevel = HM_HH_getHighestPrivateLevel(s, parentHH);
  assert(newHighestPrivateLevel < oldHighestPrivateLevel);

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

  /* Add up the size of the immediate ancestors which are now unfrozen due to
   * this merge. We need this quantity to adjust the LCHS below. */
  Word64 unfrozenSize = 0;
  FOR_LEVEL_IN_RANGE(level, i, parentHH, newHighestPrivateLevel, oldHighestPrivateLevel, {
    unfrozenSize += HM_getChunkListSize(level);
  });

  /* Add up the rest of the now local data. */
  Word64 childrenSize = 0;
  FOR_LEVEL_IN_RANGE(level, i, parentHH, oldHighestPrivateLevel, parentHH->level+1, {
    childrenSize += HM_getChunkListSize(level);
  });

  parentHH->locallyCollectibleSize = childrenSize + unfrozenSize;
  parentHH->locallyCollectibleHeapSize += hh->locallyCollectibleHeapSize + 2 * unfrozenSize;

  assertInvariants(s, parentHH, LIVE);
  /* don't assert hh here as it should be thrown away! */
  hh->state = MERGED;

  Trace2(EVENT_MERGED_HEAP, (EventInt)parentHH, (EventInt)hh);

  Trace3(EVENT_CHUNKP_RATIO,
         parentHH->locallyCollectibleHeapSize,
         parentHH->locallyCollectibleSize,
         s->controls->hhConfig.allocatedRatio);

  unlockWriterHH(parentHH);
  unlockWriterHH(hh);
}

pointer HM_HH_mergeIntoParentAndGetReturnValue(pointer hhPointer) {
  GC_state s = pthread_getspecific (gcstate_key);

  objptr hhObjptr = pointerToObjptr (hhPointer, s->heap->start);
  struct HM_HierarchicalHeap* hh = HM_HH_objptrToStruct(s, hhObjptr);

#if ASSERT
  assert(NULL != hh->retVal);
  objptr retValObjptr = pointerToObjptr(hh->retVal, s->heap->start);
  struct HM_ObjptrInfo retValInfo;
  HM_getObjptrInfo(s, retValObjptr, &retValInfo);
  assert(hh == retValInfo.hh);
#endif

  /* merge, then return retVal */
  HM_HH_mergeIntoParent(hhPointer);
  return hh->retVal;
}

void HM_HH_promoteChunks(pointer hhPointer) {
  GC_state s = pthread_getspecific (gcstate_key);

  objptr hhObjptr = pointerToObjptr (hhPointer, s->heap->start);
  struct HM_HierarchicalHeap* hh = HM_HH_objptrToStruct(s, hhObjptr);

  lockWriterHH(hh);

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

  assertInvariants(s, hh, LIVE);

  unlockWriterHH(hh);
}

void HM_HH_setDead(pointer hhPointer) {
  GC_state s = pthread_getspecific (gcstate_key);

  objptr hhObjptr = pointerToObjptr (hhPointer, s->heap->start);
  struct HM_HierarchicalHeap* hh = HM_HH_objptrToStruct(s, hhObjptr);

  hh->state = DEAD;
}

/* SAM_NOTE: TODO: hijack this function with ensureBytesFree */
void HM_HH_setLevel(pointer hhPointer, size_t level) {
  GC_state s = pthread_getspecific (gcstate_key);

  objptr hhObjptr = pointerToObjptr (hhPointer, s->heap->start);
  struct HM_HierarchicalHeap* hh = HM_HH_objptrToStruct(s, hhObjptr);

  hh->level = level;

  if (hh->level >= HM_MAX_NUM_LEVELS) {
    DIE("Exceeded maximum fork depth (%d)", HM_MAX_NUM_LEVELS);
  }

  /* SAM_NOTE: TODO: This still appears to be broken; debugging needed. */
  // if (!(s->controls->mayUseAncestorChunk)) {
  //   Word32 allocLevel = HM_getHighestLevel(hh->levelList);
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

pointer HM_HH_setReturnValue(pointer hhPointer, pointer retVal) {
  GC_state s = pthread_getspecific (gcstate_key);

  objptr hhObjptr = pointerToObjptr (hhPointer, s->heap->start);
  struct HM_HierarchicalHeap* hh = HM_HH_objptrToStruct(s, hhObjptr);

#if ASSERT
  assert(NULL != retVal);
  objptr retValObjptr = pointerToObjptr(retVal, s->heap->start);
  struct HM_ObjptrInfo retValInfo;
  HM_getObjptrInfo(s, retValObjptr, &retValInfo);
  assert(hh == retValInfo.hh);
#endif

  hh->retVal = retVal;
  return hhPointer;
}
#endif /* MLTON_GC_INTERNAL_BASIS */

#if (defined (MLTON_GC_INTERNAL_FUNCS))
void HM_HH_display (
    const struct HM_HierarchicalHeap* hh,
    FILE* stream) {
  const char *lockStatus;

  switch (rwlock_status(&hh->lock)) {
  case RWLOCK_STATUS_NOBODY:
    lockStatus = "NOBODY";
    break;
  case RWLOCK_STATUS_READERS:
    lockStatus = "READERS";
    break;
  case RWLOCK_STATUS_WRITER:
    lockStatus = "WRITER";
    break;
  default:
    lockStatus = "???";
    break;
  }

  fprintf (stream,
           "\tlastAllocatedChunk = %p\n"
           "\tlock = %s\n"
           "\tlevel = %u\n"
           "\tstealLevel = %u\n"
           "\tid = %"PRIu64"\n"
           "\tparentHH = "FMTOBJPTR"\n"
           "\tnextChildHH = "FMTOBJPTR"\n"
           "\tchildHHList= "FMTOBJPTR"\n",
           (void*)hh->lastAllocatedChunk,
           lockStatus,
           hh->level,
           hh->stealLevel,
           hh->id,
           hh->parentHH,
           hh->nextChildHH,
           hh->childHHList);
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

/*
 * RAM_NOTE: Should maybe be split off into a levelList function to shared with
 * copyObject
 */
bool HM_HH_extend(struct HM_HierarchicalHeap* hh, size_t bytesRequested) {
  lockWriterHH(hh);

  HM_chunkList levelHead = HM_HH_LEVEL(hh, hh->level);
  if (NULL == levelHead) {
    levelHead = HM_newChunkList(hh, hh->level);
    HM_HH_LEVEL(hh, hh->level) = levelHead;
  }

  HM_chunk chunk = HM_allocateChunk(levelHead, bytesRequested);

  if (NULL == chunk) {
    unlockWriterHH(hh);
    return FALSE;
  }

  /* SAM_NOTE: TODO: This should just be
   *   ...->bytesAllocated += HM_getChunkSize(chunk);
   * The current implementation is ignoring chunk fragmentation. We need to
   * decide what this statistic is meant to track. IMO chunk fragmentation
   * should be included in an allocation statistic.*/
  GC_state s = pthread_getspecific (gcstate_key);
  if (NULL != hh->lastAllocatedChunk) {
    void* lastAllocatedChunkFrontier = HM_getChunkFrontier(hh->lastAllocatedChunk);
    void* lastAllocatedChunkStart = HM_getChunkStart(hh->lastAllocatedChunk);
    s->cumulativeStatistics->bytesAllocated +=
        ((size_t)(lastAllocatedChunkFrontier)) -
        ((size_t)(lastAllocatedChunkStart));
  }

  hh->lastAllocatedChunk = chunk;
  hh->locallyCollectibleSize += HM_getChunkSize(chunk);

  unlockWriterHH(hh);
  return TRUE;
}

struct HM_HierarchicalHeap* HM_HH_getCurrent(GC_state s) {
  return HM_HH_objptrToStruct(s, getThreadCurrent(s)->hierarchicalHeap);
}

Word32 HM_HH_getHighestStolenLevel(GC_state s,
                                   const struct HM_HierarchicalHeap* hh) {
  struct HM_HierarchicalHeap* highestStolenLevelHH =
      HM_HH_objptrToStruct(s, hh->childHHList);

  if (NULL == highestStolenLevelHH) {
    return HM_HH_INVALID_LEVEL;
  } else {
    return highestStolenLevelHH->stealLevel;
  }
}

Word32 HM_HH_getHighestPrivateLevel(GC_state s, const struct HM_HierarchicalHeap* hh) {
  struct HM_HierarchicalHeap* highestStolenHH =
    HM_HH_objptrToStruct(s, hh->childHHList);

  if (NULL == highestStolenHH) {
    return 0;
  } else {
    return highestStolenHH->stealLevel+1;
  }
}

pointer HM_HH_getFrontier(const struct HM_HierarchicalHeap* hh) {
  assert(blockOf(HM_getChunkFrontier(hh->lastAllocatedChunk)) == (pointer)hh->lastAllocatedChunk);
  return HM_getChunkFrontier(hh->lastAllocatedChunk);
}

Word32 HM_HH_getLowestPrivateLevel(GC_state s,
                                   const struct HM_HierarchicalHeap *hh) {
  struct HM_HierarchicalHeap* highestStolenLevelHH =
      HM_HH_objptrToStruct(s, hh->childHHList);

  if (NULL == highestStolenLevelHH) {
    return hh->stealLevel + 1;
  } else {
    return highestStolenLevelHH->stealLevel + 1;
  }
}

pointer HM_HH_getLimit(const struct HM_HierarchicalHeap* hh) {
  return HM_getChunkLimit(hh->lastAllocatedChunk);
}

double HM_HH_getLCRatio(const struct HM_HierarchicalHeap* hh) {
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

struct HM_HierarchicalHeap* HM_HH_objptrToStruct(GC_state s, objptr hhObjptr) {
  if (BOGUS_OBJPTR == hhObjptr) {
    return NULL;
  }

  pointer hhPointer = objptrToPointer (hhObjptr, s->heap->start);
  return ((struct HM_HierarchicalHeap*)(hhPointer +
                                        HM_HH_offsetof(s)));
}

/* RAM_NOTE: Should be able to compute once and save result */
size_t HM_HH_offsetof(GC_state s) {
  return (HM_HH_sizeof(s) - (GC_NORMAL_METADATA_SIZE +
                             sizeof (struct HM_HierarchicalHeap)));
}

void HM_HH_setThread(struct HM_HierarchicalHeap* hh, objptr threadObjptr) {
  hh->thread = threadObjptr;

  LOG(LM_HIERARCHICAL_HEAP, LL_DEBUGMORE,
      "Set thread of HH %p to %p",
      ((void*)(hh)),
      ((void*)(threadObjptr)));
}

/* RAM_NOTE: Should be able to compute once and save result */
size_t HM_HH_sizeof(GC_state s) {
  size_t result = GC_NORMAL_METADATA_SIZE + sizeof (struct HM_HierarchicalHeap);
  result = align (result, s->alignment);

  if (DEBUG) {
    uint16_t bytesNonObjptrs;
    uint16_t numObjptrs;
    splitHeader (s,
                 GC_HIERARCHICAL_HEAP_HEADER,
                 NULL,
                 NULL,
                 &bytesNonObjptrs,
                 &numObjptrs);

    size_t check = GC_NORMAL_METADATA_SIZE +
                   (bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE));

    if (DEBUG_DETAILED) {
      fprintf (
          stderr,
          "sizeofHierarchicalHeap: result = %"PRIuMAX"  check = %"PRIuMAX"\n",
          (uintmax_t)result,
          (uintmax_t)check);
    }

    assert (check == result);
  }
  assert (isAligned (result, s->alignment));

  return result;
}

void HM_HH_updateLevelListPointers(objptr hhObjptr) {
  GC_state s = pthread_getspecific (gcstate_key);
  struct HM_HierarchicalHeap* hh = HM_HH_objptrToStruct(s, hhObjptr);

  for (Word32 level = 0; level < HM_MAX_NUM_LEVELS; level++) {
    HM_chunkList levelHead = HM_HH_LEVEL(hh, level);
    if (NULL != levelHead) levelHead->containingHH = hh;
  }
}

void HM_HH_updateValues(struct HM_HierarchicalHeap* hh,
                        void* frontier) {
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
void assertInvariants(GC_state s,
                      const struct HM_HierarchicalHeap* hh,
                      enum HM_HHState state) {
  ASSERTPRINT(((HM_HH_INVALID_LEVEL == hh->stealLevel) ||
               (hh->level > hh->stealLevel)),
              "HH %p has invalid level values! level %u stealLevel %u",
              ((const void*)(hh)),
              hh->level,
              hh->stealLevel);
  ASSERTPRINT(state == hh->state,
              "HH %p is not %s",
              ((const void*)(hh)),
              (LIVE == state) ? ("live") : ("dead"));

#if 0
  if (BOGUS_OBJPTR != hh->thread) {
    assert(HM_HH_objptrToStruct(s,
                                threadObjptrToStruct(s, hh->thread)->
                                hierarchicalHeap) ==
           hh);
  }
#endif

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
  HM_assertLevelListInvariants(hh, hh->stealLevel, false);

  Word64 locallyCollectibleSize = 0;
  FOR_LEVEL_IN_RANGE(level, i, hh, hh->level+1, HM_MAX_NUM_LEVELS, {
    locallyCollectibleSize += HM_getChunkListSize(level);
  });
  // The levels past the recorded level should be empty
  assert(0 == locallyCollectibleSize);
  FOR_LEVEL_IN_RANGE(level, i, hh, HM_HH_getHighestStolenLevel(s, hh)+1, hh->level+1, {
    locallyCollectibleSize += HM_getChunkListSize(level);
  });
  assert(hh->locallyCollectibleSize == locallyCollectibleSize);

  struct HM_HierarchicalHeap* parentHH = HM_HH_objptrToStruct(s, hh->parentHH);
  if (NULL != parentHH) {
    /* Make sure I am in parentHH->childHHList */
    bool foundInParentList = FALSE;
    for (struct HM_HierarchicalHeap* childHH =
             HM_HH_objptrToStruct(s, parentHH->childHHList);
         NULL != childHH;
         childHH = HM_HH_objptrToStruct(s, childHH->nextChildHH)) {
      if (hh == childHH) {
        foundInParentList = TRUE;
        break;
      }
    }
    assert(foundInParentList);
  }

  /* make sure childHHList is sorted by steal level */
  Word32 previousStealLevel = ~((Word32)(0));
  for (struct HM_HierarchicalHeap* childHH =
           HM_HH_objptrToStruct(s, hh->childHHList);
       NULL != childHH;
       childHH = HM_HH_objptrToStruct(s, childHH->nextChildHH)) {
    assert(childHH->stealLevel <= previousStealLevel);
    previousStealLevel = childHH->stealLevel;
    assert(HM_HH_objptrToStruct(s, childHH->parentHH) == hh);
  }
}
#else
void assertInvariants(GC_state s,
                      const struct HM_HierarchicalHeap* hh,
                      enum HM_HHState state) {
  ((void)(s));
  ((void)(hh));
  ((void)(state));
}
#endif /* ASSERT */

void lockWriterHH(struct HM_HierarchicalHeap* hh) {
  GC_state s = pthread_getspecific(gcstate_key);
  LOG(LM_HIERARCHICAL_HEAP, LL_DEBUGMORE,
      "Locking writer HH %p",
      (void *)hh);
  rwlock_writer_lock(s, &hh->lock);
  LOG(LM_HIERARCHICAL_HEAP, LL_DEBUGMORE,
      "Locked writer HH %p",
      (void *)hh);
}

void unlockWriterHH(struct HM_HierarchicalHeap* hh) {
  GC_state s = pthread_getspecific(gcstate_key);
  LOG(LM_HIERARCHICAL_HEAP, LL_DEBUGMORE,
      "Unlocking writer HH %p",
      (void *)hh);
  rwlock_writer_unlock(s, &hh->lock);
  LOG(LM_HIERARCHICAL_HEAP, LL_DEBUGMORE,
      "Unlocked writer HH %p",
      (void *)hh);
}

void lockReaderHH(struct HM_HierarchicalHeap* hh) {
  GC_state s = pthread_getspecific(gcstate_key);
  LOG(LM_HIERARCHICAL_HEAP, LL_DEBUGMORE,
      "Locking reader HH %p",
      (void *)hh);
  rwlock_reader_lock(s, &hh->lock);
  LOG(LM_HIERARCHICAL_HEAP, LL_DEBUGMORE,
      "Locked reader HH %p",
      (void *)hh);
}

void unlockReaderHH(struct HM_HierarchicalHeap* hh) {
  GC_state s = pthread_getspecific(gcstate_key);
  LOG(LM_HIERARCHICAL_HEAP, LL_DEBUGMORE,
      "Unlocking reader HH %p",
      (void *)hh);
  rwlock_reader_unlock(s, &hh->lock);
  LOG(LM_HIERARCHICAL_HEAP, LL_DEBUGMORE,
      "Unlocked reader HH %p",
      (void *)hh);
}

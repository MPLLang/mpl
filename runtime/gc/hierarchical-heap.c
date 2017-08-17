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

  lockWriterHH(parentHH);
  lockWriterHH(childHH);

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
    /* off-by-one loop to prevent underflow and infinite loop */
    Word32 level;
    for (level = stealLevel;
         level > (oldHighestStolenLevel + 1);
         level--) {
      sizeDelta += HM_getLevelSize(parentHH->levelList, level);
    }
    sizeDelta += HM_getLevelSize(parentHH->levelList, level);

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

  unlockWriterHH(childHH);
  unlockWriterHH(parentHH);
}

size_t HM_HH_getLevel(pointer hhPointer) {
  GC_state s = pthread_getspecific (gcstate_key);

  objptr hhObjptr = pointerToObjptr (hhPointer, s->heap->start);
  const struct HM_HierarchicalHeap* hh = HM_HH_objptrToStruct(s, hhObjptr);

  return hh->level;
}

void HM_HH_mergeIntoParent(pointer hhPointer) {
  GC_state s = pthread_getspecific (gcstate_key);

  objptr hhObjptr = pointerToObjptr (hhPointer, s->heap->start);
  struct HM_HierarchicalHeap* hh = HM_HH_objptrToStruct(s, hhObjptr);

  /* wait until hh is dead */
  while (DEAD != hh->state) { }

  assert (BOGUS_OBJPTR != hh->parentHH);
  struct HM_HierarchicalHeap* parentHH = HM_HH_objptrToStruct(s, hh->parentHH);

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
  *cursor = hh->nextChildHH;

  /* update lcs and lchs */
  size_t oldLCHS = parentHH->locallyCollectibleHeapSize;
  double parentHHRatio = HM_HH_getLCRatio(parentHH);
  double hhRatio = HM_HH_getLCRatio(hh);

  Word64 sizeDelta = 0;
  /* off-by-one loop to prevent underflow and infinite loop */
  Word32 level;
  Word32 highestStolenLevel = HM_HH_getHighestStolenLevel(s, parentHH);
  for (level = hh->stealLevel;
       level > (highestStolenLevel + 1);
       level--) {;
    sizeDelta += HM_getLevelSize(parentHH->levelList, level);
  }
  sizeDelta += HM_getLevelSize(parentHH->levelList, level);

  LOG(LM_HIERARCHICAL_HEAP, LL_INFO,
      "hh (%p) locallyCollectibleSize %"PRIu64" + %"PRIu64" = %"PRIu64,
      ((void*)(parentHH)),
      parentHH->locallyCollectibleSize,
      sizeDelta,
      parentHH->locallyCollectibleSize + sizeDelta);
  parentHH->locallyCollectibleSize += sizeDelta;

  /* merge level lists */
  HM_mergeLevelList(&(parentHH->levelList), hh->levelList, parentHH, false);
  hh->levelList = NULL;

  LOG(LM_HIERARCHICAL_HEAP, LL_INFO,
      "hh (%p) locallyCollectibleSize %"PRIu64" + %"PRIu64" = %"PRIu64,
      ((void*)(parentHH)),
      parentHH->locallyCollectibleSize,
      hh->locallyCollectibleSize,
      parentHH->locallyCollectibleSize + hh->locallyCollectibleSize);
  parentHH->locallyCollectibleSize += hh->locallyCollectibleSize;
  LOG(LM_HIERARCHICAL_HEAP, LL_INFO,
      "merged hh %p into hh %p",
      ((void*)(hh)),
      ((void*)(parentHH)));

  double newRatio = (parentHHRatio < hhRatio) ? parentHHRatio : hhRatio;
  //adjustLCHS(s, parentHH, newRatio);
  parentHH->locallyCollectibleHeapSize += hh->locallyCollectibleHeapSize;
  LOG(LM_HIERARCHICAL_HEAP, LL_DEBUGMORE,
      "hh (%p) locallyCollectibleHeapSize %"PRIu64" -> %"PRIu64" (ratio %.2f)",
      ((void*)(parentHH)),
      oldLCHS,
      parentHH->locallyCollectibleHeapSize,
      newRatio);

  assertInvariants(s, parentHH, LIVE);
  /* don't assert hh here as it should be thrown away! */
  hh->state = MERGED;

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

  assert(HM_getHighestLevel(hh->levelList) <= hh->level);
  HM_promoteChunks(&(hh->levelList), hh->level);

  assertInvariants(s, hh, LIVE);

  unlockWriterHH(hh);
}

void HM_HH_setDead(pointer hhPointer) {
  GC_state s = pthread_getspecific (gcstate_key);

  objptr hhObjptr = pointerToObjptr (hhPointer, s->heap->start);
  struct HM_HierarchicalHeap* hh = HM_HH_objptrToStruct(s, hhObjptr);

  hh->state = DEAD;
}

void HM_HH_setLevel(pointer hhPointer, size_t level) {
  GC_state s = pthread_getspecific (gcstate_key);

  objptr hhObjptr = pointerToObjptr (hhPointer, s->heap->start);
  struct HM_HierarchicalHeap* hh = HM_HH_objptrToStruct(s, hhObjptr);

  hh->level = level;
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
           "\tlevelList = %p\n"
           "\tparentHH = "FMTOBJPTR"\n"
           "\tnextChildHH = "FMTOBJPTR"\n"
           "\tchildHHList= "FMTOBJPTR"\n",
           hh->lastAllocatedChunk,
           lockStatus,
           hh->level,
           hh->stealLevel,
           hh->id,
           hh->levelList,
           hh->parentHH,
           hh->nextChildHH,
           hh->childHHList);
}

void HM_HH_ensureNotEmpty(struct HM_HierarchicalHeap* hh) {
  if (NULL == hh->levelList) {
    assert(NULL == hh->lastAllocatedChunk);

    /* add in one chunk */
    if (!HM_HH_extend(hh, GC_HEAP_LIMIT_SLOP)) {
      die(__FILE__ ":%d: Ran out of space for Hierarchical Heap!", __LINE__);
    }
  }
}

/*
 * RAM_NOTE: Should maybe be split off into a levelList function to shared with
 * copyObject
 */
bool HM_HH_extend(struct HM_HierarchicalHeap* hh, size_t bytesRequested) {
  lockWriterHH(hh);

  Word32 level = HM_getHighestLevel(hh->levelList);
  void* chunk;

  assert((CHUNK_INVALID_LEVEL == level) || (hh->level >= level));

  if ((CHUNK_INVALID_LEVEL == level) || (hh->level > level)) {
    chunk = HM_allocateLevelHeadChunk(&(hh->levelList),
                                      bytesRequested,
                                      hh->level,
                                      hh);
  } else {
    chunk = HM_allocateChunk(hh->levelList, bytesRequested);
  }

  if (NULL == chunk) {
    unlockWriterHH(hh);
    return FALSE;
  }

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

void* HM_HH_getFrontier(const struct HM_HierarchicalHeap* hh) {
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

void* HM_HH_getLimit(const struct HM_HierarchicalHeap* hh) {
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

/* RAM_NOTE: should this be moved to local-heap.h? */
bool HM_HH_objptrInHierarchicalHeap(GC_state s, objptr candidateObjptr) {
  pointer candidatePointer = objptrToPointer (candidateObjptr, s->heap->start);
  return ChunkPool_pointerInChunkPool(candidatePointer);
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

  HM_updateLevelListPointers(hh->levelList, hh);
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
  } else if (newLCHS > s->controls->hhConfig.maxLCHS) {
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

  if ((NULL != hh->levelList) && (NULL != hh->lastAllocatedChunk)) {
    HM_assertChunkInLevelList(hh->levelList, hh->lastAllocatedChunk);
  } else {
    assert((NULL == hh->levelList) && (NULL == hh->lastAllocatedChunk));
  }
  HM_assertLevelListInvariants(hh->levelList, hh, hh->stealLevel, false);
  assert(((NULL == hh->levelList) && (NULL == hh->lastAllocatedChunk)) ||
         ((NULL != hh->levelList) && (NULL != hh->lastAllocatedChunk)));

  Word64 locallyCollectibleSize = 0;
  /* off-by-one loop to prevent underflow and infinite loop */
  Word32 level;
  for (level = hh->level;
       level > (HM_HH_getHighestStolenLevel(s, hh) + 1);
       level--) {
    locallyCollectibleSize += HM_getLevelSize(hh->levelList, level);
  }
  locallyCollectibleSize += HM_getLevelSize(hh->levelList, level);
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

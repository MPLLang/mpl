/* Copyright (C) 2014 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file local-heap.c
 *
 * @author Ram Raghunathan
 *
 * This file implements the local heap interface defined in
 * local-heap.h.
 */

#include "local-heap.h"

#include "heap-utils.h"

/************************/
/* Function Definitions */
/************************/
void HM_enterLocalHeap (GC_state s) {
  struct HM_HierarchicalHeap* hh = HM_HH_getCurrent(s);

  HM_HH_ensureNotEmpty(hh);
  s->frontier = HM_HH_getFrontier(hh);
  s->limitPlusSlop = HM_HH_getLimit(hh);
  s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
}

void HM_exitLocalHeap (GC_state s) {
  struct HM_HierarchicalHeap* hh = HM_HH_getCurrent(s);

  HM_HH_updateValues(hh, s->frontier);
}

void HM_ensureHierarchicalHeapAssurances(GC_state s,
                                         bool forceGC,
                                         size_t bytesRequested,
                                         bool ensureCurrentLevel) {
  size_t heapBytesFree = s->limitPlusSlop - s->frontier;
  bool extend = FALSE;
  bool growStack = FALSE;

  LOG(LM_GLOBAL_LOCAL_HEAP, LL_DEBUGMORE,
      "bytesRequested: %zu, hasHeapBytesFree: %zu",
      bytesRequested,
      heapBytesFree);

  if (Proc_threadInSection()) {
    LOG(LM_GLOBAL_LOCAL_HEAP, LL_DEBUG,
        "Entering Management Heap GC");
    HM_enterGlobalHeap();
    ENTER0(s);
    LEAVE0(s);
    HM_exitGlobalHeap();
    LOG(LM_GLOBAL_LOCAL_HEAP, LL_DEBUG,
        "Exiting Management Heap GC");
  }

  if (!invariantForMutatorStack(s)) {
    /* need to grow stack */
    bytesRequested +=
        sizeofStackWithHeader(s,
                              sizeofStackGrowReserved (s, getStackCurrent (s)));
    growStack = TRUE;
  }

  /* fetch after management heap GC to make sure that I get the updated value */
  struct HM_HierarchicalHeap* hh = HM_HH_getCurrent(s);

  /* update hh before modification */
  HM_HH_updateValues(hh, s->frontier);

  if (s->limitPlusSlop < s->frontier) {
    DIE("s->limitPlusSlop (%p) < s->frontier (%p)",
        ((void*)(s->limit)),
        ((void*)(s->frontier)));
  }

  double allocatedRatio = ((double)(hh->locallyCollectibleHeapSize)) /
                          ((double)(hh->locallyCollectibleSize));
  if (forceGC || (allocatedRatio < s->controls->hhConfig.allocatedRatio)) {
    /* too much allocated, so let's collect */
    HM_HHC_collectLocal();

    double newAllocatedRatio = ((double)(hh->locallyCollectibleHeapSize)) /
                               ((double)(hh->locallyCollectibleSize));

    LOG(LM_GLOBAL_LOCAL_HEAP, LL_INFO,
        "Live Ratio %.2f < %.2f, performed local collection to increase "
        "ratio to %.2f (%zu / %zu)",
        allocatedRatio,
        s->controls->hhConfig.allocatedRatio,
        newAllocatedRatio,
        hh->locallyCollectibleHeapSize,
        hh->locallyCollectibleSize);
    LOG(LM_GLOBAL_LOCAL_HEAP, LL_INFO,
        "%zu/%zu bytes allocated in Chunk Pool after collection",
        ChunkPool_allocated(),
        ChunkPool_size());

    HM_HH_maybeResizeLCHS(s, hh);

    /* I may have reached a new maxHHLCS, so check */
    if (s->cumulativeStatistics->maxHHLCS <
        hh->locallyCollectibleSize) {
      s->cumulativeStatistics->maxHHLCS =
          hh->locallyCollectibleSize;
    }

    /* I may have reached a new maxHHLHCS, so check */
    if (s->cumulativeStatistics->maxHHLCHS < hh->locallyCollectibleHeapSize) {
      s->cumulativeStatistics->maxHHLCHS = hh->locallyCollectibleHeapSize;
    }

    if (NULL == hh->lastAllocatedChunk) {
      /* collected everything! */
      s->frontier = ((pointer)(GC_HEAP_LIMIT_SLOP));
      s->limitPlusSlop = ((pointer)(GC_HEAP_LIMIT_SLOP));
      s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
    } else {
      s->frontier = HM_HH_getFrontier(hh);
      s->limitPlusSlop = HM_HH_getLimit(hh);
      s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
    }

    setGCStateCurrentThreadAndStack (s);
  }

  if (((size_t)(s->limitPlusSlop - s->frontier)) < bytesRequested) {
    /* not enough space */
    extend = TRUE;
  }

  if (ensureCurrentLevel) {
    /* extend if current allocation chunk is not at the current level */
    struct HM_ObjptrInfo info;
    LOCAL_USED_FOR_ASSERT bool retrieved =
        HM_getObjptrInfo(s,
                         pointerToObjptr(s->frontier, s->heap->start),
                         &info);
    assert(retrieved);

    assert(info.level <= hh->level);
    if (info.level < hh->level) {
      /* need to extend */
      LOG(LM_GLOBAL_LOCAL_HEAP, LL_DEBUG,
          "extending due to ensureCurrentLevel frontier level = %u hh level = "
          "%u",
          info.level,
          hh->level);
      extend = TRUE;
    }
  }

  if (extend) {
    /* Need to extend */
    if (!HM_HH_extend(hh, bytesRequested)) {
      if (LOG_EMABLED(LM_GLOBAL_LOCAL_HEAP, LL_DEBUGMORE)) {
        LOG(LM_GLOBAL_LOCAL_HEAP, LL_DEBUGMORE,
            "Ran out of space for HH %p:",
            ((void*)(hh)));

        LOG(LM_GLOBAL_LOCAL_HEAP, LL_DEBUGMORE,
            "  Level List:");
        for (void* levelHead = hh->levelList;
             NULL != levelHead;
             levelHead = getChunkInfo(levelHead)->split.levelHead.nextHead) {
          LOG(LM_GLOBAL_LOCAL_HEAP, LL_DEBUGMORE,
              "    level %u size %llu",
              getChunkInfo(levelHead)->level,
              getChunkInfo(levelHead)->split.levelHead.size);
        }

        LOG(LM_GLOBAL_LOCAL_HEAP, LL_DEBUGMORE,
            "  Child HH List:");
        for (struct HM_HierarchicalHeap* hhCursor = HM_HH_objptrToStruct(s, hh->childHHList);
             NULL != hhCursor;
             hhCursor = HM_HH_objptrToStruct(s, hhCursor->nextChildHH)) {
          LOG(LM_GLOBAL_LOCAL_HEAP, LL_DEBUGMORE,
              "    %p state %s level %u stealLevel %u LCS %llu",
              ((void*)(hhCursor)),
              HM_HHStateToString[hhCursor->state],
              hhCursor->level,
              hhCursor->stealLevel,
              hhCursor->locallyCollectibleSize);
        }
      }
      DIE("Ran out of space for Hierarchical Heap!");
    }

    s->frontier = HM_HH_getFrontier(hh);
    s->limitPlusSlop = HM_HH_getLimit(hh);
    s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
  }

  if (growStack) {
    LOG(LM_GLOBAL_LOCAL_HEAP, LL_DEBUG,
        "growing stack");
    growStackCurrent (s, FALSE);
    setGCStateCurrentThreadAndStack (s);
  }

  assert(invariantForMutatorFrontier (s));
  assert(invariantForMutatorStack (s));
}

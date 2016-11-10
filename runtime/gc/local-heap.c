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

  LOG(LM_GLOBAL_LOCAL_HEAP, LL_DEBUG,
      "bytesRequested: %zu, hasHeapBytesFree: %zu\n",
      bytesRequested,
      heapBytesFree);

  if (Proc_threadInSection()) {
    LOG(LM_GLOBAL_LOCAL_HEAP, LL_DEBUG,
        "Entering Management Heap GC\n");
    HM_enterGlobalHeap();
    ENTER0(s);
    LEAVE0(s);
    HM_exitGlobalHeap();
    LOG(LM_GLOBAL_LOCAL_HEAP, LL_DEBUG,
        "Exiting Management Heap GC\n");
  }

#pragma message "Needs to be revised with in-hh stacks"
  if (!invariantForMutatorStack(s)) {
    /* go to global GC to get the stack invariant satisfied */
    HM_enterGlobalHeap();
    ensureHasHeapBytesFreeAndOrInvariantForMutator (s, forceGC,
                                                    FALSE, TRUE,
                                                    0, 0);
    HM_exitGlobalHeap();
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
  }

  bool extend = FALSE;
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
      extend = TRUE;
    }
  }

  if (extend) {
    /* Need to extend */
    if (!HM_HH_extend(hh, bytesRequested)) {
      DIE("Ran out of space for Hierarchical Heap!");
    }

    s->frontier = HM_HH_getFrontier(hh);
    s->limitPlusSlop = HM_HH_getLimit(hh);
    s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
  }

  assert(invariantForMutatorFrontier (s));
  assert(invariantForMutatorStack (s));
}

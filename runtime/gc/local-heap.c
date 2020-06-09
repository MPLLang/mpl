/* Copyright (C) 2014 Ram Raghunathan.
 *
 * MLton is released under a HPND-style license.
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

/************************/
/* Function Definitions */
/************************/
void HM_enterLocalHeap (GC_state s) {
  GC_thread thread = getThreadCurrent(s);

  HM_HH_ensureNotEmpty(s, thread);
  s->frontier = HM_HH_getFrontier(thread);
  s->limitPlusSlop = HM_HH_getLimit(thread);
  s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
}

void HM_exitLocalHeap (GC_state s) {
  HM_HH_updateValues(getThreadCurrent(s), s->frontier);
}

void HM_ensureHierarchicalHeapAssurances(GC_state s,
                                         bool forceGC,
                                         size_t bytesRequested,
                                         bool ensureCurrentLevel) {

  size_t heapBytesFree = s->limitPlusSlop - s->frontier;
  // bool emptyHH = FALSE;
  // bool extend = FALSE;
  bool growStack = FALSE;
  size_t stackBytes = 0;

  LOG(LM_GLOBAL_LOCAL_HEAP, LL_DEBUGMORE,
      "bytesRequested: %zu, heapBytesFree: %zu",
      bytesRequested,
      heapBytesFree);

  // trace pre-collection occupancy before doing anything
  // SAM_NOTE: TODO: removed for now; will need to replace with blocks statistics
  // Trace2(EVENT_CHUNKP_OCCUPANCY, ChunkPool_size(), ChunkPool_allocated());

  if (!invariantForMutatorStack(s)) {
    /* need to grow stack */
    stackBytes =
        sizeofStackWithMetaData(s,
                                sizeofStackGrowReserved (s, getStackCurrent (s)));
    growStack = TRUE;
  }

  /* fetch after management heap GC to make sure that I get the updated value */
  GC_thread thread = getThreadCurrent(s);

  /* update hh before modification */
  HM_HH_updateValues(thread, s->frontier);

  if (s->limitPlusSlop < s->frontier) {
    DIE("s->limitPlusSlop (%p) < s->frontier (%p)",
        ((void*)(s->limit)),
        ((void*)(s->frontier)));
  }

  uint32_t desiredScope = HM_HH_desiredCollectionScope(s, thread);


  // CC_collectAtPublicLevel(s, thread, (desiredScope>0?desiredScope-1:0));
  // CC_collectAtPublicLevel(s, thread, (desiredScope>1?desiredScope-2:0));
  // CC_collectAtPublicLevel(s, thread, (desiredScope>2?desiredScope-3:0));

  if (forceGC || desiredScope <= thread->currentDepth) {
    /* too much allocated, so let's collect */
    for(int i= 2; i<desiredScope; i++) {
        CC_collectAtPublicLevel(s, thread, i);
    }
    HM_HHC_collectLocal(desiredScope, forceGC);

    /* post-collection, the thread might have been moved? */
    thread = getThreadCurrent(s);

    // SAM_NOTE: TODO: removed for now; will need to replace with blocks statistics
    // LOG(LM_GLOBAL_LOCAL_HEAP, LL_INFO,
    //     "%zu/%zu bytes allocated in Chunk Pool after collection",
    //     ChunkPool_allocated(),
    //     ChunkPool_size());

    /* trace post-collection occupancy */
    // SAM_NOTE: TODO: removed for now; will need to replace with blocks statistics
    // Trace2(EVENT_CHUNKP_OCCUPANCY, ChunkPool_size(), ChunkPool_allocated());

    /* I may have reached a new maxHHLCS, so check */
    // if (s->cumulativeStatistics->maxHHLCS < newSize) {
    //   s->cumulativeStatistics->maxHHLCS = newSize;
    // }

    /* I may have reached a new maxHHLHCS, so check */
    // if (s->cumulativeStatistics->maxHHLCHS < hh->collectionThreshold) {
    //   s->cumulativeStatistics->maxHHLCHS = hh->collectionThreshold;
    // }

    if (NULL == thread->currentChunk) {
      /* collected everything! */
      s->frontier = NULL;
      s->limitPlusSlop = NULL;
      s->limit = NULL;
      // emptyHH = TRUE;
    } else {
      /* SAM_NOTE: I don't use HM_HH_getFrontier/Limit here, because these have
       * assertions for the chunk frontier invariant, which might be violated
       * here. */
      s->frontier = HM_getChunkFrontier(thread->currentChunk);
      s->limitPlusSlop = HM_getChunkLimit(thread->currentChunk);
      s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
    }

    /* Thread/stack may have been copied during GC, so need to update */
    setGCStateCurrentThreadAndStack (s);
  }

  /* SAM_NOTE: TODO: clean this shit up */
  if (growStack) {
    LOG(LM_GLOBAL_LOCAL_HEAP, LL_DEBUG,
        "growing stack");
    if (NULL == thread->currentChunk ||
        (ensureCurrentLevel && HM_HH_getDepth(HM_getLevelHead(thread->currentChunk)) != thread->currentDepth) ||
        HM_getChunkFrontier(thread->currentChunk) >= (pointer)thread->currentChunk + HM_BLOCK_SIZE ||
        (size_t)(s->limitPlusSlop - s->frontier) < stackBytes)
    {
      if (!HM_HH_extend(s, thread, stackBytes)) {
        DIE("Ran out of space for Hierarchical Heap!");
      }
      s->frontier = HM_HH_getFrontier(thread);
      s->limitPlusSlop = HM_HH_getLimit(thread);
      s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
    }
    /* SAM_NOTE: growStackCurrent triggers a stack allocation which will
     * guarantee chunk frontier invariants
     */
    growStackCurrent(s);
    /* SAM_NOTE: growing the stack can edit s->frontier, so we need to make sure
     * the saved frontier in the hh is synced. */
    /* SAM_NOTE: TODO: caching the frontier in so many different places is a
     * major headache. We need a refactor. */
    assert(HM_getChunkOf(s->frontier) == thread->currentChunk);
    HM_HH_updateValues(thread, s->frontier);
    setGCStateCurrentThreadAndStack(s);
  }

  /* Determine if we need to extend to accommodate bytesRequested (and possibly
   * ensureCurrentLevel */
  if (NULL == thread->currentChunk ||
      (ensureCurrentLevel && HM_HH_getDepth(HM_getLevelHead(thread->currentChunk)) != thread->currentDepth) ||
      HM_getChunkFrontier(thread->currentChunk) >= (pointer)thread->currentChunk + HM_BLOCK_SIZE ||
      (size_t)(s->limitPlusSlop - s->frontier) < bytesRequested)
  {
    if (!HM_HH_extend(s, thread, bytesRequested)) {
      DIE("Ran out of space for Hierarchical Heap!");
    }
    s->frontier = HM_HH_getFrontier(thread);
    s->limitPlusSlop = HM_HH_getLimit(thread);
    s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
  }

  assert(invariantForMutatorFrontier (s));
  assert(invariantForMutatorStack (s));
}

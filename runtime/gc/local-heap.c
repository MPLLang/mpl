/* Copyright (C) 2020 Sam Westrick.
 * Copyright (C) 2014 Ram Raghunathan.
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

void HM_ensureHierarchicalHeapAssurances(
  GC_state s,
  bool forceGC,
  size_t bytesRequested,
  bool ensureCurrentLevel)
{
  size_t heapBytesFree = s->limitPlusSlop - s->frontier;
  bool growStack = FALSE;

  LOG(LM_GLOBAL_LOCAL_HEAP, LL_DEBUGMORE,
      "bytesRequested: %zu, heapBytesFree: %zu",
      bytesRequested,
      heapBytesFree);

  if (!invariantForMutatorStack(s)) {
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

  /* If we want to force a collection, then using a desired scope
   * of 1 will try to collect as much as possible. Otherwise, compute
   * a desired scope based on number of allocations performed. If we
   * haven't allocated too much, then the computed desired scope will
   * be larger than thread->currentDepth, indicating that we should
   * NOT collect right now.
   */
  uint32_t desiredScope = 1;
  if (!forceGC) desiredScope = HM_HH_desiredCollectionScope(s, thread);

  if (desiredScope <= thread->currentDepth) {
    /* too much allocated, so let's collect */
    for(uint32_t i = 2; i < desiredScope; i++) {
        CC_collectAtPublicLevel(s, thread, i);
    }

    HM_HHC_collectLocal(desiredScope);

    /* post-collection, the thread might have been moved? */
    thread = getThreadCurrent(s);

    if (NULL == thread->currentChunk) {
      /* collected everything! */
      s->frontier = NULL;
      s->limitPlusSlop = NULL;
      s->limit = NULL;
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

  if (growStack) {
    LOG(LM_GLOBAL_LOCAL_HEAP, LL_DEBUG, "growing stack");
#if ASSERT
    assert(NULL == s->frontier || HM_getChunkOf(s->frontier) == thread->currentChunk);
    pointer frontierBefore = s->frontier;
#endif
    growStackCurrent(s);
#if ASSERT
    assert(NULL == s->frontier || HM_getChunkOf(s->frontier) == thread->currentChunk);
    assert(s->frontier == frontierBefore);
#endif
    setGCStateCurrentThreadAndStack(s);
  }

  /* Determine if we need to extend to accommodate bytesRequested (and possibly
   * ensureCurrentLevel */
  /* SAM_NOTE: TODO: clean this shit up */
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

#if ASSERT
  for (HM_HierarchicalHeap cursor = thread->hierarchicalHeap;
       NULL != cursor;
       cursor = cursor->nextAncestor)
  {
    HM_assertChunkListInvariants(HM_HH_getChunkList(cursor));
  }
#endif

  assert(invariantForMutatorFrontier (s));
  assert(invariantForMutatorStack (s));
}

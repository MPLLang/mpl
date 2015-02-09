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
  const struct HM_HierarchicalHeap* hh = HM_getCurrentHierarchicalHeap(s);

  s->frontier = HM_getHierarchicalHeapSavedFrontier(hh);
  s->limitPlusSlop = HM_getHierarchicalHeapLimit(hh);
  s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
}

void HM_exitLocalHeap (GC_state s) {
  struct HM_HierarchicalHeap* hh = HM_getCurrentHierarchicalHeap(s);

  HM_setHierarchicalHeapSavedFrontier(hh, s->frontier);
}

void HM_ensureHierarchicalHeapAssurances(GC_state s,
                                         bool forceGC,
                                         size_t bytesRequested) {
  assert (bytesRequested >= GC_HEAP_LIMIT_SLOP);

  int processor = s->procStates ? Proc_processorNumber (s) : -1;
  size_t heapBytesFree = s->limitPlusSlop - s->frontier;

  HM_debugMessage(s,
                  "[%d] HM_ensureHierarchicalHeapAssurances(): bytesRequested: "
                  "%zu, hasHeapBytesFree: %zu\n",
                  processor,
                  bytesRequested,
                  heapBytesFree);

  if (Proc_threadInSection()) {
    HM_debugMessage(s,
                    "[%d] HM_ensureHierarchicalHeapAssurances(): Entering "
                    "Management Heap GC\n",
                    processor);
    HM_enterGlobalHeap();
    ENTER0(s);
    LEAVE0(s);
    HM_exitGlobalHeap();
    HM_debugMessage(s,
                    "[%d] HM_ensureHierarchicalHeapAssurances(): Exiting "
                    "Management Heap GC\n",
                    processor);
  }

  if (!invariantForMutatorStack(s)) {
    /* go to global GC to get the stack invariant satisfied */
    HM_enterGlobalHeap();
    ensureHasHeapBytesFreeAndOrInvariantForMutator (s, forceGC,
                                                    FALSE, TRUE,
                                                    0, 0);
    HM_exitGlobalHeap();
  }

  /* fetch after management heap GC to make sure that I get the updated value */
  struct HM_HierarchicalHeap* hh = HM_getCurrentHierarchicalHeap(s);

  if (s->limitPlusSlop < s->frontier) {
    die(__FILE__ ":%d: s->limitPlusSlop (%p) < s->frontier (%p)",
        __LINE__,
        ((void*)(s->limit)),
        ((void*)(s->frontier)));
  }

  if (((size_t)(s->limitPlusSlop - s->frontier)) < bytesRequested) {
    /* Not enough space, so add new chunk */
    if (!HM_extendHierarchicalHeap(hh, bytesRequested)) {
      die(__FILE__ ":%d: Ran out of space for Hierarchical Heap!", __LINE__);
    }

    s->frontier = HM_getHierarchicalHeapSavedFrontier(hh);
    s->limitPlusSlop = HM_getHierarchicalHeapLimit(hh);
    s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
  }

  assert(invariantForMutatorFrontier (s));
  assert(invariantForMutatorStack (s));
}

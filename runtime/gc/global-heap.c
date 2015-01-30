/* Copyright (C) 2014 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file global-heap.c
 *
 * @author Ram Raghunathan
 *
 * This file implements the Global Heap interface defined in
 * global-heap.h.
 */

#include "global-heap.h"

#include "heap-utils.h"

/********************/
/* Static Variables */
/********************/
static bool HM_mltonParallelCalled = FALSE;

/************************/
/* Function Definitions */
/************************/
void HM_enterGlobalHeap (bool fromMLtonParallel) {
  GC_state s = pthread_getspecific (gcstate_key);
  if (NULL == s) {
    /* initialization not finished, so nothing to do yet */
    return;
  }

  GC_thread currentThread = getThreadCurrent (s);

  if (!HM_mltonParallelCalled) {
    if (fromMLtonParallel) {
      /*
       * This is the first call from MLton.Parallel, so we let it slide (nothing
       * to do). The next calls will perform the actions
       */
      HM_mltonParallelCalled = TRUE;
    }

    /**
     * On calls before MLton.Parallel called, the thread is already set
     * inGlobalHeap = 1 so nothing to do. Subsequent threads are created in
     * MLton.Parallel so those will only call exitGlobalHeap() instead of
     * enterGlobalHeap()
     */
    assert(1 == currentThread->inGlobalHeapCounter);
    return;
  }

  if ((~((size_t)(0))) == currentThread->inGlobalHeapCounter) {
    die(__FILE__ ":%d: currentThread->inGlobalHeapCounter about to overflow!",
        __LINE__);
  }
  currentThread->inGlobalHeapCounter++;

  if (1 == currentThread->inGlobalHeapCounter) {
    assert (NULL != s->globalFrontier);
    assert (NULL != s->globalLimitPlusSlop);

    HM_exitLocalHeap (s);

    s->frontier = s->globalFrontier;
    s->limitPlusSlop = s->globalLimitPlusSlop;
    s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
  }
}

void HM_exitGlobalHeap (bool fromMLtonParallel) {
  GC_state s = pthread_getspecific (gcstate_key);
  if (NULL == s) {
    /* initialization not finished, so nothing to do yet */
    return;
  }

  GC_thread currentThread = getThreadCurrent (s);

  if (!HM_mltonParallelCalled) {
    /* HM_enterGlobalHeap() should have been called first! */
    assert(!fromMLtonParallel);
    assert(1 == currentThread->inGlobalHeapCounter);
    return;
  }

  assert (currentThread->inGlobalHeapCounter > 0);
  currentThread->inGlobalHeapCounter--;
  if (0 == currentThread->inGlobalHeapCounter) {
    s->globalFrontier = s->frontier;
    s->globalLimitPlusSlop = s->limitPlusSlop;

    HM_enterLocalHeap (s);
  }
}

#if (defined (MLTON_GC_INTERNAL_FUNCS))
bool HM_inGlobalHeap (GC_state s) {
  GC_thread currentThread = getThreadCurrent (s);

  return (0 != currentThread->inGlobalHeapCounter);
}
#endif /* MLTON_GC_INTERNAL_FUNCS */

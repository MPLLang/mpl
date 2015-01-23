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

/************************/
/* Function Definitions */
/************************/
void HM_enterGlobalHeap (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  GC_thread currentThread = getThreadCurrent (s);

  if ((~((size_t)(0))) == currentThread->inGlobalHeapCounter) {
    die(__FILE__ ":%d: currentThread->inGlobalHeapCounter about to overflow!",
        __LINE__);
  }
  currentThread->inGlobalHeapCounter++;

  if (1 == currentThread->inGlobalHeapCounter) {
    assert (NULL != s->globalFrontier);
    assert (NULL != s->globalLimit);

    HM_debugMessage(s, "Entering Global Heap\n");

    HM_exitLocalHeap (s);

    s->frontier = s->globalFrontier;
    s->limit = s->globalLimit;
  }
}

void HM_exitGlobalHeap (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  GC_thread currentThread = getThreadCurrent (s);

  assert (currentThread->inGlobalHeapCounter > 0);
  currentThread->inGlobalHeapCounter--;
  if (0 == currentThread->inGlobalHeapCounter) {
    HM_debugMessage(s, "Exiting Global Heap\n");

    s->globalFrontier = s->frontier;
    s->globalLimit = s->limit;

    HM_enterLocalHeap (s);
  }
}

#if (defined (MLTON_GC_INTERNAL_FUNCS))
bool HM_inGlobalHeap (GC_state s) {
  GC_thread currentThread = getThreadCurrent (s);

  return (0 != currentThread->inGlobalHeapCounter);
}
#endif /* MLTON_GC_INTERNAL_FUNCS */

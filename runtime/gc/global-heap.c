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

#pragma message "Insert overflow check?"
  currentThread->inGlobalHeapCounter++;
  if (1 == currentThread->inGlobalHeapCounter) {
    assert (NULL != s->globalFrontier);
    assert (NULL != s->globalLimit);

    HM_debugMessage(s, "Entering Global Heap\n");

    HM_exitLocalHeap (s);
#pragma message "Activate when ready"
#if 0
    s->frontier = s->globalFrontier;
    s->limit = s->globalLimit;
#endif
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

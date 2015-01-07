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

/******************************/
/* Static Function Prototypes */
/******************************/
static pointer HeapManagement_calculateLimitFromChunk (void* chunk);

/************************/
/* Function Definitions */
/************************/
void HeapManagement_enterLocalHeap (GC_state s) {
  #if 0
  GC_thread currentThread = getThreadCurrent (s);

  if (NULL != currentThread->lastAllocatedChunk) {
    assert (NULL != currentThread->heapHead);
    assert (NULL != currentThread->frontier);

    s->frontier = currentThread->frontier;
    s->limit = HeapManagement_calculateLimitFromChunk (
        currentThread->lastAllocatedChunk);
  }
#endif
}

void HeapManagement_exitLocalHeap (GC_state s) {
  GC_thread currentThread = getThreadCurrent (s);

  currentThread->frontier = s->frontier;
}

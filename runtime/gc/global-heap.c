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

#include global-heap.h

/******************************/
/* Static Function Prototypes */
/******************************/
static void HeapManagement_debugMessage (GC_state s, const char* format, ...)
    __attribute__((format (printf, 2, 3)));

/************************/
/* Function Definitions */
/************************/
void GC_enterGlobalHeap (void) {
  GC_state s = pthread_getspecific (gcstate_key);

  if (NULL != s->globalFrontier) {
    assert (NULL != s->globalLimit);

    HeapManagement_debugMessage(
        s,
        "Entering Global Heap: frontier = %p --> %p, limit = %p --> %p\n",
        s->frontier,
        s->globalFrontier,
        s->limit,
        s->globalLimit);

    s->frontier = s->globalFrontier;
    s->limit = s->globalLimit;
  }
  /*
   * else this is the first call, which means we are already in the global heap
   * as we start the process in it
   */
#if ASSERT
  else {
    assert(NULL == s->globalLimit);
    HeapManagement_debugMessage(
        s,
        "Entering Global Heap for first time: frontier = %p, limit = %p\n",
        s->frontier,
        s->limit);
  }
#endif
}

void GC_exitGlobalHeap (pointer headArgument,
                        pointer lastAllocatedChunkArgument) {
  GC_state s = pthread_getspecific (gcstate_key);
  struct HeapManagement_TaskHeap taskHeap = {
    .head = ((void**)(headArgument)),
    .lastAllocatedChunk = ((void**)(lastAllocatedChunkArgument))
  };

  HeapManagement_debugMessage(
      s,
      "Exiting Global Heap: frontier = %p --> %p, limit = %p --> %p\n",
      s->frontier,
      s->globalFrontier,
      s->limit,
      s->globalLimit);

  s->globalFrontier = s->frontier;
  s->globalLimit = s->limit;

  HeapManagement_setLocalHeapFrontierAndLimit(s, &taskHeap);
}

void HeapManagement_debugMessage (GC_state s, const char* format, ...) {
  if (DEBUG_HEAP_MANAGEMENT or s->controls->heapManagementMessages) {
    va_list substitutions;
    va_start(substitutions, format);
    vfprintf (stderr, format, substitutions);
    va_end(substitutions);
  }
}

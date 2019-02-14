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
#if (defined (MLTON_GC_INTERNAL_BASIS))
void HM_enterGlobalHeap (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  if (NULL == s) {
    /* initialization not finished, so nothing to do yet */
    return;
  }

  GC_thread currentThread = getThreadCurrent (s);

  if ((~((Word32)(0))) == currentThread->inGlobalHeapCounter) {
    die(__FILE__ ":%d: currentThread->inGlobalHeapCounter about to overflow!",
        __LINE__);
  }
  currentThread->inGlobalHeapCounter++;

  if (currentThread->hierarchicalHeap == NULL) {
    /* This thread is not to use hierarchical heaps, so it is a noop */
    return;
  }

  if (1 == currentThread->inGlobalHeapCounter) {
    HM_exitLocalHeap(s);
    HM_chunk chunk = HM_getChunkListLastChunk(s->globalHeap);
    assert(chunk != NULL);
    s->frontier = HM_getChunkFrontier(chunk);
    s->limitPlusSlop = HM_getChunkLimit(chunk);
    s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
  }
}

void HM_exitGlobalHeap (void) {
  GC_state s = pthread_getspecific (gcstate_key);
  if (NULL == s) {
    /* initialization not finished, so nothing to do yet */
    return;
  }

  GC_thread currentThread = getThreadCurrent (s);

  assert (currentThread->inGlobalHeapCounter > 0);
  currentThread->inGlobalHeapCounter--;

  if (currentThread->hierarchicalHeap == NULL) {
    /* This thread is not to use hierarchical heaps, so it is a noop */
    return;
  }

  if (0 == currentThread->inGlobalHeapCounter) {
    HM_chunk chunk = HM_getChunkListLastChunk(s->globalHeap);
    HM_updateChunkValues(chunk, s->frontier);
    HM_enterLocalHeap(s);
  }
}

#endif /* MLTON_GC_INTERNAL_BASIS */

#if (defined (MLTON_GC_INTERNAL_FUNCS))
bool HM_inGlobalHeap (GC_state s) {
  if (BOGUS_OBJPTR == getThreadCurrentObjptr(s)) {
    return TRUE;
  }

  GC_thread currentThread = getThreadCurrent (s);

  bool answer =
    (currentThread->hierarchicalHeap == NULL) ||
    (0 != currentThread->inGlobalHeapCounter);

#if ASSERT
  if (answer) {
    HM_chunk chunk = HM_getChunkListLastChunk(s->globalHeap);
    assert(chunk != NULL);
    assert(HM_getChunkStart(chunk) <= s->frontier);
    assert(inFirstBlockOfChunk(chunk, s->frontier));
  }
#endif

  return answer;
}
#endif /* MLTON_GC_INTERNAL_FUNCS */

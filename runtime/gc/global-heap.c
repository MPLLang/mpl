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

  if (!currentThread->useHierarchicalHeap) {
    /* This thread is not to use hierarchical heaps, so it is a noop */
    return;
  }

  if (1 == currentThread->inGlobalHeapCounter) {
    assert (NULL != s->globalFrontier);
    assert (NULL != s->globalLimitPlusSlop);
    HM_exitLocalHeap (s);

    spinlock_lock(&(s->lock), Proc_processorNumber(s));
    s->frontier = s->globalFrontier;
    s->limitPlusSlop = s->globalLimitPlusSlop;
    s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
    spinlock_unlock(&(s->lock));
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

  if (!currentThread->useHierarchicalHeap) {
    /* This thread is not to use hierarchical heaps, so it is a noop */
    return;
  }

  if (0 == currentThread->inGlobalHeapCounter) {
    spinlock_lock(&(s->lock), Proc_processorNumber(s));
    s->globalFrontier = s->frontier;
    s->globalLimitPlusSlop = s->limitPlusSlop;
    HM_enterLocalHeap (s);
    spinlock_unlock(&(s->lock));
  }
}

void HM_explicitEnterGlobalHeap(Word32 inGlobalHeapCounter) {
  GC_state s = pthread_getspecific (gcstate_key);
  assert(NULL != s);
  assert(NULL != s->globalFrontier);
  assert(NULL != s->globalLimitPlusSlop);

  /* update frontier and limit */
  HM_exitLocalHeap (s);
  s->frontier = s->globalFrontier;
  s->limitPlusSlop = s->globalLimitPlusSlop;
  s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;

  /* restore inGlobalHeapCounter */
  getThreadCurrent(s)->inGlobalHeapCounter = inGlobalHeapCounter;
  LOG(LM_GLOBAL_LOCAL_HEAP, LL_DEBUG,
      "inGlobalHeapCounter = %d",
      inGlobalHeapCounter);
}

Word32 HM_explicitExitGlobalHeap(void) {
  GC_state s = pthread_getspecific (gcstate_key);

  assert(NULL != s);
  assert(HM_inGlobalHeap(s));

  /* update frontier and limit */
  s->globalFrontier = s->frontier;
  s->globalLimitPlusSlop = s->limitPlusSlop;
  HM_enterLocalHeap (s);

  /* set inGlobalHeapCounter to zero */
  GC_thread thread = getThreadCurrent(s);
  Word32 retVal = thread->inGlobalHeapCounter;
  thread->inGlobalHeapCounter = 0;

  if (0 == retVal) {
    DIE("Attempted to exit while GHC is zero!");
  }

  LOG(LM_GLOBAL_LOCAL_HEAP, LL_DEBUG,
      "inGlobalHeapCounter = %d",
      retVal);

  /* return the old inGlobalHeapCounter */
  return retVal;
}
#endif /* MLTON_GC_INTERNAL_BASIS */

#if (defined (MLTON_GC_INTERNAL_FUNCS))
bool HM_inGlobalHeap (GC_state s) {
  if (BOGUS_OBJPTR == getThreadCurrentObjptr(s)) {
    return TRUE;
  }

  GC_thread currentThread = getThreadCurrent (s);

  return (!currentThread->useHierarchicalHeap ||
          (0 != currentThread->inGlobalHeapCounter));
}
#endif /* MLTON_GC_INTERNAL_FUNCS */

/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

void switchToThread(GC_state s, objptr op) {
  GC_thread thread = (GC_thread)(objptrToPointer(op, NULL) + offsetofThread(s));

  assert(thread->hierarchicalHeap != NULL);

  size_t terminateCheckCounter = 0;
  int otherProcNum = atomicLoadS32(&(thread->currentProcNum));
  while (otherProcNum >= 0) {
    /* Spin while someone else is currently executing this thread. The
     * termination checks happen rarely, and reset terminateCheckCounter to 0
     * when they do. */
    GC_MayTerminateThreadRarely(s, &terminateCheckCounter);
    if (terminateCheckCounter == 0) sched_yield();
    // Sanity check: don't get deadlocked by self
    assert(otherProcNum != s->procNumber);
    otherProcNum = atomicLoadS32(&(thread->currentProcNum));
  }
  thread->currentProcNum = s->procNumber;

  if (DEBUG_THREADS) {
    // GC_thread thread;
    GC_stack stack;

    // thread = (GC_thread)(objptrToPointer (op, NULL)
    //                      + offsetofThread (s));
    stack = (GC_stack)(objptrToPointer (thread->stack, NULL));

    fprintf (stderr, "switchToThread ("FMTOBJPTR")  used = %"PRIuMAX
             "  reserved = %"PRIuMAX"\n",
             op, (uintmax_t)stack->used, (uintmax_t)stack->reserved);
  }

  s->currentThread = op;
  setGCStateCurrentThreadAndStack (s);
}

void GC_switchToThread (GC_state s, pointer p, size_t ensureBytesFree) {
  enter(s);
  objptr currop = getThreadCurrentObjptr(s);
  // GC_thread currThread = threadObjptrToStruct(s, currop);
  LOG(LM_THREAD, LL_DEBUG,
    "GC_switchToThread current = %p, p = %p, ensureBytesFree = %zu",
    (void*)objptrToPointer(currop, NULL),
    (void*)p,
    ensureBytesFree);

  // printf("[%d] GC_switchToThread\n  from "FMTPTR"\n  to   "FMTPTR"\n",
  //   s->procNumber,
  //   (uintptr_t)(void*)objptrToPointer(currop, NULL),
  //   (uintptr_t)(void*)p);

  GC_thread oldCurrentThread = getThreadCurrent(s);

  assert(HM_HH_getDepth(oldCurrentThread->hierarchicalHeap) <= oldCurrentThread->currentDepth);

  //ENTER1 (s, p);
  /* SPOONHOWER_NOTE: copied from enter() */
  /* used needs to be set because the mutator has changed s->stackTop. */
  // getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed(s);
  // oldCurrentThread->exnStack = s->exnStack;
  // beginAtomic(s);

  // assert(threadAndHeapOkay(s));
  // HM_HH_updateValues(oldCurrentThread, s->frontier);
  s->frontier = 0;
  s->limitPlusSlop = 0;
  s->limit = 0;

  oldCurrentThread->bytesNeeded = ensureBytesFree;

  s->currentThread = BOGUS_OBJPTR;
  /* SAM_NOTE: This write synchronizes with the spinloop in switchToThread (above) */
  atomicStoreS32(&(oldCurrentThread->currentProcNum), -1);

  // printf("[%d] switchToThread\n  from %p\n    to %p\n",
  //   s->procNumber,
  //   (void*)oldCurrentThread,
  //   (void*)p);

  switchToThread(s, pointerToObjptr(p, NULL));
  /* SAM_NOTE: TODO: do signal handlers work properly? */
  s->atomicState--;
  switchToSignalHandlerThreadIfNonAtomicAndSignalPending(s);

  assert(threadAndHeapOkay(s));
  s->frontier = HM_HH_getFrontier(getThreadCurrent(s));
  s->limitPlusSlop = HM_HH_getLimit(getThreadCurrent(s));
  s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
  HM_ensureHierarchicalHeapAssurances(s, FALSE, getThreadCurrent(s)->bytesNeeded, FALSE);

  LOG(LM_THREAD, LL_DEBUG,
    "GC_switchToThread succeeded: old current = %p, new = %p",
    (void*)objptrToPointer(currop, NULL),
    (void*)objptrToPointer(getThreadCurrentObjptr(s), NULL));

  leave(s);
  // endAtomic (s);
  // assert(invariantForMutatorStack(s));
  //LEAVE0 (s);
}

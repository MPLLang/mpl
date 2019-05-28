/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void switchToThread(GC_state s, objptr op) {
  GC_thread thread = (GC_thread)(objptrToPointer(op, NULL) + offsetofThread(s));

  assert(thread->hierarchicalHeap != NULL);

  size_t terminateCheckCounter = 0;
  while (atomicLoadS32(&(thread->currentProcNum)) >= 0) {
    /* Spin while someone else is currently executing this thread. The
     * termination checks happen rarely, and reset terminateCheckCounter to 0
     * when they do. */
    GC_MayTerminateThreadRarely(s, &terminateCheckCounter);
    if (terminateCheckCounter == 0) pthread_yield();
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
  objptr currop = getThreadCurrentObjptr(s);
  // GC_thread currThread = threadObjptrToStruct(s, currop);
  LOG(LM_THREAD, LL_DEBUG,
    "GC_switchToThread current = %p, p = %p, ensureBytesFree = %zu",
    (void*)objptrToPointer(currop, NULL),
    (void*)p,
    ensureBytesFree);

  //ENTER1 (s, p);
  /* SPOONHOWER_NOTE: copied from enter() */
  /* used needs to be set because the mutator has changed s->stackTop. */
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed(s);
  getThreadCurrent(s)->exnStack = s->exnStack;
  beginAtomic(s);

  assert(threadAndHeapOkay(s));
  struct HM_HierarchicalHeap *hh = getThreadCurrent(s)->hierarchicalHeap;
  HM_HH_updateValues(hh, s->frontier);
  s->frontier = 0;
  s->limitPlusSlop = 0;
  s->limit = 0;

  getThreadCurrent(s)->bytesNeeded = ensureBytesFree;

  /* SAM_NOTE: This write synchronizes with the spinloop in switchToThread (above) */
  atomicStoreS32(&(getThreadCurrent(s)->currentProcNum), -1);

  switchToThread(s, pointerToObjptr(p, NULL));
  /* SAM_NOTE: TODO: do signal handlers work properly? */
  s->atomicState--;
  switchToSignalHandlerThreadIfNonAtomicAndSignalPending(s);

  assert(threadAndHeapOkay(s));
  hh = getThreadCurrent(s)->hierarchicalHeap;
  s->frontier = HM_HH_getFrontier(hh);
  s->limitPlusSlop = HM_HH_getLimit(hh);
  s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;
  HM_ensureHierarchicalHeapAssurances(s, FALSE, getThreadCurrent(s)->bytesNeeded, FALSE);

  endAtomic (s);
  assert(strongInvariantForMutatorFrontier(s));
  assert(invariantForMutatorStack(s));
  //LEAVE0 (s);
}

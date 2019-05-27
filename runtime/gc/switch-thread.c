/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void switchToThread (GC_state s, objptr op) {
  GC_thread thread = (GC_thread)(objptrToPointer (op, NULL) + offsetofThread (s));

  assert(thread->hierarchicalHeap != NULL);

  size_t terminateCheckCounter = 0;
  while (thread->currentProcNum >= 0) {
    /* spin while someone else is currently executing this thread */
    GC_MayTerminateThreadRarely(s, &terminateCheckCounter);
    pthread_yield();
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
  beginAtomic (s);

  assert(!HM_inGlobalHeap(s));
  HM_exitLocalHeap(s); // remembers s->frontier in HH
  s->frontier = 0;
  s->limitPlusSlop = 0;
  s->limit = 0;
  // HM_chunk globalHeapChunk = HM_getChunkListLastChunk(s->globalHeap);
  // assert(globalHeapChunk != NULL);
  // s->frontier = HM_getChunkFrontier(globalHeapChunk);
  // s->limitPlusSlop = HM_getChunkLimit(globalHeapChunk);
  // s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;

  getThreadCurrent(s)->bytesNeeded = ensureBytesFree;

  /* SAM_NOTE: CHECK: is it necessary to synchronize after the write to
   * currentProcNum? I want this write to synchronize with the spinloop in
   * switchToThread, above. */
  getThreadCurrent(s)->currentProcNum = -1;
  __sync_synchronize();

  switchToThread(s, pointerToObjptr(p, NULL));

  assert(!HM_inGlobalHeap(s));

  /* SAM_NOTE: this does an ensureNotEmpty, but really we should just ensure
   * getThreadCurrent(s)->bytesNeeded? */
  HM_enterLocalHeap(s);

  /* SAM_NOTE: TODO: do signal handlers work properly? */
  s->atomicState--;
  switchToSignalHandlerThreadIfNonAtomicAndSignalPending(s);

  /* SAM_NOTE: Shouldn't this be getThreadCurrent(s)->bytesNeeded? */
  HM_ensureHierarchicalHeapAssurances (s, FALSE, 0, FALSE);

  endAtomic (s);
  assert(strongInvariantForMutatorFrontier(s));
  assert(invariantForMutatorStack(s));
  //LEAVE0 (s);
}

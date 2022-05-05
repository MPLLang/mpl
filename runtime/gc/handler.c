/* Copyright (C) 2019 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/* GC_startSignalHandler does not do an enter()/leave(), even though
 * it is exported.  The basis library uses it via _import, not _prim,
 * and so does not treat it as a runtime call -- so the invariant in
 * enter would fail miserably.  It is OK because GC_startHandler must
 * be called from within a critical section.
 *
 * Don't make it inline, because it is also called in basis/Thread.c,
 * and when compiling with COMPILE_FAST, they may appear out of order.
 */
void GC_startSignalHandler (GC_state s) {
  /* Switch to the signal handler thread. */
  if (DEBUG_SIGNALS) {
    fprintf (stderr, "GC_startSignalHandler [%d]\n",
             Proc_processorNumber (s));
  }
  assert (s->atomicState == 1);
  assert (s->signalsInfo.signalIsPending);
  s->signalsInfo.signalIsPending = FALSE;
  s->signalsInfo.amInSignalHandler = TRUE;
  assert (s->savedThread == BOGUS_OBJPTR);
  s->savedThread = s->currentThread;
  /* Set s->atomicState to 2 when switching to the signal handler
   * thread; leaving the runtime will decrement s->atomicState to 1,
   * the signal handler will then run atomically and will finish by
   * switching to the thread to continue with, which will decrement
   * s->atomicState to 0.
   */
  s->atomicState = 2;
}

void GC_finishSignalHandler (GC_state s) {
  if (DEBUG_SIGNALS)
    fprintf (stderr, "GC_finishSignalHandler () [%d]\n",
             Proc_processorNumber (s));
  assert (s->atomicState == 1);
  s->signalsInfo.amInSignalHandler = FALSE;
}

void switchToSignalHandlerThreadIfNonAtomicAndSignalPending (GC_state s) {
  if (s->atomicState == 1
      and s->signalsInfo.signalIsPending) {
    // printf("switchToSignalHandlerThread triggered...\n");
    GC_startSignalHandler (s);

    // SAM_NOTE: synchronizes with loop in switchToThread...
    atomicStoreS32(&(getThreadCurrent(s)->currentProcNum), -1);
    s->currentThread = BOGUS_OBJPTR;

    switchToThread (s, s->signalHandlerThread);
  }
}

/* GC_handler sets s->limit = 0 so that the next limit check will
 * fail.  Signals need to be blocked during the handler (i.e. it
 * should run atomically) because sigaddset does both a read and a
 * write of s->signalsInfo.signalsPending.  The signals are blocked
 * by Posix_Signal_handle (see Posix/Signal/Signal.c).
 */
void GC_handler (int signum) {
  GC_state s = MLton_gcState ();
  if (DEBUG_SIGNALS)
    fprintf (stderr, "GC_handler signum = %d [%d]\n", signum,
             Proc_processorNumber (s));
  // Signal disposition is per-process; use primary to maintain handled set.
  assert (sigismember (&s->procStates[0].signalsInfo.signalsHandled, signum));
  if (s->atomicState == 0)
    s->limit = 0;
  s->signalsInfo.signalIsPending = TRUE;
  sigaddset (&s->signalsInfo.signalsPending, signum);
  if (DEBUG_SIGNALS)
    fprintf (stderr, "GC_handler done [%d]\n",
             Proc_processorNumber (s));
}

pointer GC_handlerEnterHeapOfThread(GC_state s, objptr threadp) {
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed(s);
  getThreadCurrent(s)->exnStack = s->exnStack;
  HM_HH_updateValues(getThreadCurrent(s), s->frontier);
  HH_EBR_leaveQuiescentState(s);

  GC_thread target = threadObjptrToStruct(s, threadp);
  assert(getThreadCurrent(s)->currentDepth == 0);
  assert(target->currentProcNum == -1);

  HM_HierarchicalHeap abandonedHH = getThreadCurrent(s)->hierarchicalHeap;

  // copy thread details to current thread
  getThreadCurrent(s)->currentDepth = target->currentDepth;
  getThreadCurrent(s)->currentChunk = target->currentChunk;
  getThreadCurrent(s)->hierarchicalHeap = target->hierarchicalHeap;
  getThreadCurrent(s)->bytesAllocatedSinceLastCollection =
    target->bytesAllocatedSinceLastCollection;
  getThreadCurrent(s)->bytesSurvivedLastCollection =
    target->bytesSurvivedLastCollection;
  getThreadCurrent(s)->minLocalCollectionDepth = target->minLocalCollectionDepth;
  getThreadCurrent(s)->decheckState = target->decheckState;

  uint32_t* fromSyncDepths = &(target->decheckSyncDepths[0]);
  uint32_t* toSyncDepths = &(getThreadCurrent(s)->decheckSyncDepths[0]);
  memcpy(toSyncDepths, fromSyncDepths, DECHECK_DEPTHS_LEN * sizeof(uint32_t));

  // clear out some stuff for my sanity
  target->currentChunk = NULL;
  target->hierarchicalHeap = NULL;
  target->bytesAllocatedSinceLastCollection = 0;
  target->bytesSurvivedLastCollection = 0;
  target->decheckState = DECHECK_BOGUS_TID;

  s->frontier = HM_HH_getFrontier(getThreadCurrent(s));
  s->limitPlusSlop = HM_HH_getLimit(getThreadCurrent(s));
  s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;

  HM_ensureHierarchicalHeapAssurances(
    s,
    FALSE,
    getThreadCurrent(s)->bytesNeeded,
    FALSE);

  return (void*)abandonedHH;
}


void GC_handlerLeaveHeapOfThread(
  GC_state s,
  objptr threadp,
  pointer abandonedHH)
{
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed(s);
  getThreadCurrent(s)->exnStack = s->exnStack;
  HM_HH_updateValues(getThreadCurrent(s), s->frontier);
  HH_EBR_leaveQuiescentState(s);

  GC_thread target = threadObjptrToStruct(s, threadp);
  assert(target->currentProcNum == -1);

  target->currentDepth = getThreadCurrent(s)->currentDepth;
  target->currentChunk = getThreadCurrent(s)->currentChunk;
  target->hierarchicalHeap = getThreadCurrent(s)->hierarchicalHeap;
  target->bytesAllocatedSinceLastCollection =
    getThreadCurrent(s)->bytesAllocatedSinceLastCollection;
  target->bytesSurvivedLastCollection =
    getThreadCurrent(s)->bytesSurvivedLastCollection;
  target->minLocalCollectionDepth = getThreadCurrent(s)->minLocalCollectionDepth;
  target->decheckState = getThreadCurrent(s)->decheckState;

  uint32_t* fromSyncDepths = &(getThreadCurrent(s)->decheckSyncDepths[0]);
  uint32_t* toSyncDepths = &(target->decheckSyncDepths[0]);
  memcpy(toSyncDepths, fromSyncDepths, DECHECK_DEPTHS_LEN * sizeof(uint32_t));

  HM_HierarchicalHeap originalHH = (HM_HierarchicalHeap)abandonedHH;
  assert(HM_HH_getDepth(originalHH) == 0);

  getThreadCurrent(s)->currentDepth = 0;
  getThreadCurrent(s)->hierarchicalHeap = originalHH;
  getThreadCurrent(s)->bytesAllocatedSinceLastCollection = 0;
  getThreadCurrent(s)->bytesSurvivedLastCollection = 0;
  getThreadCurrent(s)->decheckState = DECHECK_BOGUS_TID;

  // Find a chunk to get back to
  HM_chunk chunk = HM_getChunkListLastChunk(HM_HH_getChunkList(originalHH));
  assert(NULL != chunk);
  getThreadCurrent(s)->currentChunk = chunk;

  if (!chunk->mightContainMultipleObjects) {
    if (!HM_HH_extend(s, getThreadCurrent(s), GC_HEAP_LIMIT_SLOP)) {
      DIE("Ran out of space for Hierarchical Heap!");
    }
  }

  assert(NULL != getThreadCurrent(s)->currentChunk);
  assert(getThreadCurrent(s)->currentChunk->mightContainMultipleObjects);

  s->frontier = HM_HH_getFrontier(getThreadCurrent(s));
  s->limitPlusSlop = HM_HH_getLimit(getThreadCurrent(s));
  s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;

  HM_ensureHierarchicalHeapAssurances(
    s,
    FALSE,
    getThreadCurrent(s)->bytesNeeded,
    FALSE);
}

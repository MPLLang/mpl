/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/* enter and leave should be called at the start and end of every GC
 * function that is exported to the outside world.  They make sure
 * that the function is run in a critical section and check the GC
 * invariant.
 */

void enter (GC_state s) {

  // Trace0(EVENT_RUNTIME_ENTER);
  GC_MayTerminateThread(s);
  /* used needs to be set because the mutator has changed s->stackTop. */
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
  setPromoStackOfCurrentThread(s, s->promoStackBot, s->promoStackTop);
  getThreadCurrent(s)->exnStack = s->exnStack;
  getThreadCurrent(s)->spareHeartbeatTokens = s->spareHeartbeatTokens;
  HM_HH_updateValues(s, getThreadCurrent(s), s->frontier);
  HH_EBR_leaveQuiescentState(s);
  HM_EBR_leaveQuiescentState(s);
  beginAtomic(s);

  assert(threadAndHeapOkay(s));

  /* The mutator never comes back to the runtime in a chunk larger than a
   * block. HM_getChunkOf finds an object's chunk from the object's block, so
   * only the first block of a chunk may hold the starts of objects, and a
   * mutator left in a larger chunk would allocate past it. A chunk of
   * several blocks is opened for one object that begins at its start, and
   * whoever opens it takes the thread out of it before returning to the
   * mutator: a large sequence (allocateLargeSequence), a stack or an object
   * that crosses the first block (newObject, initDynHeap), a to-space chunk
   * with a large object (HM_ensureHierarchicalHeapAssurances after a
   * collection). The one that is left to the mutator, for the result of an
   * IntInf primitive, is closed by the primitive itself, which makes no
   * runtime call before that (leaveOversizedChunk in int-inf.c).
   *
   * What this catches is a limit check that asked for more than a block and
   * left the mutator to fill the chunk: a statically sized request above
   * GC_LIMIT_CHECK_MAX_BYTES (see heap.h), which GC_collect used to reject
   * with an assertion of its own, before requests of a size known only at
   * run time were let through. */
  assert((size_t)(HM_getChunkLimit(getThreadCurrent(s)->currentChunk)
                  - (pointer)(getThreadCurrent(s)->currentChunk))
         <= s->controls->blockSize);

  // if (DEBUG) {
  //   /* RAM_NOTE: Switch to using LOG */
  //   displayGCState (s, stderr);
  // }

  // assert (invariantForGC (s));
}

void leave (GC_state s) {
  s->spareHeartbeatTokens = getThreadCurrent(s)->spareHeartbeatTokens;
  s->promoStackBot = getStackCurrent(s)->promoStackBot;

  /* The mutator frontier invariant may not hold
   * for functions that don't ensureBytesFree.
   */
  assert(invariantForMutator(s, FALSE, TRUE));
  endAtomic (s);
  // Trace0(EVENT_RUNTIME_LEAVE);
}

/* Copyright (C) 2018-2019 Sam Westrick
 * Copyright (C) 2016,2019 Matthew Fluet.
 * Copyright (C) 2014-2015 Ram Raghunathan
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#include "thread.h"
#include "hierarchical-heap.h"

/************************/
/* Function definitions */
/************************/
#if (defined (MLTON_GC_INTERNAL_BASIS))


#if 0
Bool GC_HH_checkFinishedCCReadyToJoin(GC_state s) {
  GC_thread thread = getThreadCurrent(s);
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed(s);
  thread->exnStack = s->exnStack;
  HM_HH_updateValues(thread, s->frontier);
  HM_HierarchicalHeap hh = thread->hierarchicalHeap;

  /** Check that we are immediately below a root CC.
    * TODO: eliminate the subHeapForCC to simplify.
    */
  bool inRightPlace =
    thread->currentDepth == 2 &&
    HM_HH_getDepth(hh) == 2 &&
    hh->nextAncestor != NULL && HM_HH_getDepth(hh->nextAncestor) == 1 &&
    hh->nextAncestor->subHeapForCC != NULL;

  if (!inRightPlace)
    return FALSE;

  HM_HierarchicalHeap rhh = hh->nextAncestor->subHeapForCC;
  return HM_HH_getConcurrentPack(rhh)->ccstate == CC_UNREG;
}
#else
Bool GC_HH_checkFinishedCCReadyToJoin(__attribute__((unused)) GC_state s) {
  DIE("GC_HH_checkFinishedCCReadyToJoin no longer used");
}
#endif

const GC_tokenPolicy TOKEN_POLICY_FAIR = 0; // split evenly
const GC_tokenPolicy TOKEN_POLICY_KEEP = 1; // keep all
const GC_tokenPolicy TOKEN_POLICY_GIVE = 2; // give all

Word32 GC_HH_getDepth(pointer threadp) {
  GC_state s = pthread_getspecific(gcstate_key);
  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));

  assert(thread != NULL);
  return thread->currentDepth;
}

void GC_HH_setDepth(pointer threadp, Word32 depth) {
  GC_state s = pthread_getspecific(gcstate_key);
  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));

  assert(thread != NULL);
  thread->currentDepth = depth;
  // printf("%s %d\n", "setting thread depth to ", depth);
  // printf("%s %d\n", "HH depth = ", thread->hierarchicalHeap->depth);

  /* SAM_NOTE: not super relevant here, but if we do eventually decide to
   * control the "use ancestor chunk" optimization, a good sanity check. */
  assert(inSameBlock(s->frontier, s->limitPlusSlop-1));
  assert(((HM_chunk)blockOf(s->frontier))->magic == CHUNK_MAGIC);
}

void GC_HH_setMinLocalCollectionDepth(pointer threadp, Word32 depth) {
  GC_state s = pthread_getspecific(gcstate_key);
  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));
  thread->minLocalCollectionDepth = depth;
}

void GC_HH_mergeThreads(pointer threadp, pointer childp) {
  GC_state s = pthread_getspecific(gcstate_key);
  enter(s);

  objptr threadop = pointerToObjptr(threadp, NULL);
  objptr childop = pointerToObjptr(childp, NULL);
  GC_thread thread = threadObjptrToStruct(s, threadop);
  GC_thread child = threadObjptrToStruct(s, childop);

  size_t terminateCheckCounter = 0;
  while (atomicLoadS32(&(child->currentProcNum)) >= 0) {
    /* Spin while someone else is currently executing this thread. The
     * termination checks happen rarely, and reset terminateCheckCounter to 0
     * when they do. */
    GC_MayTerminateThreadRarely(s, &terminateCheckCounter);
    if (terminateCheckCounter == 0) pthread_yield();
  }

#if ASSERT
  assert(threadop != BOGUS_OBJPTR);
  /* make sure thread is either mine or inactive */
  for (uint32_t i = 0; i < s->numberOfProcs; i++) {
    if ((int32_t)i != s->procNumber)
      assert(s->procStates[i].currentThread != threadop);
  }

  assert(childop != BOGUS_OBJPTR);
  /* SAM_NOTE there is a race where the following check can raise
   * a false alarm, if a worker delays to mark its current thread as
   * BOGUS_OBJPTR after completing a thread and decrementing the incounter
   * (in the scheduler). However, having the assert seems useful as a
   * sanity check regardless.
   *
   * If this becomes a problem, we can either fix the incounter business
   * (switch away before decrementing incounter) or just remove the sanity
   * check and not worry about it.
   */
  // Make sure child is inactive
  // for (uint32_t i = 0; i < s->numberOfProcs; i++) {
  //   assert(s->procStates[i].currentThread != childop);
  // }
#endif

  assert(thread != NULL);
  assert(thread->hierarchicalHeap != NULL);
  assert(child != NULL);
  assert(child->hierarchicalHeap != NULL);

  HM_HH_merge(s, thread, child);

  /* SAM_NOTE: Why do we need to ensure here?? Is it just to ensure current
   * level? */
  /* SAM_NOTE: It is important that this happens after the merge completes,
   * because the stack may contain a pointer into the child's heap (we merge
   * right after reading from the "right branch" result ref).
   */
  HM_ensureHierarchicalHeapAssurances(s, false, GC_HEAP_LIMIT_SLOP, TRUE);

  /* This should be true, otherwise our call to
   * HM_ensureHierarchicalHeapAssurances() above was on the wrong heap!
   */
  assert(getHierarchicalHeapCurrent(s) == thread->hierarchicalHeap);

  leave(s);
}

void GC_HH_promoteChunks(pointer threadp) {
  GC_state s = pthread_getspecific(gcstate_key);
  enter(s);

  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));
  assert(thread != NULL);
  assert(thread->hierarchicalHeap != NULL);
  HM_HH_promoteChunks(s, thread);
  leave(s);
}

void GC_HH_clearSuspectsAtDepth(GC_state s, pointer threadp, uint32_t depth) {
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed(s);
  getThreadCurrent(s)->exnStack = s->exnStack;
  HM_HH_updateValues(getThreadCurrent(s), s->frontier);
  assert(threadAndHeapOkay(s));

  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));
  assert(thread != NULL);
  assert(thread->hierarchicalHeap != NULL);
  HM_HH_clearSuspectsAtDepth(s, thread, depth);
}

Word64 GC_HH_numSuspectsAtDepth(GC_state s, pointer threadp, uint32_t targetDepth) {
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed(s);
  getThreadCurrent(s)->exnStack = s->exnStack;
  HM_HH_updateValues(getThreadCurrent(s), s->frontier);
  assert(threadAndHeapOkay(s));
  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));
  assert(thread != NULL);
  assert(thread->hierarchicalHeap != NULL);

  for (HM_HierarchicalHeap cursor = thread->hierarchicalHeap;
       NULL != cursor;
       cursor = cursor->nextAncestor)
  {
    uint32_t d = HM_HH_getDepth(cursor);
    if (d <= targetDepth) {
      if (d == targetDepth) return (Word64)ES_numSuspects(s, cursor);
      return 0;
    }
  }

  return 0;
}

Pointer /*ES_clearSet*/
GC_HH_takeClearSetAtDepth(GC_state s, pointer threadp, uint32_t targetDepth) {
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed(s);
  getThreadCurrent(s)->exnStack = s->exnStack;
  HM_HH_updateValues(getThreadCurrent(s), s->frontier);
  assert(threadAndHeapOkay(s));
  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));
  assert(thread != NULL);
  assert(thread->hierarchicalHeap != NULL);
  return (pointer)ES_takeClearSet(s, HM_HH_getHeapAtDepth(s, thread, targetDepth));
}

Word64 GC_HH_numChunksInClearSet(GC_state s, pointer clearSet) {
  return (Word64)ES_numChunksInClearSet(s, (ES_clearSet)clearSet);
}

Pointer /*ES_finishedClearSetGrain*/
GC_HH_processClearSetGrain(GC_state s, pointer clearSet, Word64 start, Word64 stop) {
  return (pointer)ES_processClearSetGrain(s, (ES_clearSet)clearSet, (size_t)start, (size_t)stop);
}

void GC_HH_commitFinishedClearSetGrain(GC_state s, pointer threadp, pointer finClearSetGrain) {
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed(s);
  getThreadCurrent(s)->exnStack = s->exnStack;
  HM_HH_updateValues(getThreadCurrent(s), s->frontier);
  assert(threadAndHeapOkay(s));
  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));
  ES_commitFinishedClearSetGrain(s, thread, (ES_finishedClearSetGrain)finClearSetGrain);
}

void GC_HH_deleteClearSet(GC_state s, pointer clearSet) {
  ES_deleteClearSet(s, (ES_clearSet)clearSet);
}

void GC_HH_moveNewThreadToDepth(pointer threadp, uint64_t tidParent, uint32_t depth) {
  GC_state s = pthread_getspecific(gcstate_key);
  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));
  assert(thread != NULL);
  HM_HierarchicalHeap hh = thread->hierarchicalHeap;

  /* A few sanity checks. The thread should be a "new" thread that was
   * just created by a call to copyThreadWithHeap.
   *
   * We could put in assertions to check this more thoroughly, for example
   * we could check that the only stuff currently in the chunk of the thread
   * is exactly the thread and its stack, and nothing else. But these sanity
   * checks are good enough for now... */
  assert(hh != NULL);
  assert(HM_HH_getDepth(hh) == 0);
  assert(HM_getChunkListFirstChunk(HM_HH_getChunkList(hh))->nextChunk ==
         HM_getChunkListLastChunk(HM_HH_getChunkList(hh)));
  assert(HM_getChunkOf(threadp) == HM_getChunkListFirstChunk(HM_HH_getChunkList(hh)));
  assert(HM_getChunkOf(objptrToPointer(thread->stack, NULL)) ==
         HM_getChunkListLastChunk(HM_HH_getChunkList(hh)));

  assert(HM_getChunkOf(threadp)->decheckState.bits == DECHECK_BOGUS_BITS);
  assert(HM_getChunkOf((pointer)thread->stack)->decheckState.bits == DECHECK_BOGUS_BITS);

  HM_getChunkOf(threadp)->decheckState.bits = tidParent;
  HM_getChunkOf((pointer)thread->stack)->decheckState.bits = tidParent;

  thread->currentDepth = depth;
  hh->depth = depth;
}


void GC_HH_joinIntoParentBeforeFastClone(
  GC_state s,
  pointer threadp,
  uint32_t newDepth,
  uint64_t tidLeft,
  uint64_t tidRight)
{
  enter(s);

  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));
  assert(getThreadCurrent(s) == thread);
  assert(thread != NULL);
  assert(thread->hierarchicalHeap != NULL);
  assert(newDepth == thread->currentDepth-1);

  HM_HH_promoteChunks(s, thread);
  thread->currentDepth = newDepth;

  /* SAM_NOTE: not super relevant here, but if we do eventually decide to
   * control the "use ancestor chunk" optimization, a good sanity check. */
  assert(inSameBlock(s->frontier, s->limitPlusSlop-1));
  assert(((HM_chunk)blockOf(s->frontier))->magic == CHUNK_MAGIC);

  GC_HH_decheckJoin(s, tidLeft, tidRight);

  leave(s);
}


void GC_HH_joinIntoParent(
  GC_state s,
  pointer threadp,
  pointer rightSideThreadp,
  uint32_t newDepth,
  uint64_t tidLeft,
  uint64_t tidRight)
{
  enter(s);

  objptr threadop = pointerToObjptr(threadp, NULL);
  objptr childop = pointerToObjptr(rightSideThreadp, NULL);
  GC_thread thread = threadObjptrToStruct(s, threadop);
  GC_thread child = threadObjptrToStruct(s, childop);

  assert(getThreadCurrent(s) == thread);
  assert(thread != NULL);
  assert(thread->hierarchicalHeap != NULL);
  assert(newDepth == thread->currentDepth-1);

  /* ======================================================================== */

  size_t terminateCheckCounter = 0;
  while (atomicLoadS32(&(child->currentProcNum)) >= 0) {
    /* Spin while someone else is currently executing this thread. The
     * termination checks happen rarely, and reset terminateCheckCounter to 0
     * when they do. */
    GC_MayTerminateThreadRarely(s, &terminateCheckCounter);
    if (terminateCheckCounter == 0) pthread_yield();
  }

#if ASSERT
  assert(threadop != BOGUS_OBJPTR);
  /* make sure thread is either mine or inactive */
  for (uint32_t i = 0; i < s->numberOfProcs; i++) {
    if ((int32_t)i != s->procNumber)
      assert(s->procStates[i].currentThread != threadop);
  }

  assert(childop != BOGUS_OBJPTR);
  /* SAM_NOTE there is a race where the following check can raise
   * a false alarm, if a worker delays to mark its current thread as
   * BOGUS_OBJPTR after completing a thread and decrementing the incounter
   * (in the scheduler). However, having the assert seems useful as a
   * sanity check regardless.
   *
   * If this becomes a problem, we can either fix the incounter business
   * (switch away before decrementing incounter) or just remove the sanity
   * check and not worry about it.
   */
  // Make sure child is inactive
  // for (uint32_t i = 0; i < s->numberOfProcs; i++) {
  //   assert(s->procStates[i].currentThread != childop);
  // }
#endif

  assert(thread != NULL);
  assert(thread->hierarchicalHeap != NULL);
  assert(child != NULL);
  assert(child->hierarchicalHeap != NULL);

  HM_HH_merge(s, thread, child);

  /* ======================================================================== */

  HM_HH_promoteChunks(s, thread);
  thread->currentDepth = newDepth;

  /* SAM_NOTE: not super relevant here, but if we do eventually decide to
   * control the "use ancestor chunk" optimization, a good sanity check. */
  assert(inSameBlock(s->frontier, s->limitPlusSlop-1));
  assert(((HM_chunk)blockOf(s->frontier))->magic == CHUNK_MAGIC);

  GC_HH_decheckJoin(s, tidLeft, tidRight);


  /* SAM_NOTE: Why do we need to ensure here?? Is it just to ensure current
   * level? */
  /* SAM_NOTE: It is important that this happens after the merge completes,
   * because the stack may contain a pointer into the child's heap (we merge
   * right after reading from the "right branch" result ref).
   */
  HM_ensureHierarchicalHeapAssurances(s, false, GC_HEAP_LIMIT_SLOP, TRUE);

  /* This should be true, otherwise our call to
   * HM_ensureHierarchicalHeapAssurances() above was on the wrong heap!
   */
  assert(getHierarchicalHeapCurrent(s) == thread->hierarchicalHeap);


  leave(s);
}


uint32_t GC_addSpareHeartbeats(GC_state s, uint32_t spares) {
  uint32_t current = s->spareHeartbeatTokens;
  s->spareHeartbeatTokens = min(current+spares, s->controls->heartbeatTokens);
  return s->spareHeartbeatTokens;
}


Bool GC_tryConsumeSpareHeartbeats(GC_state s, uint32_t count) {
  uint32_t spares = s->spareHeartbeatTokens;
  if (spares >= count) {
    // LOG(LM_PARALLEL, LL_FORCE, "success (count = %u). new spares = %u", count, spares - count);
    s->spareHeartbeatTokens -= count;
    return TRUE;
  }
  return FALSE;
}

bool GC_HH_canForkThread(GC_state s, pointer threadp) {
  enter(s);
  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));
  GC_stack fromStack = (GC_stack)objptrToPointer(thread->stack, NULL);
  // pointer pframe = findPromotableFrame(s, fromStack);
  pointer pframe = findYoungestPromotableFrame(s, fromStack);
  leave(s);
  return NULL != pframe;
}

objptr GC_HH_forkThread(GC_state s, bool youngestOptimization, pointer threadp, pointer dp) {
  enter(s);
  GC_thread thread = threadObjptrToStruct(s, pointerToObjptr(threadp, NULL));
  GC_stack fromStack = (GC_stack)objptrToPointer(thread->stack, NULL);

  pointer pframe =
    (youngestOptimization ?
      findYoungestPromotableFrame(s, fromStack) :
      findPromotableFrame(s, fromStack));

  /* Note that the promotion policy is _always_ oldest-first, regardless of the
   * `youngestOptimization` flag. The following assertion checks that we haven't
   * violated this. It's valid to use `youngestOptimization=TRUE` as only in the
   * case where we know that the youngest and oldest frames happen to coincide.
   * The scheduler figures this out: if we have a spare heartbeat token at the
   * moment that we execute a pcall, then we know that no other ancestor frames
   * in the stack are promotable (otherwise the spare token would have been
   * spent to promote them).
   */
  assert(findPromotableFrame(s, fromStack) == pframe);

  if (NULL == pframe) {
    DIE("forkThread failed!");
    leave(s);
    return BOGUS_OBJPTR;
  }

  GC_returnAddress cont_ret = *((GC_returnAddress*)(pframe - GC_RETURNADDRESS_SIZE));
  GC_frameInfo fi = getFrameInfoFromReturnAddress(s, cont_ret);
#if ASSERT
  assert(fi->sporkInfo != NULL);
#endif
  GC_returnAddress spwn_ret = 0;
  GC_tokenPolicy token_policy;
  uintptr_t spwn_idx;
  for (spwn_idx = 0 ; spwn_idx < fi->sporkInfo->nest ; spwn_idx++) {
    objptr p = *((objptr*) (pframe - fi->size + fi->sporkInfo->offset + OBJPTR_SIZE * spwn_idx));
    if (!isObjptr(p)) {
      spwn_ret = fi->sporkInfo->spwns[spwn_idx];
      token_policy = fi->sporkInfo->tokenPolicies[spwn_idx];
      break;
    }
  }

  // =========================================================================
  // First, write dp (data pointer) onto the promotable frame
  // =========================================================================

  objptr dop = pointerToObjptr(dp, NULL);
  *((objptr*)(pframe - fi->size + fi->sporkInfo->offset + OBJPTR_SIZE * spwn_idx)) = dop;

  // =========================================================================
  // Next, copy the promotable frame
  // =========================================================================

  // SAM_NOTE: we need a decent amount of reserved space to make sure that
  // the next call on this stack has enough space. Next call might be a
  // non-tail, which might bump up to max frame size.
  size_t newStackReserved = alignStackReserved(s, 2*s->maxFrameSize);
  // TODO: do we actually need to multiply by 2?

  uint32_t newDepth = getThreadCurrent(s)->currentDepth;

  assert (s->savedThread == BOGUS_OBJPTR);
  s->savedThread = pointerToObjptr(threadp, NULL);
  GC_thread copied = newThread (s, newStackReserved);
  assert(s->savedThread == pointerToObjptr(threadp, NULL));
  s->savedThread = BOGUS_OBJPTR;
  objptr copiedp =
    pointerToObjptr((pointer)copied - offsetofThread(s), NULL);

  GC_stack toStack = (GC_stack)objptrToPointer(copied->stack, NULL);
  copyStackFrameToNewStack(s, pframe, fromStack, toStack);
  pointer newFrame = getStackTop(s, toStack);

  // Transition new frame to SPORK_SPWN_FRAME
  *(GC_returnAddress*)(newFrame - GC_RETURNADDRESS_SIZE) = spwn_ret;
  // Invalidate other spork data slots of new frame.
  for (uintptr_t i = 0 ; i < spwn_idx ; i++) {
    *((objptr*) (newFrame - fi->size + fi->sporkInfo->offset + OBJPTR_SIZE * i)) = BOGUS_OBJPTR;
  }

  /* SAM_NOTE: would like to assert these, but jop may have already been
   * forwarded above (calling newThread, above, might trigger a GC)
   */
  // assert(*(objptr*)(pframe - fi->size) == jop);
  // assert(*(objptr*)(newFrame - fi->size) == jop);

  // Is this right? Or are we missing one suspended depth because forkThread
  // happens before the decheck fork? Might need to fix this by fusing decheck
  // fork with this function.
  GC_HH_copySyncDepthsFromThread(s, getThreadCurrentObjptr(s), copiedp, newDepth);

  // uint32_t spares = getThreadCurrent(s)->spareHeartbeatTokens;
  // uint32_t half = (spares == 0) ? 0 : min(spares-1, spares >> 1);
  // copied->spareHeartbeatTokens += half;
  // getThreadCurrent(s)->spareHeartbeatTokens -= half;

  uint32_t spares = s->spareHeartbeatTokens;
  uint32_t give_tokens = 0;
  if (token_policy == TOKEN_POLICY_FAIR) {
      give_tokens = spares >> 1;
  } else if (token_policy == TOKEN_POLICY_GIVE) {
      give_tokens = spares;
  } else if (token_policy == TOKEN_POLICY_KEEP) {
      give_tokens = 0;
  } else {
    char msg[100];
    snprintf(msg, 100, "unknown token policy %u", token_policy);
    DIE(msg);
    leave(s);
    return BOGUS_OBJPTR;
  }
  s->spareHeartbeatTokens -= give_tokens;
  copied->spareHeartbeatTokens += give_tokens;

  leave(s);
  return pointerToObjptr((pointer)copied - offsetofThread(s), NULL);
}


#endif /* MLTON_GC_INTERNAL_BASIS */

#if (defined (MLTON_GC_INTERNAL_FUNCS))
void displayThread (GC_state s,
                    GC_thread thread,
                    FILE *stream) {
  fprintf(stream,
          "\t\texnStack = %"PRIiMAX"\n"
          "\t\tbytesNeeded = %"PRIuMAX"\n"
          "\t\thierarchicalHeap = %p\n"
          "\t\tstack = "FMTOBJPTR"\n",
          (intmax_t)thread->exnStack,
          (uintmax_t)thread->bytesNeeded,
          (void*)thread->hierarchicalHeap,
          thread->stack);
  displayStack (s, (GC_stack)(objptrToPointer (thread->stack, NULL)),
                stream);
  /* RAM_NOTE: displayHH! */
}

size_t sizeofThread(GC_state s) {
  size_t res;

  res = GC_NORMAL_METADATA_SIZE + sizeof (struct GC_thread);
  res = align (res, s->alignment);
  if (DEBUG) {
    size_t check;
    uint16_t bytesNonObjptrs, numObjptrs;

    splitHeader (s, GC_THREAD_HEADER, NULL, NULL, &bytesNonObjptrs, &numObjptrs);
    check = GC_NORMAL_METADATA_SIZE + (bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE));
    if (DEBUG_DETAILED)
      fprintf (stderr,
               "sizeofThread: res = %"PRIuMAX"  check = %"PRIuMAX"\n",
               (uintmax_t)res, (uintmax_t)check);
    assert (check == res);
  }
  assert (isAligned (res, s->alignment));
  return res;
}

// SAM_NOTE: padding for alignment.
size_t offsetofThread (GC_state s) {
  return (sizeofThread (s)) - (GC_NORMAL_METADATA_SIZE + sizeof (struct GC_thread));
}

static inline GC_thread threadObjptrToStruct(GC_state s, objptr threadObjptr) {
  if (BOGUS_OBJPTR == threadObjptr) {
    return NULL;
  }

  pointer threadPointer = objptrToPointer (threadObjptr, NULL);
  return ((GC_thread)(threadPointer + offsetofThread(s)));
}
#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

/* Copyright (C) 2019-2020 Sam Westrick
 * Copyright (C) 1999-2017 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

void Assignable_decheckObjptr(objptr op)
{
  GC_state s = pthread_getspecific(gcstate_key);
  s->cumulativeStatistics->numDisentanglementChecks++;
  decheckRead(s, op);
}

objptr Assignable_readBarrier(
  GC_state s,
  __attribute__((unused)) objptr obj,
  objptr* field)
{
  s->cumulativeStatistics->numDisentanglementChecks++;
  objptr ptr = *field;
  decheckRead(s, ptr);
  return ptr;
}

void Assignable_writeBarrier(
  GC_state s,
  objptr dst,
  ARG_USED_FOR_ASSERT objptr* field,
  objptr src)
{
  assert(isObjptr(dst));
  pointer dstp = objptrToPointer(dst, NULL);
  pointer srcp = objptrToPointer(src, NULL);

#if ASSERT
  // check that field is actually inside this object
  GC_header header = getHeader(dstp);
  GC_objectTypeTag tag;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
  bool hasIdentity;
  splitHeader(s, header, &tag, &hasIdentity, &bytesNonObjptrs, &numObjptrs);
  pointer objend = dstp;
  if (!hasIdentity) {
    DIE("write barrier: attempting to modify immutable object "FMTOBJPTR, dst);
  }
  if (NORMAL_TAG == tag) {
    objend += bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
  }
  else if (SEQUENCE_TAG == tag) {
    size_t dataBytes = getSequenceLength(dstp) * (bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE));
    objend += alignWithExtra (s, dataBytes, GC_SEQUENCE_METADATA_SIZE);
  }
  else {
    DIE("write barrier: cannot handle tag %u", tag);
  }
  pointer fieldp = (pointer)field;
  ASSERTPRINT(
    dstp <= fieldp && fieldp + OBJPTR_SIZE <= objend,
    "write barrier: objptr field %p outside object "FMTOBJPTR" of size %zu",
    (void*)field,
    dst,
    (size_t)(objend - dstp));
#endif

  /* If src does not reference an object, then no need to check for
   * down-pointers. */
  if (!isObjptr(src))
    return;

  /* deque down-pointers are handled separately during collection. */
  if (dst == s->wsQueue)
    return;

  if (s->controls->manageEntanglement &&
      getThreadCurrent(s)->decheckState.bits != DECHECK_BOGUS_BITS &&

      ((HM_getChunkOf(srcp)->decheckState.bits != DECHECK_BOGUS_BITS &&
      !decheckIsOrdered(getThreadCurrent(s), HM_getChunkOf(srcp)->decheckState))
      ||
      (HM_getChunkOf(dstp)->decheckState.bits != DECHECK_BOGUS_BITS &&
      !decheckIsOrdered(getThreadCurrent(s), HM_getChunkOf(dstp)->decheckState))))
  {
    /** Nasty entanglement. To be safe, just pin the object. A safe unpin
      * depth is the overall lca.
      */

    int unpinDepth1 =
      lcaHeapDepth(getThreadCurrent(s)->decheckState,
                   HM_getChunkOf(srcp)->decheckState);

    int unpinDepth2 =
      lcaHeapDepth(getThreadCurrent(s)->decheckState,
                   HM_getChunkOf(dstp)->decheckState);

    int unpinDepth = (unpinDepth1 < unpinDepth2 ? unpinDepth1 : unpinDepth2);

    if (pinObject(src, (uint32_t)unpinDepth)) {
      /** Just remember it at some arbitrary place... */
      HM_rememberAtLevel(getThreadCurrent(s)->hierarchicalHeap, src);
    }

    return;
  }

  HM_HierarchicalHeap dstHH = HM_getLevelHeadPathCompress(HM_getChunkOf(dstp));
  HM_HierarchicalHeap srcHH = HM_getLevelHeadPathCompress(HM_getChunkOf(srcp));

  /* Internal or up-pointer. */
  if (dstHH->depth >= srcHH->depth)
    return;

  /* Otherwise, remember the down-pointer! */
  uint32_t d = srcHH->depth;
  GC_thread thread = getThreadCurrent(s);

  /** Fix a silly issue where, when we are dealing with entanglement, the
    * lower object is actually deeper than the current thread (which is
    * possible because of entanglement! the thread is peeking inside of
    * some other thread's heaps, and the other thread might be deeper).
    */
  if (d > thread->currentDepth && s->controls->manageEntanglement)
    d = thread->currentDepth;

  HM_HierarchicalHeap hh = HM_HH_getHeapAtDepth(s, thread, d);

  if (hh == NULL)
  {
    LOG(LM_HH_PROMOTION, LL_WARNING,
      "Write down pointer without local hierarchical heap: "FMTOBJPTR " to "FMTOBJPTR,
      dst, src);
    return;
  }

  if (pinObject(src, dstHH->depth)) {
    HM_rememberAtLevel(hh, src);
  }

  LOG(LM_HH_PROMOTION, LL_INFO,
    "remembered downptr %"PRIu32"->%"PRIu32" from "FMTOBJPTR" to "FMTOBJPTR,
    dstHH->depth, srcHH->depth,
    dst, src);

  /* SAM_NOTE: TODO: track bytes allocated here in
   * thread->bytesAllocatedSinceLast...? */
}

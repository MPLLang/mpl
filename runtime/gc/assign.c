/* Copyright (C) 2019-2021 Sam Westrick
 * Copyright (C) 1999-2017 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */
void Assignable_decheckObjptr(ARG_USED_FOR_ASSERT objptr *obj, objptr op)
{
  GC_state s = pthread_getspecific(gcstate_key);
  assert(!isObjptr(*obj) || ES_contains(NULL, *obj));
  s->cumulativeStatistics->numDisentanglementChecks++;
  decheckRead(s, op);
}

objptr Assignable_readBarrier(
  GC_state s,
  ARG_USED_FOR_ASSERT objptr obj,
  objptr *field)
{

#if ASSERT
  assert(isObjptr(obj));
  // check that field is actually inside this object
  pointer objp = objptrToPointer(obj, NULL);
  GC_header header = getHeader(objp);
  GC_objectTypeTag tag;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
  bool hasIdentity;
  splitHeader(s, header, &tag, &hasIdentity, &bytesNonObjptrs, &numObjptrs);
  pointer objend = objp;
  if (!hasIdentity) {
    DIE("read barrier: attempting to read immutable object "FMTOBJPTR, obj);
  }
  if (NORMAL_TAG == tag) {
    objend += bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
  }
  else if (SEQUENCE_TAG == tag) {
    size_t dataBytes = getSequenceLength(objp) * (bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE));
    objend += alignWithExtra (s, dataBytes, GC_SEQUENCE_METADATA_SIZE);
  }
  else {
    DIE("read barrier: cannot handle tag %u", tag);
  }
  pointer fieldp = (pointer)field;
  ASSERTPRINT(
    objp <= fieldp && fieldp + OBJPTR_SIZE <= objend,
    "read barrier: objptr field %p outside object "FMTOBJPTR" of size %zu",
    (void*)field,
    obj,
    (size_t)(objend - objp));
#endif
  assert(ES_contains(NULL, obj));
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

  HM_HierarchicalHeap dstHH = HM_getLevelHeadPathCompress(HM_getChunkOf(dstp));

  objptr readVal = *field;
  if (dstHH->depth >= 1 && isObjptr(readVal) && s->wsQueueTop!=BOGUS_OBJPTR) {
    pointer currp = objptrToPointer(readVal, NULL);
    HM_HierarchicalHeap currHH = HM_getLevelHead(HM_getChunkOf(currp));
    if (currHH->depth == dstHH->depth
        && HM_HH_getConcurrentPack(currHH)->ccstate != CC_UNREG
        && !CC_isPointerMarked(currp))
    {
      HM_HH_addRootForCollector(s, currHH, currp);
    }
  }

  /* If src does not reference an object, then no need to check for
   * down-pointers. */
  if (!isObjptr(src)){
    return;
  }

  /* deque down-pointers are handled separately during collection. */
  if (dst == s->wsQueue)
    return;

  struct HM_remembered remElem_ = {.object = src, .from = dst};
  HM_remembered remElem = &remElem_;

  pointer srcp = objptrToPointer(src, NULL);

#if 0
  /** This is disabled for now. In the future we will come back to
    * managing entanglement.
    */
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
      HM_rememberAtLevel(getThreadCurrent(s)->hierarchicalHeap, remElem);
    }

    return;
  }
#endif

  HM_HierarchicalHeap srcHH = HM_getLevelHeadPathCompress(HM_getChunkOf(srcp));

  /* Up-pointer. */
  if (dstHH->depth > srcHH->depth)
    return;

  /* Internal pointer. It's safe to ignore an internal pointer if:
   *   1. it's contained entirely within one subheap, or
   *   2. the pointed-to object (src) lives in an already snapshotted subregion
   */
  if ( (dstHH == srcHH) ||
       (dstHH->depth == srcHH->depth &&
         HM_HH_getConcurrentPack(srcHH)->ccstate != CC_UNREG) ) {
    // assert(...);
    // if (dstHH != srcHH) {
    //   printf(
    //     "ignore internal pointer "FMTPTR" --> "FMTPTR". dstHH == srcHH? %d\n",
    //     (uintptr_t)dstp,
    //     (uintptr_t)srcp,
    //     srcHH == dstHH);
    // }
    return;
  }
  /** Otherwise, its a down-pointer, so
    *  (i) make dst a suspect for entanglement, i.e., mark the suspect bit of dst's header
    *      (see pin.h for header-layout).
    *      the compiler checks this suspect bit and calls the read-barrier
    *      only when the bit is set.
    *  (ii) pin the src object
    *  (iii) remember the down pointer
    */

  /* make dst a suspect for entanglement */
  uint32_t dd = dstHH->depth;
  GC_thread thread = getThreadCurrent(s);
  if (dd > 0 && !ES_contains(NULL, dst)) {
    HM_HierarchicalHeap dhh = HM_HH_getHeapAtDepth(s, thread, dd);
    ES_add(s, HM_HH_getSuspects(dhh), dst);
  }

  bool success = pinObject(src, dd);

  // any concurrent pin can only decrease unpinDepth
  uint32_t unpinDepth = unpinDepthOf(src);
  assert(unpinDepth <= dd);

  if (success || dd == unpinDepth)
  {
    uint32_t sd = srcHH->depth;
#if 0
    /** Fix a silly issue where, when we are dealing with entanglement, the
      * lower object is actually deeper than the current thread (which is
      * possible because of entanglement! the thread is peeking inside of
      * some other thread's heaps, and the other thread might be deeper).
      */
    if (d > thread->currentDepth && s->controls->manageEntanglement)
      d = thread->currentDepth;
#endif

    HM_HierarchicalHeap shh = HM_HH_getHeapAtDepth(s, thread, sd);
    assert(NULL != shh);
    assert(HM_HH_getConcurrentPack(shh)->ccstate == CC_UNREG);
    HM_rememberAtLevel(shh, remElem);

    LOG(LM_HH_PROMOTION, LL_INFO,
      "remembered downptr %"PRIu32"->%"PRIu32" from "FMTOBJPTR" to "FMTOBJPTR,
      dstHH->depth, srcHH->depth,
      dst, src);
  }

  /* SAM_NOTE: TODO: track bytes allocated here in
   * thread->bytesAllocatedSinceLast...? */
}

// void Assignable_updateBarrier (GC_state s, objptr dst, objptr* field, objptr src) {
//   Assignable_writeBarrier(s, dst, field, src, false);
//   // *field = src;
// }
// void Assignable_casBarrier (GC_state s, objptr dst, objptr* field, objptr src) {
//   Assignable_writeBarrier(s, dst, field, src, true);
//   // cas(field, (*field), dst); //return?
// }





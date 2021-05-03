/* Copyright (C) 2019-2021 Sam Westrick
 * Copyright (C) 1999-2017 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

// #define cas(F, O, N) ((__sync_val_compare_and_swap(F, O, N)))

void Assignable_writeBarrier(GC_state s, objptr dst, objptr* field, objptr src) {
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
    if (currHH->depth == dstHH->depth) {
    // if(currHH == dstHH) {
      assert(currHH->depth == 1 || (currHH == dstHH));
      // printf("old pointer tracked dst="FMTPTR" old="FMTPTR" new="FMTPTR". currHH == dstHH? %d\n",
      //   (uintptr_t)dstp,
      //   (uintptr_t)currp,
      //   (uintptr_t)src,
      //   currHH == dstHH);
      HM_HH_addRootForCollector(currHH, currp);
    }
  }

  /* If src does not reference an object, then no need to check for
   * down-pointers. */
  if (!isObjptr(src))
    return;

  pointer srcp = objptrToPointer(src, NULL);
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

  /* deque down-pointers are handled separately during collection. */
  if (dst == s->wsQueue)
    return;

  /* Otherwise, remember the pointer! */
  uint32_t d = srcHH->depth;
  GC_thread thread = getThreadCurrent(s);
  HM_HierarchicalHeap hh = HM_HH_getHeapAtDepth(s, thread, d);
  assert(NULL != hh);
  if (HM_HH_getConcurrentPack(hh)->ccstate == CC_UNREG) {
    HM_rememberAtLevel(hh, dst, field, src);
  }
  else {
    /** This special subheap is guaranteed to exist while at least one CC
      * is registered or in-progress. Its sole purpose is to contain new
      * remembered-set entries. We can't use the remembered-set of the heap
      * that is undergoing CC, because the CC will be concurrently accessing
      * that remset.
      *
      * It's a bit strange... a bit of a hack... because this subheap is
      * always "empty" and is only used for its remset.
      */
    assert(NULL != hh->subHeapCompletedCC);
    HM_rememberAtLevel(hh->subHeapCompletedCC, dst, field, src);
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





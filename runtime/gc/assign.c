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
    if(currHH == dstHH) {
      HM_HH_addRootForCollector(dstHH, currp);
    }
  }

  /* If src does not reference an object, then no need to check for
   * down-pointers. */
  if (!isObjptr(src))
    return;

  pointer srcp = objptrToPointer(src, NULL);
  HM_HierarchicalHeap srcHH = HM_getLevelHeadPathCompress(HM_getChunkOf(srcp));

  /* Internal or up-pointer. */
  if (dstHH->depth >= srcHH->depth)
    return;

  /* deque down-pointers are handled separately during collection. */
  if (dst == s->wsQueue)
    return;

  /* Otherwise, remember the down-pointer! */
  uint32_t d = srcHH->depth;
  GC_thread thread = getThreadCurrent(s);
  HM_HierarchicalHeap hh = HM_HH_getHeapAtDepth(s, thread, d);
  assert(NULL != hh);
  HM_rememberAtLevel(hh, dst, field, src);

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





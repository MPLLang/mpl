/* Copyright (C) 2019 Sam Westrick
 * Copyright (C) 1999-2017 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#define cas(F, O, N) ((__sync_val_compare_and_swap(F, O, N)))

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

  /* If src does not reference an object, then no need to check for
   * down-pointers. */


  HM_HierarchicalHeap dstHH = HM_getLevelHeadPathCompress(HM_getChunkOf(dstp));


  objptr readVal = *field;
  pointer currp = objptrToPointer(readVal, NULL);

  // bool saveReadVal = false;

  if (isObjptr(readVal) && s->wsQueueTop!=BOGUS_OBJPTR) {
    // check for the case where this is laggy
    // uint64_t topval = *(uint64_t*)objptrToPointer(s->wsQueueTop, NULL);
    // uint32_t shallowestPrivateLevel = UNPACK_IDX(topval);
    // uint32_t minDepth = (shallowestPrivateLevel>0)?(shallowestPrivateLevel-1):0;

    // Need to remember for all levels
    HM_HierarchicalHeap currHH = HM_getLevelHeadPathCompress(HM_getChunkOf(currp));

    bool z = cas(&(dstHH->concurrentPack->isCollecting), false, false);
    assert(!z);

    if(currHH == dstHH) {
      printf("%s\n", "storing this");
      HM_HH_addRootForCollector(dstHH, currp);
    }
    //   bool saveReadVal = false;
    //   if(!isCasInst) {
    //     objptr old = cas(field, readVal, dst);

    //     if(old == readVal){
    //       // this means the first cas succeeded. Therefore, this write needs to save the pointer.
    //       saveReadVal = true;
    //     }
    //     else {
    //       while(old!=readVal) {
    //         readVal = old;
    //         old = cas(field, readVal, dst);
    //       }
    //     }
    //   }

    //   else {
    //     objptr old = cas (field, readVal, dst);
    //   }

    //   //  If the input instruction is a cas, then it must succeed and there is no need to check(?).
    //   if(saveReadVal || isCasInst) {
    //     HM_HH_addRootForCollector(srcHH, currp);
    //   }
  }


  if (!isObjptr(src))
    return;

  pointer srcp = objptrToPointer(src, NULL);
  HM_HierarchicalHeap srcHH = HM_getLevelHeadPathCompress(HM_getChunkOf(srcp));
  // if (dstHH->depth >= srcHH->depth) {
  //   if(HM_HH_isCCollecting(srcHH)) {
  //     if (!CC_isPointerMarked(srcp)) {
  //       HM_HH_addRootForCollector(srcHH, src);
  //     }
  //   }
  // }
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

  if (hh == NULL)
  {
    LOG(LM_HH_PROMOTION, LL_WARNING,
      "Write down pointer without local hierarchical heap: "FMTOBJPTR " to "FMTOBJPTR,
      dst, src);
    return;
  }
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





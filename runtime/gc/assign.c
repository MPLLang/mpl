/* Copyright (C) 2019-2021 Sam Westrick
 * Copyright (C) 1999-2017 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */
#ifdef DETECT_ENTANGLEMENT

objptr Assignable_decheckObjptr(objptr dst, objptr src)
{
  GC_state s = pthread_getspecific(gcstate_key);
  s->cumulativeStatistics->numDisentanglementChecks++;
  objptr new_src = src;
  pointer dstp = objptrToPointer(dst, NULL);
  HM_HierarchicalHeap dstHH = HM_getLevelHead(HM_getChunkOf(dstp));

  if (!isObjptr(src) || HM_HH_getDepth(dstHH) == 0 || !ES_contains(NULL, dst))
  {
    return src;
  }

  HM_EBR_leaveQuiescentState(s);
  if (!decheck(s, src))
  {
    assert (isMutable(s, dstp));
    new_src = manage_entangled(s, src, getThreadCurrent(s)->decheckState);
    assert (isPinned(new_src));
  }
  HM_EBR_enterQuiescentState(s);
  assert (!hasFwdPtr(objptrToPointer(new_src, NULL)));
  return new_src;
}

objptr Assignable_readBarrier(
  GC_state s,
  objptr obj,
  objptr *field)
{
// can't rely on obj header becaues it may be forwarded.

  s->cumulativeStatistics->numDisentanglementChecks++;
  objptr ptr = *field;
  pointer objp = objptrToPointer(obj, NULL);
  HM_HierarchicalHeap objHH = HM_getLevelHead(HM_getChunkOf(objp));
  if (!isObjptr(ptr) || HM_HH_getDepth(objHH) == 0 || !ES_contains(NULL, obj))
  {
    return ptr;
  }
  HM_EBR_leaveQuiescentState(s);
  if (!decheck(s, ptr))
  {
    assert (ES_contains(NULL, obj));
    // assert (isMutable(s, obj));
    // if (!ES_contains(NULL, obj))
    // {
    //   if (!decheck(s, obj)) {
    //     assert (false);
    //   }
    //   assert(isPinned(ptr));
    //   assert(!hasFwdPtr(ptr));
    //   assert(pinType(getHeader(ptr)) == PIN_ANY);
    // }
    ptr = manage_entangled(s, ptr, getThreadCurrent(s)->decheckState);
  }
  HM_EBR_enterQuiescentState(s);
  assert (!hasFwdPtr(objptrToPointer(ptr, NULL)));

  return ptr;
}

#else

objptr Assignable_decheckObjptr(objptr dst, objptr src) {
  (void) dst;
  return src;
}

objptr Assignable_readBarrier(
  GC_state s,
  objptr obj,
  objptr *field) {
  (void)s;
  (void)obj;
  return *field;
}
#endif

static inline bool decheck_opt_fast (GC_state s, pointer p) {
  HM_HierarchicalHeap hh = HM_getLevelHead(HM_getChunkOf(p));
  return (hh->depth <= 1) || hh == getThreadCurrent(s)->hierarchicalHeap;
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

  assert (!hasFwdPtr(dstp));
  assert (!isObjptr(src) || !hasFwdPtr(srcp));

// #if ASSERT
//   // check that field is actually inside this object
//   GC_header header = getHeader(dstp);
//   GC_objectTypeTag tag;
//   uint16_t bytesNonObjptrs;
//   uint16_t numObjptrs;
//   bool hasIdentity;
//   splitHeader(s, header, &tag, &hasIdentity, &bytesNonObjptrs, &numObjptrs);
//   pointer objend = dstp;
//   if (!hasIdentity) {
//     DIE("write barrier: attempting to modify immutable object "FMTOBJPTR, dst);
//   }
//   if (NORMAL_TAG == tag) {
//     objend += bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
//   }
//   else if (SEQUENCE_TAG == tag) {
//     size_t dataBytes = getSequenceLength(dstp) * (bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE));
//     objend += alignWithExtra (s, dataBytes, GC_SEQUENCE_METADATA_SIZE);
//   }
//   else {
//     DIE("write barrier: cannot handle tag %u", tag);
//   }
//   pointer fieldp = (pointer)field;
//   ASSERTPRINT(
//     dstp <= fieldp && fieldp + OBJPTR_SIZE <= objend,
//     "write barrier: objptr field %p outside object "FMTOBJPTR" of size %zu",
//     (void*)field,
//     dst,
//     (size_t)(objend - dstp));
// #endif

  HM_HierarchicalHeap dstHH = HM_getLevelHead(HM_getChunkOf(dstp));

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
  if (dst == s->wsQueue) {
    return;
  }

  HM_HierarchicalHeap srcHH = HM_getLevelHead(HM_getChunkOf(srcp));
  if (srcHH == dstHH) {
    /* internal pointers are always traced */
    return;
  }

  uint32_t dd = dstHH->depth;
  bool src_de = decheck_opt_fast(s, srcp) || decheck(s, src);
  if (src_de) {
    bool dst_de = decheck_opt_fast(s, dstp) || decheck(s, dst);
    if (dst_de) {
      uint32_t sd = srcHH->depth;
      /* up pointer (snapshotted by the closure)
       * or internal (within a chain) pointer to a snapshotted heap
       */
      if(dd > sd ||
        ((HM_HH_getConcurrentPack(srcHH)->ccstate != CC_UNREG) &&
        dd == sd))
      {
        return;
      }

      uint32_t unpinDepth = dd;
      bool success = pinObject(src, unpinDepth, PIN_DOWN);

      if (success || dd == unpinDepthOf(src))
      {
        struct HM_remembered remElem_ = {.object = src, .from = dst};
        HM_remembered remElem = &remElem_;

        HM_HierarchicalHeap shh = HM_HH_getHeapAtDepth(s, getThreadCurrent(s), sd);
        assert(NULL != shh);
        assert(HM_HH_getConcurrentPack(shh)->ccstate == CC_UNREG);

        HM_HH_rememberAtLevel(shh, remElem, false);
        LOG(LM_HH_PROMOTION, LL_INFO,
            "remembered downptr %" PRIu32 "->%" PRIu32 " from " FMTOBJPTR " to " FMTOBJPTR,
            dstHH->depth, srcHH->depth,
            dst, src);
      }

      if (dd > 0 && !ES_contains(NULL, dst)) {
        /*if dst is not a suspect, it must be disentangled*/
        // if (!dst_de) {
        //   printf("problematix: %p \n", dst);
        //   DIE("done");
        // }
        // assert (dst_de);
        HM_HierarchicalHeap dhh = HM_HH_getHeapAtDepth(s, getThreadCurrent(s), dd);
        ES_add(s, HM_HH_getSuspects(dhh), dst);
      }
    }
    else if(dstHH->depth != 0) {
      // traverseAndCheck(s, &dst, dst, NULL);
      manage_entangled (s, src, HM_getChunkOf(dstp)->decheckState);
    }


    // if (!dst_de) {
    //   assert (ES_contains(NULL, dst));
    // }

    /* Depth comparisons make sense only when src && dst are on the same root-to-leaf path,
     * checking this maybe expensive, so we approximate here.
     * If both dst_de && src_de hold, they are on the same path
     * Otherwise, we assume they are on different paths.
     */




    // /* otherwise pin*/
    // // bool primary_down_ptr = dst_de && dd < sd && (HM_HH_getConcurrentPack(dstHH)->ccstate == CC_UNREG);
    //  = dst_de ? dd :
    //   (uint32_t)lcaHeapDepth(HM_getChunkOf(srcp)->decheckState,
    //     HM_getChunkOf(dstp)->decheckState);
    // enum PinType pt = dst_de ? PIN_DOWN : PIN_ANY;

    // bool success = pinTemp(s, src, unpinDepth, pt);
    // if (success || (dst_de && dd == unpinDepthOf (src))) {
    //   objptr fromObj = pt == PIN_DOWN ? dst : BOGUS_OBJPTR;
    //   struct HM_remembered remElem_ = {.object = src, .from = fromObj};
    //   HM_remembered remElem = &remElem_;

    //   HM_HierarchicalHeap shh = HM_HH_getHeapAtDepth(s, getThreadCurrent(s), sd);
    //   assert(NULL != shh);
    //   assert(HM_HH_getConcurrentPack(shh)->ccstate == CC_UNREG);

    //   HM_HH_rememberAtLevel(shh, remElem, false);
    //   LOG(LM_HH_PROMOTION, LL_INFO,
    //     "remembered downptr %"PRIu32"->%"PRIu32" from "FMTOBJPTR" to "FMTOBJPTR,
    //     dstHH->depth, srcHH->depth,
    //     dst, src);
    // }

    // /*add dst to the suspect set*/
    // if (dd > 0 && dst_de && !ES_contains(NULL, dst)) {
    //   /*if dst is not a suspect, it must be disentangled*/
    //   // if (!dst_de) {
    //   //   printf("problematix: %p \n", dst);
    //   //   DIE("done");
    //   // }
    //   // assert (dst_de);
    //   HM_HierarchicalHeap dhh = HM_HH_getHeapAtDepth(s, getThreadCurrent(s), dd);
    //   ES_add(s, HM_HH_getSuspects(dhh), dst);
    // }
  } else {
    // assert (isPinned(src));
    // assert (!hasFwdPtr(srcp));
    // assert (pinType(getHeader(srcp)) == PIN_ANY);
    traverseAndCheck(s, &src, src, NULL);
  }


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
      HM_HH_rememberAtLevel(getThreadCurrent(s)->hierarchicalHeap, remElem);
    }

    return;
  }
#endif


  /* Up-pointer. */
  // if (dstHH->depth > srcHH->depth)
  //   return;

  /* Internal pointer. It's safe to ignore an internal pointer if:
   *   1. it's contained entirely within one subheap, or
   *   2. the pointed-to object (src) lives in an already snapshotted subregion
   */
  // if ( (dstHH == srcHH) ||
  //      (dstHH->depth == srcHH->depth &&
  //        HM_HH_getConcurrentPack(srcHH)->ccstate != CC_UNREG) ) {
  //   // assert(...);
  //   // if (dstHH != srcHH) {
  //   //   printf(
  //   //     "ignore internal pointer "FMTPTR" --> "FMTPTR". dstHH == srcHH? %d\n",
  //   //     (uintptr_t)dstp,
  //   //     (uintptr_t)srcp,
  //   //     srcHH == dstHH);
  //   // }
  //   return;
  // }
  /** Otherwise, its a down-pointer, so
    *  (i) make dst a suspect for entanglement, i.e., mark the suspect bit of dst's header
    *      (see pin.h for header-layout).
    *      the compiler checks this suspect bit and calls the read-barrier
    *      only when the bit is set.
    *  (ii) pin the src object
    *  (iii) remember the down pointer
    */

  /* make dst a suspect for entanglement */
  // uint32_t dd = dstHH->depth;
  // if (dd > 0 && !ES_contains(NULL, dst)) {
  //   HM_HierarchicalHeap dhh = HM_HH_getHeapAtDepth(s, thread, dd);
  //   ES_add(s, HM_HH_getSuspects(dhh), dst);
  // }

  // if (decheck(s, src)) {
  //   uint32_t sd = srcHH->depth;
  //   bool dst_de = decheck(s, dst);
  //   assert (dd <= sd);
  //   bool true_down_ptr = dd < sd && (HM_HH_getConcurrentPack(dstHH)->ccstate == CC_UNREG) && dst_de;
  //   // bool unpinDepth = dst_de ? dd : lcaDepth(srcHH->tid, dstHH->tid);
  //   /* treat a pointer from a chained heap as a cross pointer */
  //   bool success = pinObject(src, dd, true_down_ptr ? PIN_DOWN : PIN_ANY);
  //   if (success)
  //   {
  //     HM_HierarchicalHeap shh = HM_HH_getHeapAtDepth(s, thread, sd);
  //     assert(NULL != shh);
  //     assert(HM_HH_getConcurrentPack(shh)->ccstate == CC_UNREG);
  //     HM_HH_rememberAtLevel(shh, remElem, false);

  //     LOG(LM_HH_PROMOTION, LL_INFO,
  //       "remembered downptr %"PRIu32"->%"PRIu32" from "FMTOBJPTR" to "FMTOBJPTR,
  //       dstHH->depth, srcHH->depth,
  //       dst, src);
  //   }
  //   if (!dst_de)
  //   {
  //     DIE("HAVE TO HANDLE ENTANGLED WRITES SEPARATELY");
  //     // HM_HierarchicalHeap lcaHeap = HM_HH_getHeapAtDepth(s, thread, unpinDepth);
  //     // ES_add(s, HM_HH_getSuspects(lcaHeap), ptr);
  //   }
  // }


  // // any concurrent pin can only decrease unpinDepth
  // assert(unpinDepth <= dd);

  // bool maybe_across_chain = write_de && (dd == unpinDepth) && (dd == sd);


  /* SAM_NOTE: TODO: track bytes allocated here in
   * thread->bytesAllocatedSinceLast...? */
}

// void Assignable_updateBarrier (GC_state s, objptr dst, objptr* field, objptr src) {
//   Assignable_writeBarrier(s, dst, field, src, false);
//   // *field = src;
// }
// void Assignable_casBarrier (objptr dst, objptr* field, objptr src) {
//   GC_state s = pthread_getspecific(gcstate_key);
//   Assignable_writeBarrier(s, dst, field, src);
//   // cas(field, (*field), dst); //return?
// }





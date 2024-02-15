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

  // HM_EBR_leaveQuiescentState(s);
  if (!decheck(s, src))
  {
    assert (isMutable(s, dstp));
    s->cumulativeStatistics->numEntanglements++;
    new_src = manage_entangled(s, src, getThreadCurrent(s)->decheckState);
    assert (isPinned(new_src));
  }
  // HM_EBR_enterQuiescentState(s);
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
  objptr ptr = __atomic_load_n(field, __ATOMIC_ACQUIRE);
  pointer objp = objptrToPointer(obj, NULL);
  HM_HierarchicalHeap objHH = HM_getLevelHead(HM_getChunkOf(objp));
  if (!isObjptr(ptr) || HM_HH_getDepth(objHH) == 0 || !ES_contains(NULL, obj))
  {
    return ptr;
  }
  // HM_EBR_leaveQuiescentState(s);
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
    s->cumulativeStatistics->numEntanglements++;
    ptr = manage_entangled(s, ptr, getThreadCurrent(s)->decheckState);
  }
  // HM_EBR_enterQuiescentState(s);
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
  objptr* field,
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

  objptr readVal = __atomic_load_n(field, __ATOMIC_ACQUIRE);
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
      bool success = pinObject(s, src, unpinDepth, PIN_DOWN);

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
        HM_HierarchicalHeap dhh = HM_HH_getHeapAtDepth(s, getThreadCurrent(s), dd);
        ES_add(s, HM_HH_getSuspects(dhh), dst);
      }
    }
    else if(dstHH->depth != 0) {
      s->cumulativeStatistics->numEntanglements++;
      objptr newsrc = manage_entangled (s, src, HM_getChunkOf(dstp)->decheckState);

      // this better be true... otherwise everything is going to explode
      assert(newsrc == src);
    }

  } else {
    traverseAndCheck(s, &src, src, NULL);
  }

}


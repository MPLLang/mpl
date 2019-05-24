/* Copyright (C) 2019 Sam Westrick
 * Copyright (C) 1999-2017 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

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
  else if (ARRAY_TAG == tag) {
    size_t dataBytes = getArrayLength(dstp) * (bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE));
    objend += alignWithExtra (s, dataBytes, GC_ARRAY_METADATA_SIZE);
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

  HM_chunkList dstList = HM_getLevelHeadPathCompress(HM_getChunkOf(dstp));

  pointer srcp = objptrToPointer(src, NULL);
  HM_chunkList srcList = HM_getLevelHeadPathCompress(HM_getChunkOf(srcp));

  /* This creates a down pointer; must be remembered. */
  if (dstList->level < srcList->level) {
    if (dst != s->wsQueue) {
      // assert(getHierarchicalHeapCurrent(s) != NULL);
      struct HM_HierarchicalHeap* hh = getHierarchicalHeapCurrent(s);
      if (hh == NULL) {
        LOG(LM_HH_PROMOTION, LL_WARNING,
          "Write down pointer without local hierarchical heap: "FMTOBJPTR " to "FMTOBJPTR,
          dst, src);
      } else {
        Word32 level = srcList->level;
        if (NULL == HM_HH_LEVEL(hh, level)) {
          HM_HH_LEVEL(hh, level) = HM_newChunkList(hh, level);
        }
        HM_rememberAtLevel(HM_HH_LEVEL(hh, level), dst, field, src);
      }
    }
  }
}

#if ASSERT
void assertObjptrDisentangledForMe(GC_state s, objptr op) {
  if (HM_inGlobalHeap(s) || !isObjptr(op)) return;

  /* Don't call HM_getChunkOf() here, because it does additional asserts that
   * we don't want. */
  HM_chunk objectChunk = (HM_chunk)blockOf(objptrToPointer(op, NULL));
  struct HM_HierarchicalHeap* hh = getHierarchicalHeapCurrent(s);
  assert(hh != NULL);

  /* Search all chunks in my own hierarchical heap. Off-by-one loop to
   * prevent underflow. */
  for (Word32 i = hh->level+1; i > 0; i--) {
    HM_chunkList list = hh->levels[i-1];
    if (list == NULL) continue;
    for (HM_chunk cursor = HM_getChunkListFirstChunk(list);
         cursor != NULL;
         cursor = cursor->nextChunk) {
      if (cursor == objectChunk) return;
    }
  }

  /* Search accessible chunks of each parent hh */
  while (hh->parentHH != NULL) {
    struct HM_HierarchicalHeap* phh = hh->parentHH;
    assert(hh->stealLevel != HM_HH_INVALID_LEVEL);
    Word32 start = hh->stealLevel+1;
    Word32 stop = (phh->stealLevel == HM_HH_INVALID_LEVEL ? 1 : phh->stealLevel+1);
    /* off-by-one to prevent underflow. This executes the body for each
     * stop <= i < start, but in decreasing order. */
    for (Word32 i = start; i > stop; i--) {
      HM_chunkList list = phh->levels[i-1];
      if (list == NULL) continue;
      for (HM_chunk cursor = HM_getChunkListFirstChunk(list);
           cursor != NULL;
           cursor = cursor->nextChunk) {
        if (cursor == objectChunk) return;
      }
    }
    hh = phh;
  }

  /* Check that it's in the global heap. */
  /* SAM_NOTE: This is concurrent access with other processor's manipulation
   * of their own global heap chunks, but on x86 I believe it should be safe.
   */
  for (int i = 0; i < s->numberOfProcs; i++) {
    HM_chunkList list = s->procStates[i].globalHeap;
    for (HM_chunk cursor = HM_getChunkListFirstChunk(list);
         cursor != NULL;
         cursor = cursor->nextChunk) {
      if (cursor == objectChunk) return;
    }
  }

  /* None of my ancestors chunks contain this objptr */
  DIE("entanglement detected: object "FMTOBJPTR" in chunk %p",
      op,
      (void*)objectChunk);

  return;
}
#endif /* ASSERT */

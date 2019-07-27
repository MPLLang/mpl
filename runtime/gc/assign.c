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

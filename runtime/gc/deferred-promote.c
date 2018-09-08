/* Copyright (C) 2018 Sam Westrick.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#include "remembered-set.h"
#include "hierarchical-heap-collection.h"

void bucketIfValid(GC_state s, objptr dst, objptr* field, objptr src, void* args);
void promoteDownPtr(GC_state s, objptr dst, objptr* field, objptr src, void* rawArgs);
void promoteIfPointingDownIntoLocalScope(GC_state s, objptr* field, void* rawArgs);

/* ========================================================================= */

void HM_deferredPromote(GC_state s, struct ForwardHHObjptrArgs* args) {
  LOG(LM_HH_COLLECTION, LL_DEBUG, "START deferred promotion");

  /* First, bucket in-scope downptrs by the level of the downptr origin */
  HM_chunkList downPtrs[HM_MAX_NUM_LEVELS];
  for (Word32 i = 0; i < HM_MAX_NUM_LEVELS; i++) {
    downPtrs[i] = NULL;
  }
  FOR_LEVEL_IN_RANGE(level, i, args->hh, args->minLevel, args->maxLevel+1, {
    HM_foreachRemembered(
      s,
      level->rememberedSet,
      bucketIfValid,
      &(downPtrs[0]));
  });

  /* Next, promote objects to the appropriate level, beginning at the root and
   * working downwards towards to the leaf. Any resulting downptr that cannot
   * be promoted yet (due to pointing to a shared object) is recorded in the
   * appropriate remembered-set */
  for (Word32 i = 0; i <= args->maxLevel; i++) {
    /* remember where the roots begin, so we know where to scan from after
     * promoting the roots */
    HM_chunk rootsBeginChunk = NULL;
    pointer rootsBegin = NULL;
    if (NULL != HM_HH_LEVEL(args->hh, i) &&
        NULL != HM_getChunkListLastChunk(HM_HH_LEVEL(args->hh, i))) {
      rootsBeginChunk = HM_getChunkListLastChunk(HM_HH_LEVEL(args->hh, i));
      rootsBegin = HM_getChunkFrontier(rootsBeginChunk);
    }

    /* promote the roots, as indicated by the remembered downptrs */
    args->toLevel = i;
    HM_foreachRemembered(
      s,
      downPtrs[i],
      promoteDownPtr,
      args);

    if (NULL == HM_HH_LEVEL(args->hh, i)) {
      /* No promotions occurred, so no forwardings necessary here. */
      continue;
    }

    /* forward transitively reachable objects within local scope */
    if (rootsBegin == NULL) {
      rootsBeginChunk = HM_getChunkListFirstChunk(HM_HH_LEVEL(args->hh, i));
      rootsBegin = HM_getChunkStart(rootsBeginChunk);
    }

    /* SAM_NOTE: TODO:
     * for each object beginning at rootsBegin, for each objptr field of that
     * object, call
     *   promoteIfPointingDownIntoLocalScope(..., object, field, ...) */
    HM_forwardHHObjptrsInChunkList(
      s,
      rootsBeginChunk,
      rootsBegin,
      trueObjptrPredicate,
      NULL,
      promoteIfPointingDownIntoLocalScope,
      args);
  }

  /* Finally, free the chunks that we used as temporary storage for bucketing */
  for (Word32 i = 0; i < HM_MAX_NUM_LEVELS; i++) {
    HM_appendChunkList(s->freeListSmall, downPtrs[i]);
  }

  LOG(LM_HH_COLLECTION, LL_DEBUG, "END deferred promotion");
}

void bucketIfValid(GC_state s, objptr dst, objptr* field, objptr src, void* arg) {
  HM_chunkList* downPtrs = arg;
  // pointer dstp = objptrToPointer(dst, NULL);
  // pointer srcp = objptrToPointer(src, NULL);

  assert(!hasFwdPtr(objptrToPointer(dst, NULL)));
  if (*field != src) {
    /* Another write to this location has since invalidated this particular
     * remembered entry. */
    return;
  }

  Word32 dstLevel = HM_getObjptrLevel(dst);
  Word32 srcLevel = HM_getObjptrLevel(src);

  assert(dstLevel <= srcLevel);

  if (dstLevel == srcLevel) {
    /* levels have coincided due to joins, so ignore this entry. */
    return;
  }

  if (downPtrs[dstLevel] == NULL) {
    downPtrs[dstLevel] = HM_newChunkList(NULL, CHUNK_INVALID_LEVEL);
  }

  HM_remember(downPtrs[dstLevel], NULL, dst, field, src);
}

void promoteDownPtr(GC_state s, objptr dst, objptr* field, objptr src, void* rawArgs) {
  struct ForwardHHObjptrArgs* args = (struct ForwardHHObjptrArgs*)rawArgs;
  assert(args->toLevel == HM_getObjptrLevel(dst));

  pointer srcp = objptrToPointer(src, NULL);

  /* It's possible that a previous promotion has already relocated the src object. */
  if (hasFwdPtr(srcp)) {
    assert(!hasFwdPtr(objptrToPointer(getFwdPtr(srcp), NULL)));
    assert(HM_getObjptrLevel(getFwdPtr(srcp)) <= args->toLevel);
    *field = getFwdPtr(srcp);
    return;
  }

  /* Relocation might not change the physical address of the object:
   * for large objects housed in single-object chunks, the chunk is logically
   * (but not physically) moved. It's incorrect to relocate the object again,
   * because this could create an unrecorded downptr. */
  if (HM_getObjptrLevel(src) <= args->toLevel) {
    /* src was logically moved in a previous promotion */
    return;
  }

  if (NULL == HM_HH_LEVEL(args->hh, args->toLevel)) {
    HM_chunkList list = HM_newChunkList(args->hh, args->toLevel);
    /* just need to allocate a valid chunk; the size is arbitrary */
    HM_allocateChunk(list, GC_HEAP_LIMIT_SLOP);
    HM_HH_LEVEL(args->hh, args->toLevel) = list;
  }

  assert(HM_HH_LEVEL(args->hh, args->toLevel) != NULL);
  *field = relocateObject(s, src, HM_HH_LEVEL(args->hh, args->toLevel));
  assert(HM_getObjptrLevel(*field) == args->toLevel);
}

/* SAM_NOTE: TODO: DRY: very similar to promoteDownPtr */
void promoteIfPointingDownIntoLocalScope(GC_state s, objptr* field, void* rawArgs) {
  struct ForwardHHObjptrArgs* args = (struct ForwardHHObjptrArgs*)rawArgs;
  assert(args->toLevel == HM_getObjptrLevel(args->containingObject));

  objptr src = *field;
  pointer srcp = objptrToPointer(src, NULL);

  if (isObjptrInGlobalHeap(s, src)) {
    assert(!hasFwdPtr(srcp));
    return;
  }

  /* Similar to `promoteDownPtr`, it's possible that a previous promotion has
   * already relocated the src object */
  if (hasFwdPtr(srcp)) {
    assert(!hasFwdPtr(objptrToPointer(getFwdPtr(srcp), NULL)));
    assert(HM_getObjptrLevel(getFwdPtr(srcp)) <= args->toLevel);
    *field = getFwdPtr(srcp);
    return;
  }

  Word32 srcLevel = HM_getObjptrLevel(src);
  assert(srcLevel <= args->maxLevel);

  if (srcLevel <= args->toLevel) {
    /* This is an up- or internal- pointer */
    return;
  }

  assert(HM_HH_LEVEL(args->hh, args->toLevel) != NULL);

  if (srcLevel < args->minLevel) {
    /* This is outside local scope; we just need to remember this new downptr */
    HM_rememberAtLevel(HM_HH_LEVEL(args->hh, args->toLevel), args->containingObject, field, src);
    return;
  }

  *field = relocateObject(s, src, HM_HH_LEVEL(args->hh, args->toLevel));
  assert(HM_getObjptrLevel(*field) == args->toLevel);
}

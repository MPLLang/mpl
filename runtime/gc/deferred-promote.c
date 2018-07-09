/* Copyright (C) 2018 Sam Westrick.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#include "remembered-set.h"
#include "hierarchical-heap-collection.h"

void bucketIfValid(GC_state s, objptr dst, objptr* field, objptr src, void* args);
void promoteDownPtr(GC_state s, objptr dst, objptr* field, objptr src, void* rawArgs);

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
    pointer rootsBegin = NULL;
    if (NULL != args->toSpace[i] && NULL != HM_getChunkListLastChunk(args->toSpace[i])) {
      rootsBegin = HM_getChunkFrontier(HM_getChunkListLastChunk(args->toSpace[i]));
    }

    /* promote the roots, as indicated by the remembered downptrs */
    args->toLevel = i;
    HM_foreachRemembered(
      s,
      downPtrs[i],
      promoteDownPtr,
      args);

    /* forward transitively reachable objects within local scope */
    if (rootsBegin == NULL) {
      rootsBegin = HM_getChunkStart(HM_getChunkListFirstChunk(args->toSpace[i]));
    }
    HM_forwardHHObjptrsInChunkList(
      s,
      rootsBegin,
      trueObjptrPredicate,
      NULL,
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
  pointer dstp = objptrToPointer(dst, NULL);
  pointer srcp = objptrToPointer(src, NULL);

  if (hasFwdPtr(dstp)) {
    /* dst has already been forwarded; if src needs to be forwarded too, it
     * will be handled implicitly by the forwarding of dst. */
    return;
  }
  if (*field != src) {
    /* Another write to this location has since invalidated this particular
     * remembered entry. */
    return;
  }

  Word32 dstLevel = HM_getLevelHead(HM_getChunkOf(dstp))->level;
  Word32 srcLevel = HM_getLevelHead(HM_getChunkOf(srcp))->level;

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
  objptr src_copy = src;
  forwardHHObjptr(s, &src_copy, rawArgs);
  *field = src_copy;
  assert(HM_getLevelHead(HM_getChunkOf(objptrToPointer(src_copy, NULL)))->level ==
         HM_getLevelHead(HM_getChunkOf(objptrToPointer(dst, NULL)))->level);
}

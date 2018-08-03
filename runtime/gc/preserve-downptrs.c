/* Copyright (C) 2018 Sam Westrick.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#include "remembered-set.h"
#include "hierarchical-heap-collection.h"

void copyRememberedIfValid(GC_state s, objptr dst, objptr* field, objptr src, void* rawArgs);

void HM_preserveDownPtrs(GC_state s, struct ForwardHHObjptrArgs* args) {
  LOG(LM_HH_COLLECTION, LL_DEBUG, "START deferred promotion");

  FOR_LEVEL_DECREASING_IN_RANGE(level, i, args->hh, args->minLevel, args->hh->level+1, {
    HM_foreachRemembered(
      s,
      level->rememberedSet,
      copyRememberedIfValid,
      args);
  });

  LOG(LM_HH_COLLECTION, LL_DEBUG, "END deferred promotion");
}

void copyRememberedIfValid(GC_state s, objptr dst, objptr* field, objptr src, void* rawArgs) {
  if (hasFwdPtr(objptrToPointer(dst, NULL))) {
    /* dst has already been forwarded; if src needs to be forwarded too, it
     * will be handled implicitly by the forwarding of dst. */
    return;
  }
  if (*field != src) {
    /* Another write to this location has since invalidated this particular
     * remembered entry. */
    return;
  }

  HM_chunkList dstList = HM_getLevelHead(HM_getChunkOf(objptrToPointer(dst, NULL)));
  HM_chunkList srcList = HM_getLevelHead(HM_getChunkOf(objptrToPointer(src, NULL)));

  assert(dstList->level <= srcList->level);

  if (dstList->level == srcList->level) {
    /* levels have coincided due to joins, so ignore this entry. */
    return;
  }

  objptr src_copy = src;

  forwardHHObjptr(s, &src_copy, rawArgs);
  struct ForwardHHObjptrArgs* args = (struct ForwardHHObjptrArgs*)rawArgs;
  assert(NULL != args->toSpace[srcList->level]);
  *field = src_copy;
  HM_rememberAtLevel(args->toSpace[srcList->level], dst, field, src_copy);
}

/* Copyright (C) 2018-2019 Sam Westrick.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#include "remembered-set.h"
#include "hierarchical-heap-collection.h"

void promoteDownPtr(GC_state s, objptr dst, objptr* field, objptr src, void* rawArgs);
void promoteIfPointingDownIntoLocalScope(GC_state s, objptr* field, void* rawArgs);


void bucketIfValid(GC_state s, objptr dst, objptr* field, objptr src, void* args);

/* ========================================================================= */

void HM_deferredPromote(
  GC_state s,
  GC_thread thread,
  HM_chunkList globalDownPtrs,
  struct ForwardHHObjptrArgs* args)
{
  LOG(LM_HH_COLLECTION, LL_DEBUG, "START deferred promotion");

  uint32_t numLevels = args->maxDepth+1;

  /* First, bucket in-scope downptrs by the level of the downptr origin */
  struct HM_chunkList downPtrs[numLevels];
  for (uint32_t i = 0; i < numLevels; i++) {
    HM_initChunkList(&(downPtrs[i]));
  }
  for (HM_HierarchicalHeap cursor = args->hh;
       (NULL != cursor) && (HM_HH_getDepth(cursor) >= args->minDepth);
       cursor = cursor->nextAncestor)
  {
    HM_foreachRemembered(
      s,
      HM_HH_getRemSet(cursor),
      bucketIfValid,
      &(downPtrs[0]));
  }

  /* memoize the fromSpace chunkLists for quick access */
  HM_HierarchicalHeap fromSpace[numLevels];
  for (uint32_t i = 0; i < numLevels; i++) fromSpace[i] = NULL;
  for (HM_HierarchicalHeap cursor = args->hh;
       NULL != cursor;
       cursor = cursor->nextAncestor)
  {
    fromSpace[HM_HH_getDepth(cursor)] = cursor;
  }
  args->fromSpace = &(fromSpace[0]);

  /* Next, promote objects to the appropriate level, beginning below the root and
   * working downwards towards to the leaf. Any resulting downptr that cannot
   * be promoted yet (due to pointing to a shared object) is recorded in the
   * appropriate remembered-set.
   * Note that we skip down-pointers from the root heap; these are "preserved"
   * instead of promoted, and used as roots for collection. */
  for (uint32_t i = 1; i <= args->maxDepth; i++) {
    /* remember where the roots begin, so we know where to scan from after
     * promoting the roots */
    HM_chunk rootsBeginChunk = NULL;
    pointer rootsBegin = NULL;
    if (NULL != fromSpace[i] &&
        NULL != HM_getChunkListLastChunk(HM_HH_getChunkList(fromSpace[i])))
    {
      rootsBeginChunk = HM_getChunkListLastChunk(HM_HH_getChunkList(fromSpace[i]));
      rootsBegin = HM_getChunkFrontier(rootsBeginChunk);
    }

    /* promote the roots, as indicated by the remembered downptrs */
    args->toDepth = i;
    HM_foreachRemembered(
      s,
      &(downPtrs[i]),
      promoteDownPtr,
      args);

    if (NULL == fromSpace[i] ||
        NULL == HM_getChunkListFirstChunk(HM_HH_getChunkList(fromSpace[i])))
    {
      /* No promotions occurred, so no forwardings necessary here. */
      continue;
    }

    /* forward transitively reachable objects within local scope */
    if (rootsBegin == NULL) {
      rootsBeginChunk = HM_getChunkListFirstChunk(HM_HH_getChunkList(fromSpace[i]));
      rootsBegin = HM_getChunkStart(rootsBeginChunk);
    }

    /* for each object beginning at rootsBegin, for each objptr field of that
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

  /* free the chunks that we used as temporary storage for bucketing */
  for (uint32_t i = 1; i < numLevels; i++) {
    HM_appendChunkList(getFreeListSmall(s), &(downPtrs[i]));
  }

  /* reinstantiate the hh linked list from new chunkLists that may have been
   * created during promotion */
  HM_HierarchicalHeap heaps[numLevels];
  for (uint32_t i = 0; i < numLevels; i++) heaps[i] = NULL;
  for (HM_HierarchicalHeap cursor = args->hh;
       NULL != cursor;
       cursor = cursor->nextAncestor)
  {
    heaps[HM_HH_getDepth(cursor)] = cursor;
  }
  HM_HierarchicalHeap prevhh = NULL;
  for (uint32_t i = 0; i < numLevels; i++)
  {
    if (NULL != heaps[i])
    {
      assert(NULL != fromSpace[i]);
      assert(heaps[i] == fromSpace[i]);
      heaps[i]->nextAncestor = prevhh;
      prevhh = heaps[i];
    }
    else if (NULL != fromSpace[i])
    {
      HM_HierarchicalHeap newhh = fromSpace[i];
      newhh->nextAncestor = prevhh;
      prevhh = newhh;
    }
  }
  args->hh = prevhh;
  thread->hierarchicalHeap = prevhh;

  LOG(LM_HH_COLLECTION, LL_DEBUG, "END deferred promotion");

  /* return downPtrs[0] to parent */
  HM_appendChunkList(globalDownPtrs, &(downPtrs[0]));
  return;
}

bool checkValid(objptr dst, objptr* field, objptr src) {
  assert(!hasFwdPtr(objptrToPointer(dst, NULL)));

  if (*field != src) {
    /* Another write to this location has since invalidated this particular
     * remembered entry. */
    return false;
  }

  uint32_t dstDepth = HM_getObjptrDepth(dst);
  uint32_t srcDepth = HM_getObjptrDepth(src);

  assert(dstDepth <= srcDepth);

  if (dstDepth == srcDepth) {
    /* levels have coincided due to joins, so ignore this entry. */
    return false;
  }
  return true;
}

void bucketIfValidAtList(__attribute__((unused)) GC_state s,
                   objptr dst,
                   objptr* field,
                   objptr src,
                   HM_chunkList remSet)
{
  if(checkValid(dst, field, src)) {
    HM_remember(remSet, dst, field, src);
  }
}

void bucketIfValid(__attribute__((unused)) GC_state s,
                   objptr dst,
                   objptr* field,
                   objptr src,
                   void* arg)
{

  struct HM_chunkList* downPtrs = arg;
  uint32_t dstDepth = HM_getObjptrDepth(dst);
  bucketIfValidAtList(s, dst, field, src, &(downPtrs[dstDepth]));
  // if (checkValid(dst, field, src)) {
  //   struct HM_chunkList* downPtrs = arg;
  //   HM_remember(&(downPtrs[dstDepth]), dst, field, src);
  // }
}

void promoteDownPtr(__attribute__((unused)) GC_state s,
                    __attribute__((unused)) objptr dst,
                    objptr* field,
                    objptr src,
                    void* rawArgs)
{
  struct ForwardHHObjptrArgs* args = (struct ForwardHHObjptrArgs*)rawArgs;
  assert(args->toDepth == HM_getObjptrDepth(dst));

  pointer srcp = objptrToPointer(src, NULL);

  /* It's possible that a previous promotion has already relocated the src object. */
  if (hasFwdPtr(srcp)) {
    assert(!hasFwdPtr(objptrToPointer(getFwdPtr(srcp), NULL)));
    assert(HM_getObjptrDepth(getFwdPtr(srcp)) <= args->toDepth);
    *field = getFwdPtr(srcp);
    return;
  }

  /* Relocation might not change the physical address of the object:
   * for large objects housed in single-object chunks, the chunk is logically
   * (but not physically) moved. It's incorrect to relocate the object again,
   * because this could create an unrecorded downptr. */
  if (HM_getObjptrDepth(src) <= args->toDepth) {
    /* src was logically moved in a previous promotion */
    return;
  }

  if (NULL == args->fromSpace[args->toDepth]) {
    /* note that new heaps are initialized with a free chunk. */
    HM_HierarchicalHeap newhh = HM_HH_new(s, args->toDepth);
    args->fromSpace[args->toDepth] = newhh;
  }

  assert(args->fromSpace[args->toDepth] != NULL);
  *field = relocateObject(s, src, args->fromSpace[args->toDepth], args);
  assert(HM_getObjptrDepth(*field) == args->toDepth);
}

/* SAM_NOTE: TODO: DRY: very similar to promoteDownPtr */
void promoteIfPointingDownIntoLocalScope(GC_state s, objptr* field, void* rawArgs) {
  struct ForwardHHObjptrArgs* args = (struct ForwardHHObjptrArgs*)rawArgs;
  assert(args->toDepth == HM_getObjptrDepth(args->containingObject));

  objptr src = *field;
  pointer srcp = objptrToPointer(src, NULL);

  if (isObjptrInRootHeap(s, src)) {
    assert(!hasFwdPtr(srcp));
    return;
  }

  /* Similar to `promoteDownPtr`, it's possible that a previous promotion has
   * already relocated the src object */
  if (hasFwdPtr(srcp)) {
    assert(!hasFwdPtr(objptrToPointer(getFwdPtr(srcp), NULL)));
    //assert(HM_getObjptrDepth(getFwdPtr(srcp)) <= args->toDepth);
    *field = getFwdPtr(srcp);
    return;
  }

  uint32_t srcDepth = HM_getObjptrDepth(src);
  assert(srcDepth <= args->maxDepth);

  if (srcDepth <= args->toDepth) {
    /* This is an up- or internal- pointer */
    return;
  }

  assert(args->fromSpace[args->toDepth] != NULL);

  if (srcDepth < args->minDepth) {
    /* This is outside local scope; we just need to remember this new downptr */
    HM_rememberAtLevel(args->fromSpace[args->toDepth], args->containingObject, field, src);
    return;
  }

  *field = relocateObject(s, src, args->fromSpace[args->toDepth], args);
  assert(HM_getObjptrDepth(*field) == args->toDepth);
}

/* Copyright (C) 2018-2020 Sam Westrick
 * Copyright (C) 2015 Ram Raghunathan.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file hierarchical-heap-collection.c
 *
 * @author Ram Raghunathan
 *
 * This file implements the hierarchical heap collection interface described in
 * hierarchical-heap-collection.h.
 */

#include "hierarchical-heap-collection.h"

/******************************/
/* Static Function Prototypes */
/******************************/

// void forwardDownPtr(GC_state s, objptr dst, objptr* field, objptr src, void* args);

void checkDisentangledDepthAndFreeze(GC_state s, objptr op, void* rawArgs);
void unfreezeDisentangledDepthBefore(GC_state s, objptr op, void* rawArgs);
void unfreezeDisentangledDepthAfter(GC_state s, objptr op, void* rawArgs);

void tryUnpinOrKeepPinned(GC_state s, objptr op, void* rawArgs);
void forwardObjptrsOfRemembered(GC_state s, objptr op, void* rawArgs);
// void scavengeChunkOfPinnedObject(GC_state s, objptr op, void* rawArgs);

#if ASSERT
void checkRememberedEntry(GC_state s, objptr object, void* args);
bool hhContainsChunk(HM_HierarchicalHeap hh, HM_chunk theChunk);
#endif

/**
 * Compute the size of the object, how much of it has to be copied, as well as
 * how much metadata it has.
 *
 * @param s GC state
 * @param p The pointer to copy
 * @param objectSize Where to store the size of the object (in bytes)
 * @param copySize Where to store the number of bytes to copy
 * @param metaDataSize Where to store the metadata size (in bytes)
 *
 * @return the tag of the object
 */
GC_objectTypeTag computeObjectCopyParameters(GC_state s, pointer p,
                                             size_t *objectSize,
                                             size_t *copySize,
                                             size_t *metaDataSize);

pointer copyObject(pointer p,
                   size_t objectSize,
                   size_t copySize,
                   HM_HierarchicalHeap tgtHeap);

/**
 * ObjptrPredicateFunction for skipping stacks and threads in the hierarchical
 * heap.
 *
 * @note This function takes as additional arguments the
 * struct SSATOPredicateArgs
 */
struct SSATOPredicateArgs {
  pointer expectedStackPointer;
  pointer expectedThreadPointer;
};
bool skipStackAndThreadObjptrPredicate(GC_state s,
                                       pointer p,
                                       void* rawArgs);

/************************/
/* Function Definitions */
/************************/
#if (defined (MLTON_GC_INTERNAL_BASIS))
#endif /* MLTON_GC_INTERNAL_BASIS */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

void HM_HHC_collectLocal(uint32_t desiredScope) {
  GC_state s = pthread_getspecific(gcstate_key);
  GC_thread thread = getThreadCurrent(s);
  struct HM_HierarchicalHeap* hh = thread->hierarchicalHeap;

  struct rusage ru_start;
  struct timespec startTime;
  struct timespec stopTime;
  uint64_t oldObjectCopied;

  if (NONE == s->controls->collectionType) {
    /* collection disabled */
    return;
  }

  if (s->wsQueueTop == BOGUS_OBJPTR || s->wsQueueBot == BOGUS_OBJPTR) {
    LOG(LM_HH_COLLECTION, LL_INFO, "Skipping collection, deque not registered yet");
    return;
  }

  uint64_t topval = *(uint64_t*)objptrToPointer(s->wsQueueTop, NULL);
  uint32_t potentialLocalScope = UNPACK_IDX(topval);

  uint32_t originalLocalScope = pollCurrentLocalScope(s);
  uint32_t minDepth = originalLocalScope;
  // claim as many levels as we can, but only as far as desired
  while (minDepth > desiredScope &&
         minDepth > thread->minLocalCollectionDepth &&
         tryClaimLocalScope(s)) {
    minDepth--;
  }

  if (minDepth < thread->minLocalCollectionDepth) {
    LOG(LM_HH_COLLECTION, LL_INFO, "Skipping collection too shallow");
    releaseLocalScope(s, originalLocalScope);
    return;
  }

  if (minDepth > thread->currentDepth) {
    LOG(LM_HH_COLLECTION, LL_INFO,
        "Skipping collection because minDepth > current depth (%u > %u)",
        minDepth,
        thread->currentDepth);
    releaseLocalScope(s, originalLocalScope);
    return;
  }

  uint32_t maxDepth = thread->currentDepth;

  LOG(LM_HH_COLLECTION, LL_DEBUG,
      "START");

  Trace0(EVENT_GC_ENTER);
  TraceResetCopy();

  s->cumulativeStatistics->numHHLocalGCs++;

  /* used needs to be set because the mutator has changed s->stackTop. */
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
  getThreadCurrent(s)->exnStack = s->exnStack;

  assertInvariants(thread);

  if (SUPERLOCAL == s->controls->collectionType) {
    minDepth = maxDepth;
  }

  /* copy roots */
  struct ForwardHHObjptrArgs forwardHHObjptrArgs = {
    .hh = hh,
    .minDepth = minDepth,
    .maxDepth = maxDepth,
    .toDepth = HM_HH_INVALID_DEPTH,
    .fromSpace = NULL,
    .toSpace = NULL,
    .pinned = NULL,
    .containingObject = BOGUS_OBJPTR,
    .bytesCopied = 0,
    .objectsCopied = 0,
    .stacksCopied = 0,
    .bytesMoved = 0,
    .objectsMoved = 0
  };

  struct HM_chunkList pinned[maxDepth+1];
  forwardHHObjptrArgs.pinned = &(pinned[0]);
  for (uint32_t i = 0; i <= maxDepth; i++) HM_initChunkList(&(pinned[i]));

  HM_HierarchicalHeap toSpace[maxDepth+1];
  forwardHHObjptrArgs.toSpace = &(toSpace[0]);
  for (uint32_t i = 0; i <= maxDepth; i++) toSpace[i] = NULL;

  HM_HierarchicalHeap fromSpace[maxDepth+1];
  forwardHHObjptrArgs.fromSpace = &(fromSpace[0]);
  for (uint32_t i = 0; i <= maxDepth; i++) fromSpace[i] = NULL;
  for (HM_HierarchicalHeap cursor = hh;
       NULL != cursor;
       cursor = cursor->nextAncestor) {
    fromSpace[HM_HH_getDepth(cursor)] = cursor;
  }

  /* =====================================================================
   * logging */
  size_t sizesBefore[maxDepth+1];
  for (uint32_t i = 0; i <= maxDepth; i++)
    sizesBefore[i] = 0;
  size_t totalSizeBefore = 0;
  for (HM_HierarchicalHeap cursor = hh;
       NULL != cursor;
       cursor = cursor->nextAncestor)
  {
    uint32_t d = HM_HH_getDepth(cursor);
    size_t sz = HM_getChunkListSize(HM_HH_getChunkList(cursor));
    sizesBefore[d] = sz;
    totalSizeBefore += sz;
  }

  /* ===================================================================== */

  /** When we're managing entanglement carefully, check if we're allowed to
    * do this GC.
    */
  if (s->controls->manageEntanglement) {
    struct checkDEDepthsArgs ddArgs = {
      .minDisentangledDepth = INT32_MAX,
      .fromSpace = forwardHHObjptrArgs.fromSpace,
      .toSpace = forwardHHObjptrArgs.toSpace,
      .maxDepth = forwardHHObjptrArgs.maxDepth
    };

    bool allowedToGC = TRUE;

    for (HM_HierarchicalHeap cursor = hh;
         NULL != cursor && HM_HH_getDepth(cursor) >= minDepth;
         cursor = cursor->nextAncestor)
    {
      HM_foreachRemembered(s, HM_HH_getRemSet(cursor),
        &checkDisentangledDepthAndFreeze, &ddArgs);

      assert(ddArgs.minDisentangledDepth > 0);
      if ((uint32_t)ddArgs.minDisentangledDepth < maxDepth) {
        allowedToGC = FALSE;
        break;
      }
    }

    if (ddArgs.minDisentangledDepth < thread->disentangledDepth) {
      thread->disentangledDepth = ddArgs.minDisentangledDepth;
    }

    if (!allowedToGC) {
      for (HM_HierarchicalHeap cursor = hh;
           NULL != cursor && HM_HH_getDepth(cursor) >= minDepth;
           cursor = cursor->nextAncestor)
      {
        HM_foreachRemembered(s, HM_HH_getRemSet(cursor),
          &unfreezeDisentangledDepthBefore, &ddArgs);
      }

      releaseLocalScope(s, originalLocalScope);
      return;
    }
  }

  /* ===================================================================== */

  /* SAM_NOTE: the name "promotion" is no longer true but these trace
   * events are hardcoded, ugh. */
  Trace0(EVENT_PROMOTION_ENTER);
  timespec_now(&startTime);

  /* For each remembered entry, if possible, unpin and discard the entry.
   * otherwise, copy the remembered entry to the toSpace remembered set. */
  for (HM_HierarchicalHeap cursor = hh;
       NULL != cursor && HM_HH_getDepth(cursor) >= minDepth;
       cursor = cursor->nextAncestor)
  {
    forwardHHObjptrArgs.toDepth = HM_HH_getDepth(cursor);
    HM_foreachRemembered(s, HM_HH_getRemSet(cursor),
      &tryUnpinOrKeepPinned, &forwardHHObjptrArgs);
  }
  forwardHHObjptrArgs.toDepth = HM_HH_INVALID_DEPTH;

  // assertInvariants(thread);

  timespec_now(&stopTime);
  timespec_sub(&stopTime, &startTime);
  timespec_add(&(s->cumulativeStatistics->timeLocalPromo), &stopTime);
  Trace0(EVENT_PROMOTION_LEAVE);

  /* ===================================================================== */

  if (needGCTime(s)) {
    startTiming (RUSAGE_THREAD, &ru_start);
  }

  timespec_now(&startTime);

  LOG(LM_HH_COLLECTION, LL_INFO,
      "collecting hh %p (L: %u):\n"
      "  potential local scope is %u -> %u\n"
      "  collection scope is      %u -> %u\n",
      // "  lchs %"PRIu64" lcs %"PRIu64,
      ((void*)(hh)),
      thread->currentDepth,
      potentialLocalScope,
      thread->currentDepth,
      forwardHHObjptrArgs.minDepth,
      forwardHHObjptrArgs.maxDepth);

  LOG(LM_HH_COLLECTION, LL_DEBUG, "START root copy");

  /* forward contents of stack */
  oldObjectCopied = forwardHHObjptrArgs.objectsCopied;
  foreachObjptrInObject(s,
                        objptrToPointer(getStackCurrentObjptr(s),
                                        NULL),
                        FALSE,
                        trueObjptrPredicate,
                        NULL,
                        forwardHHObjptr,
                        &forwardHHObjptrArgs);
  LOG(LM_HH_COLLECTION, LL_DEBUG,
      "Copied %"PRIu64" objects from stack",
      forwardHHObjptrArgs.objectsCopied - oldObjectCopied);
  Trace3(EVENT_COPY,
	 forwardHHObjptrArgs.bytesCopied,
	 forwardHHObjptrArgs.objectsCopied,
	 forwardHHObjptrArgs.stacksCopied);

  /* forward contents of thread (hence including stack) */
  oldObjectCopied = forwardHHObjptrArgs.objectsCopied;
  foreachObjptrInObject(s,
                        objptrToPointer(getThreadCurrentObjptr(s),
                                        NULL),
                        FALSE,
                        trueObjptrPredicate,
                        NULL,
                        forwardHHObjptr,
                        &forwardHHObjptrArgs);
  LOG(LM_HH_COLLECTION, LL_DEBUG,
      "Copied %"PRIu64" objects from thread",
      forwardHHObjptrArgs.objectsCopied - oldObjectCopied);
  Trace3(EVENT_COPY,
	 forwardHHObjptrArgs.bytesCopied,
	 forwardHHObjptrArgs.objectsCopied,
	 forwardHHObjptrArgs.stacksCopied);

  /* forward thread itself */
  LOG(LM_HH_COLLECTION, LL_DEBUG,
    "Trying to forward current thread %p",
    (void*)s->currentThread);
  oldObjectCopied = forwardHHObjptrArgs.objectsCopied;
  forwardHHObjptr(s, &(s->currentThread), &forwardHHObjptrArgs);
  LOG(LM_HH_COLLECTION, LL_DEBUG,
      (1 == (forwardHHObjptrArgs.objectsCopied - oldObjectCopied)) ?
      "Copied thread from GC_state" : "Did not copy thread from GC_state");
  Trace3(EVENT_COPY,
	 forwardHHObjptrArgs.bytesCopied,
	 forwardHHObjptrArgs.objectsCopied,
	 forwardHHObjptrArgs.stacksCopied);

  /* forward contents of deque */
  oldObjectCopied = forwardHHObjptrArgs.objectsCopied;
  foreachObjptrInObject(s,
                        objptrToPointer(s->wsQueue,
                                        NULL),
                        FALSE,
                        trueObjptrPredicate,
                        NULL,
                        forwardHHObjptr,
                        &forwardHHObjptrArgs);
  LOG(LM_HH_COLLECTION, LL_DEBUG,
      "Copied %"PRIu64" objects from deque",
      forwardHHObjptrArgs.objectsCopied - oldObjectCopied);
  Trace3(EVENT_COPY,
	 forwardHHObjptrArgs.bytesCopied,
	 forwardHHObjptrArgs.objectsCopied,
	 forwardHHObjptrArgs.stacksCopied);

  LOG(LM_HH_COLLECTION, LL_DEBUG, "END root copy");

  /* do copy-collection */
  oldObjectCopied = forwardHHObjptrArgs.objectsCopied;
  /*
   * I skip the stack and thread since they are already forwarded as roots
   * above
   */
  // struct SSATOPredicateArgs ssatoPredicateArgs = {
  //   .expectedStackPointer = objptrToPointer(getStackCurrentObjptr(s),
  //                                           NULL),
  //   .expectedThreadPointer = objptrToPointer(getThreadCurrentObjptr(s),
  //                                            NULL)
  // };

  /* off-by-one to prevent underflow */
  uint32_t depth = thread->currentDepth+1;
  while (depth > forwardHHObjptrArgs.minDepth) {
    depth--;
    HM_HierarchicalHeap toSpaceLevel = toSpace[depth];
    if (NULL == toSpaceLevel) {
      continue;
    }

    LOG(LM_HH_COLLECTION, LL_INFO,
      "level %"PRIu32": num pinned: %zu",
      depth,
      HM_numRemembered(HM_HH_getRemSet(toSpaceLevel)));

    /* use the remembered (pinned) entries at this level as extra roots */
    HM_foreachRemembered(s, HM_HH_getRemSet(toSpaceLevel),
      &forwardObjptrsOfRemembered, &forwardHHObjptrArgs);

    if (NULL != HM_HH_getChunkList(toSpaceLevel)->firstChunk)
    {
      HM_chunkList toSpaceList = HM_HH_getChunkList(toSpaceLevel);
      HM_forwardHHObjptrsInChunkList(
        s,
        toSpaceList->firstChunk,
        HM_getChunkStart(toSpaceList->firstChunk),
        // &skipStackAndThreadObjptrPredicate,
        // &ssatoPredicateArgs,
        &trueObjptrPredicate,
        NULL,
        &forwardHHObjptr,
        &forwardHHObjptrArgs);
    }
  }

  /* after everything has been scavenged, we have to move the pinned chunks */
  depth = thread->currentDepth+1;
  while (depth > forwardHHObjptrArgs.minDepth) {
    depth--;
    HM_HierarchicalHeap toSpaceLevel = toSpace[depth];
    if (NULL == toSpaceLevel) {
      /* check that there are also no pinned chunks at this level
       * (if there was pinned chunk, then we would have also created a
       * toSpace HH at this depth, because we would have scavenged the
       * remembered entry) */
      assert(pinned[depth].firstChunk == NULL);
      continue;
    }

    /* unset the flags on pinned chunks and update their HH pointer */
    for (HM_chunk chunkCursor = pinned[depth].firstChunk;
         chunkCursor != NULL;
         chunkCursor = chunkCursor->nextChunk)
    {
      assert(chunkCursor->pinnedDuringCollection);
      chunkCursor->pinnedDuringCollection = FALSE;
      chunkCursor->levelHead = toSpaceLevel;
    }

    /* put the pinned chunks into the toSpace */
    HM_appendChunkList(HM_HH_getChunkList(toSpaceLevel), &(pinned[depth]));
  }

  LOG(LM_HH_COLLECTION, LL_DEBUG,
      "Copied %"PRIu64" objects in copy-collection",
      forwardHHObjptrArgs.objectsCopied - oldObjectCopied);
  LOG(LM_HH_COLLECTION, LL_DEBUG,
      "Copied %"PRIu64" stacks in copy-collection",
      forwardHHObjptrArgs.stacksCopied);
  Trace3(EVENT_COPY,
	 forwardHHObjptrArgs.bytesCopied,
	 forwardHHObjptrArgs.objectsCopied,
	 forwardHHObjptrArgs.stacksCopied);

  /* ===================================================================== */

  /* Free old chunks and find the tail (upper segment) of the original hh
   * that will be merged with the toSpace */
  HM_HierarchicalHeap hhTail = hh;
  while (NULL != hhTail && HM_HH_getDepth(hhTail) >= minDepth)
  {
    HM_HierarchicalHeap nextAncestor = hhTail->nextAncestor;

    HM_chunkList level = HM_HH_getChunkList(hhTail);
    HM_chunkList remset = HM_HH_getRemSet(hhTail);
    if (NULL != remset) {
#if ASSERT
      /* clear out memory to quickly catch some memory safety errors */
      HM_chunk chunkCursor = remset->firstChunk;
      while (chunkCursor != NULL) {
        pointer start = HM_getChunkStart(chunkCursor);
        size_t length = (size_t)(chunkCursor->limit - start);
        memset(start, 0xBF, length);
        chunkCursor = chunkCursor->nextChunk;
      }
#endif
      HM_appendChunkList(getFreeListSmall(s), remset);
    }

#if ASSERT
    /* clear out memory to quickly catch some memory safety errors */
    HM_chunk chunkCursor = level->firstChunk;
    while (chunkCursor != NULL) {
      pointer start = HM_getChunkStart(chunkCursor);
      size_t length = (size_t)(chunkCursor->limit - start);
      memset(start, 0xBF, length);
      chunkCursor = chunkCursor->nextChunk;
    }
#endif

    /* This implicitly frees the heap records too, because they are stored in
     * the level list. */
    HM_appendChunkList(getFreeListSmall(s), level);

    hhTail = nextAncestor;
  }

  /* Build the toSpace hh */
  HM_HierarchicalHeap hhToSpace = NULL;
  for (uint32_t i = 0; i <= maxDepth; i++)
  {
    if (NULL == toSpace[i])
      continue;

    toSpace[i]->nextAncestor = hhToSpace;
    hhToSpace = toSpace[i];
  }

  /* merge in toSpace */
  hh = HM_HH_zip(hhTail, hhToSpace);
  thread->hierarchicalHeap = hh;
  for (HM_HierarchicalHeap cursor = hh;
       NULL != cursor;
       cursor = cursor->nextAncestor)
  {
    toSpace[HM_HH_getDepth(cursor)] = cursor;
  }

  /* update currentChunk and associated */
  HM_chunk lastChunk = NULL;
  for (HM_HierarchicalHeap cursor = hh;
       NULL != cursor;
       cursor = cursor->nextAncestor)
  {
    if (HM_getChunkListLastChunk(HM_HH_getChunkList(cursor)) != NULL) {
      lastChunk = HM_getChunkListLastChunk(HM_HH_getChunkList(cursor));
      break;
    }
  }
  thread->currentChunk = lastChunk;

  if (lastChunk != NULL && !lastChunk->mightContainMultipleObjects) {
    if (!HM_HH_extend(s, thread, GC_HEAP_LIMIT_SLOP)) {
      DIE("Ran out of space for hierarchical heap!\n");
    }
  }

  /* SAM_NOTE: the following assert is broken, because it is possible that
   * lastChunk == NULL (if we collected everything). Also, note that even when
   * lastChunk != NULL, this assert sometimes trips... which is puzzling,
   * because during collection we are careful to allocate fresh chunks
   * specifically to prevent this.
   *
   * assert(lastChunk->frontier < (pointer)lastChunk + HM_BLOCK_SIZE);
   */


  /** Finally, unfreeze chunks if we need to. */
  if (s->controls->manageEntanglement) {
    struct checkDEDepthsArgs ddArgs = {
      .minDisentangledDepth = INT32_MAX,
      .fromSpace = forwardHHObjptrArgs.fromSpace,
      .toSpace = forwardHHObjptrArgs.toSpace,
      .maxDepth = forwardHHObjptrArgs.maxDepth
    };

    for (HM_HierarchicalHeap cursor = hh;
         NULL != cursor && HM_HH_getDepth(cursor) >= minDepth;
         cursor = cursor->nextAncestor)
    {
      HM_foreachRemembered(s, HM_HH_getRemSet(cursor),
        &unfreezeDisentangledDepthAfter, &ddArgs);
    }
  }


#if ASSERT
  assertInvariants(thread);

  /* some additional assertions for pinned objects */
  for (HM_HierarchicalHeap cursor = hh;
       cursor != NULL && HM_HH_getDepth(cursor) >= minDepth;
       cursor = cursor->nextAncestor) {
    HM_foreachRemembered(s, HM_HH_getRemSet(cursor), &checkRememberedEntry, cursor);
  }
#endif

  s->cumulativeStatistics->bytesHHLocaled += forwardHHObjptrArgs.bytesCopied;

  /* SAM_NOTE: bytesSurvivedLastCollection is more precise than the
   * corresponding bytesAllocatedSinceLastCollection, which granularizes on
   * chunk boundaries.
   *
   * TODO: IS THIS A PROBLEM?
   */
  thread->bytesSurvivedLastCollection =
    forwardHHObjptrArgs.bytesMoved + forwardHHObjptrArgs.bytesCopied;

  thread->bytesAllocatedSinceLastCollection = 0;

  // sizes info and stats
  size_t totalSizeAfter = 0;

  for (HM_HierarchicalHeap cursor = hh;
       NULL != cursor;
       cursor = cursor->nextAncestor)
  {
    uint32_t i = HM_HH_getDepth(cursor);

    HM_chunkList lev = HM_HH_getChunkList(cursor);
    size_t sizeAfter = HM_getChunkListSize(lev);
    totalSizeAfter += sizeAfter;

    if (LOG_ENABLED(LM_HH_COLLECTION, LL_INFO) &&
        (sizesBefore[i] != 0 || sizeAfter != 0))
    {
      size_t sizeBefore = sizesBefore[i];
      const char *sign;
      size_t diff;
      if (sizeBefore > sizeAfter) {
        sign = "-";
        diff = sizeBefore - sizeAfter;
      } else {
        sign = "+";
        diff = sizeAfter - sizeBefore;
      }

      LOG(LM_HH_COLLECTION, LL_INFO,
          "level %u, after collect: %zu --> %zu (%s%zu)",
          i,
          sizeBefore,
          sizeAfter,
          sign,
          diff);
    }
  }

  if (totalSizeAfter > totalSizeBefore) {
    // whoops?
  } else {
    s->cumulativeStatistics->bytesReclaimedByLocal +=
      (totalSizeBefore - totalSizeAfter);
  }

  /* enter statistics if necessary */

  timespec_now(&stopTime);
  timespec_sub(&stopTime, &startTime);
  timespec_add(&(s->cumulativeStatistics->timeLocalGC), &stopTime);

  // if (stopTime.tv_sec >= 1 || stopTime.tv_nsec > 999999999 / 2) {
  //   printf("[WARN] long GC %lld.%.9ld s, %d -> %d, %d\n",
  //     (long long)stopTime.tv_sec,
  //     stopTime.tv_nsec,
  //     (int)minDepth,
  //     (int)maxDepth,
  //     (int)thread->minLocalCollectionDepth);
  // }

  if (needGCTime(s)) {
    if (detailedGCTime(s)) {
      stopTiming(RUSAGE_THREAD, &ru_start, &s->cumulativeStatistics->ru_gcHHLocal);
    }
    /*
     * RAM_NOTE: small extra here since I recompute delta, but probably not a
     * big deal...
     */
    stopTiming(RUSAGE_THREAD, &ru_start, &s->cumulativeStatistics->ru_gc);
  }

  TraceResetCopy();
  Trace0(EVENT_GC_LEAVE);

  LOG(LM_HH_COLLECTION, LL_DEBUG,
      "END");

  releaseLocalScope(s, originalLocalScope);
  return;
}

/* ========================================================================= */

bool isObjptrInToSpace(objptr op, struct ForwardHHObjptrArgs *args)
{
  HM_chunk c = HM_getChunkOf(objptrToPointer(op, NULL));
  HM_HierarchicalHeap levelHead = HM_getLevelHeadPathCompress(c);
  uint32_t depth = HM_HH_getDepth(levelHead);
  assert(depth <= args->maxDepth);
  assert(NULL != levelHead);

  return args->toSpace[depth] == levelHead;
}

/* ========================================================================= */

#if 0
/* SAM_NOTE: TODO: DRY: this code is similar (but not identical) to
 * forwardHHObjptr */
objptr relocateObject(
  GC_state s,
  objptr op,
  HM_HierarchicalHeap tgtHeap,
  struct ForwardHHObjptrArgs *args)
{
  pointer p = objptrToPointer(op, NULL);

  assert(!hasFwdPtr(p));
  assert(HM_HH_isLevelHead(tgtHeap));

  HM_chunkList tgtChunkList = HM_HH_getChunkList(tgtHeap);

  size_t metaDataBytes;
  size_t objectBytes;
  size_t copyBytes;

  /* compute object size and bytes to be copied */
  computeObjectCopyParameters(s,
                              p,
                              &objectBytes,
                              &copyBytes,
                              &metaDataBytes);

  if (!HM_getChunkOf(p)->mightContainMultipleObjects) {
    /* This chunk contains *only* this object, so no need to copy. Instead,
     * just move the chunk. Don't forget to update the levelHead, too! */
    HM_chunk chunk = HM_getChunkOf(p);
    HM_unlinkChunk(HM_HH_getChunkList(HM_getLevelHead(chunk)), chunk);
    HM_appendChunk(tgtChunkList, chunk);
    chunk->levelHead = tgtHeap;

    /* SAM_NOTE: this is inefficient. unnecessary fragmentation. */
    HM_chunk newChunk = HM_allocateChunk(tgtChunkList, GC_HEAP_LIMIT_SLOP);
    if (NULL == newChunk) {
      DIE("Ran out of space for Hierarchical Heap!");
    }
    newChunk->levelHead = tgtHeap;

    LOG(LM_HH_COLLECTION, LL_DEBUGMORE,
      "Moved single-object chunk %p of size %zu",
      (void*)chunk,
      HM_getChunkSize(chunk));
    args->bytesMoved += copyBytes;
    args->objectsMoved++;
    return op;
  }

  pointer copyPointer = copyObject(p - metaDataBytes,
                                   objectBytes,
                                   copyBytes,
                                   tgtHeap);

  /* Store the forwarding pointer in the old object metadata. */
  *(getFwdPtrp(p)) = pointerToObjptr (copyPointer + metaDataBytes,
                                      NULL);
  assert (hasFwdPtr(p));

  args->bytesCopied += copyBytes;
  args->objectsCopied++;

  /* use the forwarding pointer */
  return getFwdPtr(p);
}
#endif

/* ========================================================================= */

#if 0
void forwardDownPtr(GC_state s, objptr dst, objptr* field, objptr src, void* rawArgs) {
  struct ForwardHHObjptrArgs* args = (struct ForwardHHObjptrArgs*)rawArgs;
  uint32_t srcDepth = HM_getObjptrDepth(src);

  assert(args->minDepth <= srcDepth);
  assert(srcDepth <= args->maxDepth);
  assert(args->toDepth == HM_HH_INVALID_DEPTH);

  forwardHHObjptr(s, &src, rawArgs);
  assert(NULL != args->toSpace[srcDepth]);

  *field = src;
  HM_rememberAtLevel(args->toSpace[srcDepth], dst, field, src);
}
#endif

/* ========================================================================= */

void checkDisentangledDepthAndFreeze(
  __attribute__((unused)) GC_state s,
  objptr op,
  void* rawArgs)
{
  assert(isPinned(op));
  HM_chunk chunk = HM_getChunkOf(objptrToPointer(op, NULL));
  HM_HierarchicalHeap hh = HM_getLevelHead(chunk);
  struct checkDEDepthsArgs *args = rawArgs;
  uint32_t opDepth = HM_HH_getDepth(hh);

  /** If it's not in our from-space, then it's entangled, so skip it.
    * (Note: we can't just check if it's entangled, because even if it's
    * entangled, we might still have it in our local heap...)
    */
  if (opDepth > args->maxDepth || args->fromSpace[opDepth] != hh) {
    assert(s->controls->manageEntanglement);
    /** TODO: assert entangled here */
    return;
  }

  assert(hhContainsChunk(args->fromSpace[opDepth], chunk));

  if (opDepth <= unpinDepthOf(op)) {
    /* don't freeze anything that is going to get unpinned! */
    return;
  }

  int32_t thisDD = atomicLoadS32(&(chunk->disentangledDepth));
  while (thisDD > 0) {
    if (__sync_bool_compare_and_swap(&(chunk->disentangledDepth), thisDD, -thisDD))
      break;
    thisDD = atomicLoadS32(&(chunk->disentangledDepth));
  }
  assert(chunk->disentangledDepth < 0);
  thisDD = -(chunk->disentangledDepth);
  if (thisDD < args->minDisentangledDepth)
    args->minDisentangledDepth = thisDD;
}

void unfreezeDisentangledDepthBefore(
  __attribute__((unused)) GC_state s,
  objptr op,
  void* rawArgs)
{
  assert(isPinned(op));
  HM_chunk chunk = HM_getChunkOf(objptrToPointer(op, NULL));
  HM_HierarchicalHeap hh = HM_getLevelHead(chunk);
  struct checkDEDepthsArgs *args = rawArgs;
  uint32_t opDepth = HM_HH_getDepth(hh);

  /** If it's not in our from-space, then it's entangled, so skip it.
    * (Note: we can't just check if it's entangled, because even if it's
    * entangled, we might still have it in our local heap...)
    */
  if (opDepth > args->maxDepth || args->fromSpace[opDepth] != hh) {
    assert(s->controls->manageEntanglement);
    /** TODO: assert entangled here */
    return;
  }

  assert(hhContainsChunk(args->fromSpace[opDepth], chunk));
  assert(chunk->disentangledDepth != 0);

  if (chunk->disentangledDepth < 0) {
    chunk->disentangledDepth = -(chunk->disentangledDepth);
  }
}

void unfreezeDisentangledDepthAfter(
  __attribute__((unused)) GC_state s,
  objptr op,
  void* rawArgs)
{
  assert(isPinned(op));
  HM_chunk chunk = HM_getChunkOf(objptrToPointer(op, NULL));
  HM_HierarchicalHeap hh = HM_getLevelHead(chunk);
  struct checkDEDepthsArgs *args = rawArgs;
  uint32_t opDepth = HM_HH_getDepth(hh);

  /** If it's not in our to-space (now we're AFTER collection completed),
    * then it's entangled, so skip it.
    */
  if (opDepth > args->maxDepth || args->toSpace[opDepth] != hh) {
    assert(s->controls->manageEntanglement);
    /** TODO: assert entangled here */
    return;
  }

  assert(hhContainsChunk(args->toSpace[opDepth], chunk));
  assert(chunk->disentangledDepth != 0);

  if (chunk->disentangledDepth < 0) {
    chunk->disentangledDepth = -(chunk->disentangledDepth);
  }
}

/* ========================================================================= */

void tryUnpinOrKeepPinned(GC_state s, objptr op, void* rawArgs) {
  assert(isPinned(op));
  struct ForwardHHObjptrArgs* args = (struct ForwardHHObjptrArgs*)rawArgs;

  /* We could just look up the depth of `op`, with the normal technique
   * (getLevelHead, etc.), but this should be faster. The toDepth field
   * is set by the loop that calls this function */
  uint32_t opDepth = args->toDepth;
  HM_chunk chunk = HM_getChunkOf(objptrToPointer(op, NULL));
  HM_HierarchicalHeap hh = HM_getLevelHead(chunk);

  if (NULL == args->toSpace[opDepth]) {
    /** BUG HERE. chunk->decheckState is WRONG for entangled shit. */
    args->toSpace[opDepth] = HM_HH_new(s, opDepth, chunk->decheckState);
  }

  /** If it's not in our from-space, then it's entangled.
    * KEEP THE ENTRY but don't do any of the other nasty stuff.
    */
  if (opDepth > args->maxDepth || args->fromSpace[opDepth] != hh) {
    assert(s->controls->manageEntanglement);
    /** TODO: assert entangled here */

    HM_remember(HM_HH_getRemSet(args->toSpace[opDepth]), op);
    return;
  }

  assert(HM_getObjptrDepth(op) == opDepth);

  if (opDepth <= unpinDepthOf(op)) {
    unpinObject(op);
    return;
  }

  /* otherwise, object stays pinned. we have to scavenge this remembered
   * entry into the toSpace. */

  HM_remember(HM_HH_getRemSet(args->toSpace[opDepth]), op);

  if (chunk->pinnedDuringCollection) {
    return;
  }

  chunk->pinnedDuringCollection = TRUE;
  assert(hhContainsChunk(args->fromSpace[opDepth], chunk));
  assert(HM_getLevelHead(chunk) == args->fromSpace[opDepth]);

  /* SAM_NOTE: unlinkChunk unsets levelHead, which is a little strange. */
  HM_unlinkChunk(HM_HH_getChunkList(args->fromSpace[opDepth]), chunk);
  HM_appendChunk(&(args->pinned[opDepth]), chunk);
  chunk->levelHead = args->fromSpace[opDepth];
}

/* ========================================================================= */

#if 0
void scavengeChunkOfPinnedObject(
  GC_state s,
  objptr op,
  void* rawArgs)
{
  assert(isPinned(op));
  struct ForwardHHObjptrArgs* args = (struct ForwardHHObjptrArgs*)rawArgs;
  /* The toDepth field is set by the loop that calls this function.
   * Note: Can't do HM_getObjptrDepth or similar here! We broke that by
   * moving chunks around during this phase! */
  uint32_t opDepth = args->toDepth;

  /* can't be NULL, because when we filtered the remembered set, we scavenged
   * all entries into the toSpace */
  assert(NULL != args->toSpace[opDepth]);

  HM_chunk chunk = HM_getChunkOf(objptrToPointer(op, NULL));

  if (!chunk->pinnedDuringCollection) {
    assert(isObjptrInToSpace(op, args));
    return;
  }

#if ASSERT
  /* make sure we're removing the chunk from the correct list!! */
  assert(hhContainsChunk(args->fromSpace[opDepth], chunk));
#endif

  /* Can't do HM_getLevelHead here! We broke that by moving chunks around! */
  HM_unlinkChunk(HM_HH_getChunkList(args->fromSpace[opDepth]), chunk);
  HM_appendChunk(HM_HH_getChunkList(args->toSpace[opDepth]), chunk);
  chunk->levelHead = args->toSpace[opDepth];
  chunk->pinnedDuringCollection = FALSE;

  /* All unpinned objects need to be replaced by gaps!!
   * These objects were scavenged out of these chunks, and so now the
   * leftover space is meaningless bits. If we interpret this leftover space
   * as actual objects in a subsequent collection, it will be incorrect.
   *
   * TODO: coalesce gaps (easy to do; just need to hop forward until we find
   * the next pinned object and then create a single large gap)
   *
   * TODO: this could be more efficient if we have special pinned chunks that
   * do not contain unpinned objects? Then we can leave all the gaps alone,
   * and we know that these chunks will never need to be scanned. But it is
   * tricky to guarantee that a chunk contains no unpinned objects... here's
   * a brainstorm.
   *   - (Bad idea) At write barrier, eagerly forward freshly pinned object
   *   into a pinned chunk. Difficult to implement because then the mutator
   *   needs a read barrier to follow forwarding pointers created at pins.
   *   And how do you unpin?
   *   - (Possible?) For each heap, maintain a separate space of pinned chunks.
   *   At write barrier, we could buffer newly pinned objects and pin their
   *   chunks later. At local GC, make sure to scavenge all unpinned objects
   *   out of these chunks...
   */
  pointer p = HM_getChunkStart(chunk);
  while (p < HM_getChunkFrontier(chunk)) {
    pointer objStart = advanceToObjectData(s, p);

    /* to calculate the object's size, we have to handle the case where it
     * was forwarded out of this chunk. */
    size_t objSize;
    if (hasFwdPtr(objStart)) {
      objSize = objectSize(s, objptrToPointer(getFwdPtr(objStart), NULL));
    } else {
      objSize = objectSize(s, objStart);
    }

    pointer objEnd = objStart + objSize;
    assert(objEnd <= HM_getChunkFrontier(chunk));

    /* leave pinned objects alone */
    if (!hasFwdPtr(objStart) && isPinned(pointerToObjptr(objStart, NULL))) {
      p = objEnd;
      continue;
    }

    /* fill garbage object with a gap */
    fillGap(s, p, objEnd);
    p = objEnd;
  }

  assert(p == HM_getChunkFrontier(chunk));
  assert(isObjptrInToSpace(op, args));
}
#endif

/* ========================================================================= */

void forwardObjptrsOfRemembered(GC_state s, objptr op, void* rawArgs) {
  assert(isPinned(op));

  foreachObjptrInObject(
    s,
    objptrToPointer(op, NULL),
    FALSE,
    &trueObjptrPredicate,
    NULL,
    &forwardHHObjptr,
    rawArgs
  );
}

/* ========================================================================= */

void forwardHHObjptr (GC_state s,
                      objptr* opp,
                      void* rawArgs) {
  struct ForwardHHObjptrArgs* args = ((struct ForwardHHObjptrArgs*)(rawArgs));
  objptr op = *opp;
  pointer p = objptrToPointer (op, NULL);

  assert(args->toDepth == HM_HH_INVALID_DEPTH);

  if (DEBUG_DETAILED) {
    fprintf (stderr,
             "forwardHHObjptr  opp = "FMTPTR"  op = "FMTOBJPTR"  p = "
             ""FMTPTR"\n",
             (uintptr_t)opp,
             op,
             (uintptr_t)p);
  }

  LOG(LM_HH_COLLECTION, LL_DEBUGMORE,
      "opp = "FMTPTR"  op = "FMTOBJPTR"  p = "FMTPTR,
      (uintptr_t)opp,
      op,
      (uintptr_t)p);

  if (!isObjptr(op) || isObjptrInRootHeap(s, op)) {
    /* does not point to an HH objptr, so not in scope for collection */
    LOG(LM_HH_COLLECTION, LL_DEBUGMORE,
        "skipping opp = "FMTPTR"  op = "FMTOBJPTR"  p = "FMTPTR": not in HH.",
        (uintptr_t)opp,
        op,
        (uintptr_t)p);
    return;
  }

  uint32_t opDepth = HM_getObjptrDepthPathCompress(op);

  if (opDepth > args->maxDepth) {
    DIE("entanglement detected during collection: %p is at depth %u, below %u",
        (void *)p,
        opDepth,
        args->maxDepth);
  }

  /* RAM_NOTE: This is more nuanced with non-local collection */
  if ((opDepth > args->maxDepth) ||
      /* cannot forward any object below 'args->minDepth' */
      (opDepth < args->minDepth)) {
      LOG(LM_HH_COLLECTION, LL_DEBUGMORE,
          "skipping opp = "FMTPTR"  op = "FMTOBJPTR"  p = "FMTPTR
          ": depth %d not in [minDepth %d, maxDepth %d].",
          (uintptr_t)opp,
          op,
          (uintptr_t)p,
          opDepth,
          args->minDepth,
          args->maxDepth);
      return;
  }

  assert(HM_getObjptrDepth(op) >= args->minDepth);

  if (isObjptrInToSpace(op, args)) {
    assert(!hasFwdPtr(objptrToPointer(op, NULL)));
    assert(!isPinned(op));
    return;
  }

  /* Assert is in from space. This holds for pinned objects, too, because
   * their levelHead is still set to the fromSpace HH. (Pinned objects are
   * stored in a different chunklist during collection through.) */
  assert( HM_getLevelHead(HM_getChunkOf(objptrToPointer(op, NULL)))
          ==
          args->fromSpace[HM_getObjptrDepth(op)] );

  if (hasFwdPtr(p)) {
    objptr fop = getFwdPtr(p);
    assert(!hasFwdPtr(objptrToPointer(fop, NULL)));
    assert(isObjptrInToSpace(fop, args));
    assert(HM_getObjptrDepth(fop) == opDepth);
    *opp = fop;
    return;
  }

  assert(!hasFwdPtr(p));

  if (isPinned(op)) {
    return;
  }

  /* ========================================================================
   * if we get here, we have to actually scavenge the object:
   * we know this object is in the from-space, is not pinned, and is
   * in-scope of collection.
   */
  {
    assert(!isPinned(op));
    assert(!isObjptrInToSpace(op, args));
    assert(HM_getObjptrDepth(op) == opDepth);
    assert(opDepth >= args->minDepth);
    /* forward the object */
    GC_objectTypeTag tag;
    size_t metaDataBytes;
    size_t objectBytes;
    size_t copyBytes;

    /* compute object size and bytes to be copied */
    tag = computeObjectCopyParameters(s,
                                      p,
                                      &objectBytes,
                                      &copyBytes,
                                      &metaDataBytes);

    switch (tag) {
    case STACK_TAG:
        args->stacksCopied++;
        break;
    case WEAK_TAG:
        die(__FILE__ ":%d: "
            "forwardHHObjptr() does not support WEAK_TAG objects!",
            __LINE__);
        break;
    default:
        break;
    }

    HM_HierarchicalHeap tgtHeap = args->toSpace[opDepth];

    if (tgtHeap == NULL) {
      /* Level does not exist, so create it */
      /* SAM_NOTE: new heaps are initialized with one free chunk. */
      tgtHeap = HM_HH_new(s, opDepth, HM_getChunkOf(p)->decheckState);
      args->toSpace[opDepth] = tgtHeap;
    }

    HM_chunkList tgtChunkList = HM_HH_getChunkList(tgtHeap);

    assert (!hasFwdPtr(p));

    LOG(LM_HH_COLLECTION, LL_DEBUGMORE,
        "during collection, copying pointer %p at depth %u to chunk list %p",
        (void *)p,
        opDepth,
        (void *)tgtChunkList);

    /* SAM_NOTE: TODO: get this spaghetti code out of here. */
    if (!HM_getChunkOf(p)->mightContainMultipleObjects) {
      // assert(FALSE);
      /* This chunk contains *only* this object, so no need to copy. Instead,
       * just move the chunk. Don't forget to update the levelHead, too! */
      assert(!hasFwdPtr(p));
      HM_chunk chunk = HM_getChunkOf(p);
      HM_unlinkChunk(HM_HH_getChunkList(HM_getLevelHead(chunk)), chunk);
      /* SAM_NOTE: TODO: this is inefficient, because we have to abandon the
       * previous last chunk, resulting in unnecessary fragmentation. This can
       * be avoided by not relying upon using the tgtChunkList...lastChunk to
       * allocate the next object, similiar to how currentChunk
       * doesn't need to be at the end of its chunk list. */
      /* SAM_NOTE: it is crucial that this is append and not prepend, because
       * traversing the to-space executes left-to-right. */
      HM_appendChunk(tgtChunkList, chunk);
      chunk->levelHead = tgtHeap;

      HM_chunk newChunk = HM_allocateChunk(tgtChunkList, GC_HEAP_LIMIT_SLOP);
      if (NULL == newChunk) {
        DIE("Ran out of space for Hierarchical Heap!");
      }
      /* Safe to use the same dechecker state as the moved chunk, because all
       * dechecker states within the same heap are equivalent from the
       * perspective of future disentanglement checks. */
      newChunk->decheckState = chunk->decheckState;
      newChunk->levelHead = tgtHeap;

      LOG(LM_HH_COLLECTION, LL_DEBUGMORE,
        "Moved single-object chunk %p of size %zu",
        (void*)chunk,
        HM_getChunkSize(chunk));
      args->bytesMoved += copyBytes;
      args->objectsMoved++;
      return;
    }

    pointer copyPointer = copyObject(p - metaDataBytes,
                                     objectBytes,
                                     copyBytes,
                                     tgtHeap);

    args->bytesCopied += copyBytes;
    args->objectsCopied++;
    LOG(LM_HH_COLLECTION, LL_DEBUGMORE,
        "%p --> %p", ((void*)(p - metaDataBytes)), ((void*)(copyPointer)));

    /* Store the forwarding pointer in the old object metadata. */
    *(getFwdPtrp(p)) = pointerToObjptr (copyPointer + metaDataBytes,
                                        NULL);
    assert (hasFwdPtr(p));

    /* use the forwarding pointer */
    *opp = getFwdPtr(p);

    assert(isObjptrInToSpace(getFwdPtr(p), args));
  }

  LOG(LM_HH_COLLECTION, LL_DEBUGMORE,
      "opp "FMTPTR" set to "FMTOBJPTR,
      ((uintptr_t)(opp)),
      *opp);
}
#endif /* MLTON_GC_INTERNAL_FUNCS */

GC_objectTypeTag computeObjectCopyParameters(GC_state s, pointer p,
                                             size_t *objectSize,
                                             size_t *copySize,
                                             size_t *metaDataSize) {
    GC_header header;
    GC_objectTypeTag tag;
    uint16_t bytesNonObjptrs;
    uint16_t numObjptrs;
    header = getHeader(p);
    splitHeader(s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);

    /* Compute the space taken by the metadata and object body. */
    if ((NORMAL_TAG == tag) or (WEAK_TAG == tag)) { /* Fixed size object. */
      if (WEAK_TAG == tag) {
        die(__FILE__ ":%d: "
            "computeObjectSizeAndCopySize() #define does not support"
            " WEAK_TAG objects!",
            __LINE__);
      }
      *metaDataSize = GC_NORMAL_METADATA_SIZE;
      *objectSize = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
      *copySize = *objectSize;
    } else if (SEQUENCE_TAG == tag) {
      *metaDataSize = GC_SEQUENCE_METADATA_SIZE;
      *objectSize = sizeofSequenceNoMetaData (s, getSequenceLength (p),
                                              bytesNonObjptrs, numObjptrs);
      *copySize = *objectSize;
    } else {
      /* Stack. */
      // bool current;
      // size_t reservedNew;
      GC_stack stack;

      assert (STACK_TAG == tag);
      *metaDataSize = GC_STACK_METADATA_SIZE;
      stack = (GC_stack)p;

      /* SAM_NOTE:
       * I am disabling shrinking here because it assumes that
       * the stack is going to be copied, which doesn't work with the
       * "stacks-in-their-own-chunks" strategy.
       */
#if 0
      /* RAM_NOTE: This changes with non-local collection */
      /* Check if the pointer is the current stack of my processor. */
      current = getStackCurrent(s) == stack;

      reservedNew = sizeofStackShrinkReserved (s, stack, current);
      if (reservedNew < stack->reserved) {
        LOG(LM_HH_COLLECTION, LL_DEBUG,
            "Shrinking stack of size %s bytes to size %s bytes"
            ", using %s bytes.",
            uintmaxToCommaString(stack->reserved),
            uintmaxToCommaString(reservedNew),
            uintmaxToCommaString(stack->used));
        stack->reserved = reservedNew;
      }
#endif
      *objectSize = sizeof (struct GC_stack) + stack->reserved;
      *copySize = sizeof (struct GC_stack) + stack->used;
    }

    *objectSize += *metaDataSize;
    *copySize += *metaDataSize;

    return tag;
}

pointer copyObject(pointer p,
                   size_t objectSize,
                   size_t copySize,
                   HM_HierarchicalHeap tgtHeap) {
  assert(HM_HH_isLevelHead(tgtHeap));
  assert(copySize <= objectSize);

  HM_chunkList tgtChunkList = HM_HH_getChunkList(tgtHeap);
  assert(NULL != tgtChunkList);

  /* get the chunk to allocate in */
  HM_chunk chunk = HM_getChunkListLastChunk(tgtChunkList);
  pointer frontier = HM_getChunkFrontier(chunk);
  pointer limit = HM_getChunkLimit(chunk);
  assert(frontier <= limit);

  bool mustExtend = ((size_t)(limit - frontier) < objectSize) ||
                    (frontier >= (pointer)chunk + HM_BLOCK_SIZE);

  if (mustExtend) {
    /* Need to allocate a new chunk. Safe to use the dechecker state of where
     * the object came from, as all objects in the same heap can be safely
     * reassigned to any dechecker state of that heap. */
    chunk = HM_allocateChunk(tgtChunkList, objectSize);
    if (NULL == chunk) {
      DIE("Ran out of space for Hierarchical Heap!");
    }
    chunk->decheckState = HM_getChunkOf(p)->decheckState;
    chunk->levelHead = tgtHeap;
    frontier = HM_getChunkFrontier(chunk);
  }

  GC_memcpy(p, frontier, copySize);
  pointer newFrontier = frontier + objectSize;
  HM_updateChunkValues(chunk, newFrontier);
  if (newFrontier >= (pointer)chunk + HM_BLOCK_SIZE) {
    /* size is arbitrary; just need a new chunk */
    chunk = HM_allocateChunk(tgtChunkList, GC_HEAP_LIMIT_SLOP);
    if (NULL == chunk) {
      DIE("Ran out of space for Hierarchical Heap!");
    }
    chunk->decheckState = HM_getChunkOf(p)->decheckState;
    chunk->levelHead = tgtHeap;
  }

  return frontier;
}

bool skipStackAndThreadObjptrPredicate(GC_state s,
                                       pointer p,
                                       void* rawArgs) {
  /* silence compliler */
  ((void)(s));

  /* extract expected stack */
  LOCAL_USED_FOR_ASSERT const struct SSATOPredicateArgs* args =
      ((struct SSATOPredicateArgs*)(rawArgs));

  /* run through FALSE cases */
  GC_header header;
  header = getHeader(p);
  if (header == GC_STACK_HEADER) {
    assert(args->expectedStackPointer == p);
    return FALSE;
  } else if (header == GC_THREAD_HEADER) {
    assert(args->expectedThreadPointer == p);
    return FALSE;
  }

  return TRUE;
}

#if ASSERT

void checkRememberedEntry(
  __attribute__((unused)) GC_state s,
  objptr object,
  void* args)
{
  HM_HierarchicalHeap hh = (HM_HierarchicalHeap)args;

  assert(isPinned(object));
  assert(unpinDepthOf(object) < HM_HH_getDepth(hh));

  HM_chunk theChunk = HM_getChunkOf(objptrToPointer(object, NULL));

  assert(hhContainsChunk(hh, theChunk));
  assert(HM_getLevelHead(theChunk) == hh);
}

bool hhContainsChunk(HM_HierarchicalHeap hh, HM_chunk theChunk)
{
  for (HM_chunk chunk = HM_HH_getChunkList(hh)->firstChunk;
       chunk != NULL;
       chunk = chunk->nextChunk)
  {
    if (chunk == theChunk) {
      return TRUE;
    }
  }

  return FALSE;
}

#endif

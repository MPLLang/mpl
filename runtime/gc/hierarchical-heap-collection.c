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

#if 0
void checkDisentangledDepthAndFreeze(
  GC_state s,
  HM_remembered remElem,
  void* rawArgs);

void unfreezeDisentangledDepthBefore(
  GC_state s,
  HM_remembered remElem,
  void* rawArgs);

void unfreezeDisentangledDepthAfter(
  GC_state s,
  HM_remembered remElem,
  void* rawArgs);
#endif

void tryUnpinOrKeepPinned(
  GC_state s,
  HM_remembered remElem,
  void* rawArgs);

void copySuspect(GC_state s, objptr *opp, objptr op, void *rawArghh);

void forwardObjptrsOfRemembered(
  GC_state s,
  HM_remembered remElem,
  void* rawArgs);

// void scavengeChunkOfPinnedObject(GC_state s, objptr op, void* rawArgs);

#if ASSERT
void checkRememberedEntry(GC_state s, HM_remembered remElem, void* args);
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

enum LGC_freedChunkType {
  LGC_FREED_REMSET_CHUNK,
  LGC_FREED_STACK_CHUNK,
  LGC_FREED_NORMAL_CHUNK,
  LGC_FREED_SUSPECT_CHUNK
};

static const char* LGC_freedChunkTypeToString[] = {
  "LGC_FREED_REMSET_CHUNK",
  "LGC_FREED_STACK_CHUNK",
  "LGC_FREED_NORMAL_CHUNK",
  "LGC_FREED_SUSPECT_CHUNK"
};

struct LGC_chunkInfo {
  uint32_t depth;
  int32_t procNum;
  uintmax_t collectionNumber;
  enum LGC_freedChunkType freedType;
};


void LGC_writeFreeChunkInfo(
  __attribute__((unused)) GC_state s,
  char* infoBuffer,
  size_t bufferLen,
  void* env)
{
  struct LGC_chunkInfo *info = env;

  snprintf(infoBuffer, bufferLen,
    "freed %s at depth %u by LGC %d:%zu",
    LGC_freedChunkTypeToString[info->freedType],
    info->depth,
    info->procNum,
    info->collectionNumber);
}


uint32_t minDepthWithoutCC(GC_thread thread) {
  assert(thread != NULL);
  assert(thread->hierarchicalHeap != NULL);
  HM_HierarchicalHeap cursor = thread->hierarchicalHeap;

  if (cursor->subHeapForCC != NULL)
    return thread->currentDepth+1;

  while (cursor->nextAncestor != NULL &&
         cursor->nextAncestor->subHeapForCC == NULL)
  {
    assert(HM_HH_getConcurrentPack(cursor)->ccstate == CC_UNREG);
    cursor = cursor->nextAncestor;
  }

  assert(cursor->subHeapForCC == NULL);
  assert(cursor->subHeapCompletedCC == NULL);
  assert(cursor->nextAncestor == NULL ||
         cursor->nextAncestor->subHeapForCC != NULL);
  assert(HM_HH_getConcurrentPack(cursor)->ccstate == CC_UNREG);

  return HM_HH_getDepth(cursor);
}

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

  // if (NULL != hh->subHeapForCC) {
  //   LOG(LM_HH_COLLECTION, LL_INFO,
  //     "Skipping local collection at depth %u due to outstanding CC",
  //     HM_HH_getDepth(hh));
  //   return;
  // }

  if (s->wsQueueTop == BOGUS_OBJPTR || s->wsQueueBot == BOGUS_OBJPTR) {
    LOG(LM_HH_COLLECTION, LL_DEBUG, "Skipping collection, deque not registered yet");
    return;
  }

  uint64_t topval = *(uint64_t*)objptrToPointer(s->wsQueueTop, NULL);
  uint32_t potentialLocalScope = UNPACK_IDX(topval);
  uint32_t originalLocalScope = pollCurrentLocalScope(s);

  if (thread->currentDepth != originalLocalScope) {
    LOG(LM_HH_COLLECTION, LL_DEBUG,
      "Skipping collection:\n"
      "  currentDepth %u\n"
      "  originalLocalScope %u\n"
      "  potentialLocalScope %u\n",
      thread->currentDepth,
      originalLocalScope,
      potentialLocalScope);
    return;
  }

  /** Compute the min depth for local collection. We claim as many levels
    * as we can without interfering with CC, but only so far as desired.
    *
    * Note that we could permit local collection at the same level as a
    * registered (but not yet stolen) CC, as long as we update the rootsets
    * stored for the CC. But this is tricky. Much simpler to just avoid CC'ed
    * levels entirely.
    */
  uint32_t minNoCC = minDepthWithoutCC(thread);
  uint32_t minOkay = desiredScope;
  minOkay = max(minOkay, thread->minLocalCollectionDepth);
  minOkay = max(minOkay, minNoCC);
  uint32_t minDepth = originalLocalScope;
  while (minDepth > minOkay && tryClaimLocalScope(s)) {
    minDepth--;
    assert(minDepth == pollCurrentLocalScope(s));
  }
  assert(minDepth == pollCurrentLocalScope(s));

  if ( minDepth == 0 ||
       minOkay > minDepth ||
       minDepth > thread->currentDepth )
  {
    LOG(LM_HH_COLLECTION, LL_DEBUG,
      "Skipping collection:\n"
      "  minDepth %u\n"
      "  currentDepth %u\n"
      "  minNoCC %u\n"
      "  desiredScope %u\n"
      "  potentialLocalScope %u\n",
      minDepth,
      thread->currentDepth,
      minNoCC,
      desiredScope,
      potentialLocalScope);

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
  struct GC_foreachObjptrClosure forwardHHObjptrClosure =
    {.fun = forwardHHObjptr, .env = &forwardHHObjptrArgs};

  LOG(LM_HH_COLLECTION, LL_INFO,
      "collecting hh %p (L: %u):\n"
      "  LGC id %d:%zu\n"
      "  thread min-local depth is %u\n"
      "  min without CC is         %u\n"
      "  min okay is               %u\n"
      "  desired min is            %u\n"
      "  potential local scope is  %u -> %u\n"
      "  collection scope is       %u -> %u\n",
      // "  lchs %"PRIu64" lcs %"PRIu64,
      ((void*)(hh)),
      thread->currentDepth,
      s->procNumber,
      s->cumulativeStatistics->numHHLocalGCs,
      thread->minLocalCollectionDepth,
      minNoCC,
      minOkay,
      desiredScope,
      potentialLocalScope,
      thread->currentDepth,
      forwardHHObjptrArgs.minDepth,
      forwardHHObjptrArgs.maxDepth);

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
#if ASSERT
    /** SAM_NOTE: can't do this here, because pinned chunks contain garbage. */
    // traverseEachObjInChunkList(s, HM_HH_getChunkList(cursor));
#endif
    uint32_t d = HM_HH_getDepth(cursor);
    size_t sz = HM_getChunkListSize(HM_HH_getChunkList(cursor));
    sizesBefore[d] = sz;
    totalSizeBefore += sz;
  }

  /* ===================================================================== */

#if 0
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
      struct HM_foreachDownptrClosure closure =
        {.fun = checkDisentangledDepthAndFreeze, .env = (void*)&ddArgs};
      HM_foreachRemembered(s, HM_HH_getRemSet(cursor), &closure);

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
        struct HM_foreachDownptrClosure closure =
          {.fun = unfreezeDisentangledDepthBefore, .env = (void*)&ddArgs};
        HM_foreachRemembered(s, HM_HH_getRemSet(cursor), &closure);
      }

      releaseLocalScope(s, originalLocalScope);
      return;
    }
  }
#endif

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

    struct HM_foreachDownptrClosure closure =
      {.fun = tryUnpinOrKeepPinned, .env = (void*)&forwardHHObjptrArgs};
    HM_foreachRemembered(s, HM_HH_getRemSet(cursor), &closure);
  }
  forwardHHObjptrArgs.toDepth = HM_HH_INVALID_DEPTH;

  // assertInvariants(thread);

#if ASSERT
  for (HM_HierarchicalHeap cursor = hh;
       NULL != cursor;
       cursor = cursor->nextAncestor)
  {
    assert(forwardHHObjptrArgs.fromSpace[HM_HH_getDepth(cursor)] == cursor);
  }
#endif

  timespec_now(&stopTime);
  timespec_sub(&stopTime, &startTime);
  timespec_add(&(s->cumulativeStatistics->timeLocalPromo), &stopTime);
  Trace0(EVENT_PROMOTION_LEAVE);

  /* ===================================================================== */

  if (needGCTime(s)) {
    startTiming (RUSAGE_THREAD, &ru_start);
  }

  timespec_now(&startTime);

  LOG(LM_HH_COLLECTION, LL_DEBUG, "START root copy");

  // HM_HierarchicalHeap toSpace[maxDepth+1];
  // for (uint32_t i = 0; i <= maxDepth; i++) toSpace[i] = NULL;
  // forwardHHObjptrArgs.toSpace = &(toSpace[0]);
  forwardHHObjptrArgs.toDepth = HM_HH_INVALID_DEPTH;

  /* forward contents of stack */
  oldObjectCopied = forwardHHObjptrArgs.objectsCopied;
  foreachObjptrInObject(s,
                        objptrToPointer(getStackCurrentObjptr(s),
                                        NULL),
                        &trueObjptrPredicateClosure,
                        &forwardHHObjptrClosure,
                        FALSE);
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
                        &trueObjptrPredicateClosure,
                        &forwardHHObjptrClosure,
                        FALSE);
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
  forwardHHObjptr(s, &(s->currentThread), s->currentThread, &forwardHHObjptrArgs);
  LOG(LM_HH_COLLECTION, LL_DEBUG,
      (1 == (forwardHHObjptrArgs.objectsCopied - oldObjectCopied)) ?
      "Copied thread from GC_state" : "Did not copy thread from GC_state");
  Trace3(EVENT_COPY,
   forwardHHObjptrArgs.bytesCopied,
   forwardHHObjptrArgs.objectsCopied,
   forwardHHObjptrArgs.stacksCopied);

  /* =================================================================
   * forward contents of additional root during signal handler, if any
   */
  if (s->savedThreadDuringSignalHandler != BOGUS_OBJPTR) {
    /* forward contents of stack */
    foreachObjptrInObject(
      s,
      objptrToPointer(
        threadObjptrToStruct(s, s->savedThreadDuringSignalHandler)->stack,
        NULL
      ),
      &trueObjptrPredicateClosure,
      &forwardHHObjptrClosure,
      FALSE
    );

    /* forward contents of thread (hence including stack) */
    foreachObjptrInObject(
      s,
      objptrToPointer(
        s->savedThreadDuringSignalHandler,
        NULL
      ),
      &trueObjptrPredicateClosure,
      &forwardHHObjptrClosure,
      FALSE
    );

    forwardHHObjptr(
      s,
      &(s->savedThreadDuringSignalHandler),
      s->savedThreadDuringSignalHandler,
      &forwardHHObjptrArgs
    );
  }

  /* =======================================================================
   * forward s->savedThread, if necessary
   */

  if (s->savedThread != BOGUS_OBJPTR) {
    /* forward contents of stack */
    foreachObjptrInObject(
      s,
      objptrToPointer(
        threadObjptrToStruct(s, s->savedThread)->stack,
        NULL
      ),
      &trueObjptrPredicateClosure,
      &forwardHHObjptrClosure,
      FALSE
    );

    /* forward contents of thread (hence including stack) */
    foreachObjptrInObject(
      s,
      objptrToPointer(
        s->savedThread,
        NULL
      ),
      &trueObjptrPredicateClosure,
      &forwardHHObjptrClosure,
      FALSE
    );

    forwardHHObjptr(
      s,
      &(s->savedThread),
      s->savedThread,
      &forwardHHObjptrArgs
    );
  }

  /* ======================================================================= */

  /* forward contents of deque */
  oldObjectCopied = forwardHHObjptrArgs.objectsCopied;
  foreachObjptrInObject(s,
                        objptrToPointer(s->wsQueue,
                                        NULL),
                        &trueObjptrPredicateClosure,
                        &forwardHHObjptrClosure,
                        FALSE);
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
    struct HM_foreachDownptrClosure closure =
      {.fun = forwardObjptrsOfRemembered, .env = (void*)&forwardHHObjptrArgs};
    HM_foreachRemembered(s, HM_HH_getRemSet(toSpaceLevel), &closure);

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

#if ASSERT
    // SAM_NOTE: safe to check here, because pinned chunks are separate.
    traverseEachObjInChunkList(s, HM_HH_getChunkList(toSpaceLevel));
#endif

    /* unset the flags on pinned chunks and update their HH pointer */
    for (HM_chunk chunkCursor = pinned[depth].firstChunk;
         chunkCursor != NULL;
         chunkCursor = chunkCursor->nextChunk)
    {
      assert(chunkCursor->pinnedDuringCollection);
      chunkCursor->pinnedDuringCollection = FALSE;
      chunkCursor->levelHead = HM_HH_getUFNode(toSpaceLevel);
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

  struct LGC_chunkInfo info =
    {.depth = 0,
     .procNum = s->procNumber,
     .collectionNumber = s->cumulativeStatistics->numHHLocalGCs,
     .freedType = LGC_FREED_NORMAL_CHUNK};
  struct writeFreedBlockInfoFnClosure infoc =
    {.fun = LGC_writeFreeChunkInfo, .env = &info};

  for (HM_HierarchicalHeap cursor = hh;
       NULL != cursor && HM_HH_getDepth(cursor) >= minDepth;
       cursor = cursor->nextAncestor) {
    HM_chunkList suspects = HM_HH_getSuspects(cursor);
    if (suspects->size != 0) {
      uint32_t depth = HM_HH_getDepth(cursor);
      forwardHHObjptrArgs.toDepth = depth;
      struct GC_foreachObjptrClosure fObjptrClosure =
        {.fun = copySuspect, .env = &forwardHHObjptrArgs};
      ES_foreachSuspect(s, suspects, &fObjptrClosure);
      info.depth = depth;
      info.freedType = LGC_FREED_SUSPECT_CHUNK;
      HM_freeChunksInListWithInfo(s, suspects, &infoc);
    }
  }

  /* Free old chunks and find the tail (upper segment) of the original hh
   * that will be merged with the toSpace */
  HM_HierarchicalHeap hhTail = hh;
  while (NULL != hhTail && HM_HH_getDepth(hhTail) >= minDepth)
  {
    assert(hhTail->subHeapForCC == NULL);
    assert(hhTail->subHeapCompletedCC == NULL);
    HM_HierarchicalHeap nextAncestor = hhTail->nextAncestor;

    HM_chunkList level = HM_HH_getChunkList(hhTail);
    HM_chunkList remset = HM_HH_getRemSet(hhTail);
    if (NULL != remset) {
#if ASSERT
      /* clear out memory to quickly catch some memory safety errors */
      // HM_chunk chunkCursor = remset->firstChunk;
      // while (chunkCursor != NULL) {
      //   pointer start = HM_getChunkStart(chunkCursor);
      //   size_t length = (size_t)(chunkCursor->limit - start);
      //   memset(start, 0xBF, length);
      //   chunkCursor = chunkCursor->nextChunk;
      // }
#endif
      info.depth = HM_HH_getDepth(hhTail);
      info.freedType = LGC_FREED_REMSET_CHUNK;
      HM_freeChunksInListWithInfo(s, remset, &infoc);
    }

#if ASSERT
    HM_chunk chunkCursor = level->firstChunk;
    while (chunkCursor != NULL) {
      assert(!chunkCursor->pinnedDuringCollection);
      chunkCursor = chunkCursor->nextChunk;
    }
#endif

    info.depth = HM_HH_getDepth(hhTail);
    info.freedType = LGC_FREED_NORMAL_CHUNK;
    HM_freeChunksInListWithInfo(s, level, &infoc);
    HM_HH_freeAllDependants(s, hhTail, FALSE);
    freeFixedSize(getUFAllocator(s), HM_HH_getUFNode(hhTail));
    freeFixedSize(getHHAllocator(s), hhTail);

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
  if (NULL == hhTail && NULL == hhToSpace) {
    /** SAM_NOTE: If we collected everything, I suppose this is possible.
      * But shouldn't the stack and thread at least be in the root-to-leaf
      * path? Should look into this...
      */
    hh = HM_HH_new(s, thread->currentDepth);
  }
  else {
    hh = HM_HH_zip(s, hhTail, hhToSpace);
  }

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


#if 0
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
      struct HM_foreachDownptrClosure closure =
        {.fun = unfreezeDisentangledDepthAfter, .env = (void*)&ddArgs};
      HM_foreachRemembered(s, HM_HH_getRemSet(cursor), &closure);
    }
  }
#endif

#if ASSERT
  assertInvariants(thread);

  /* some additional assertions for pinned objects */
  for (HM_HierarchicalHeap cursor = hh;
       cursor != NULL && HM_HH_getDepth(cursor) >= minDepth;
       cursor = cursor->nextAncestor)
  {
    struct HM_foreachDownptrClosure closure =
      {.fun = checkRememberedEntry, .env = (void*)cursor};
    HM_foreachRemembered(s, HM_HH_getRemSet(cursor), &closure);
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

#if ASSERT
    HM_assertChunkListInvariants(lev);

    // SAM_NOTE: can't do this here, because pinned chunks contain garbage.
    // traverseEachObjInChunkList(s, lev);
#endif

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
    chunk->levelHead = HM_HH_getUFNode(tgtHeap);

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

/* ========================================================================= */

#if 0
void checkDisentangledDepthAndFreeze(
  __attribute__((unused)) GC_state s,
  HM_remembered remElem,
  void* rawArgs)
{
  objptr op = remElem->object;

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
  HM_remembered remElem,
  void* rawArgs)
{
  objptr op = remElem->object;

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
  HM_remembered remElem,
  void* rawArgs)
{
  objptr op = remElem->object;

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
#endif

/* ========================================================================= */
void copySuspect(
  GC_state s,
  __attribute__((unused)) objptr *opp,
  objptr op,
  void *rawArghh)
{
  struct ForwardHHObjptrArgs *args = (struct ForwardHHObjptrArgs *)rawArghh;
  assert(isObjptr(op));
  pointer p = objptrToPointer(op, NULL);
  objptr new_ptr = op;
  if (hasFwdPtr(p)) {
    new_ptr = getFwdPtr(p);
  }
  else if (!isPinned(op)) {
    /* the suspect does not have a fwd-ptr and is not pinned
     * ==> its garbage, so skip it
     */
    return;
  }
  uint32_t opDepth = args->toDepth;
  if (NULL == args->toSpace[opDepth])
  {
    args->toSpace[opDepth] = HM_HH_new(s, opDepth);
  }
  HM_storeInchunkList(HM_HH_getSuspects(args->toSpace[opDepth]), &new_ptr, sizeof(objptr));
}

void tryUnpinOrKeepPinned(GC_state s, HM_remembered remElem, void* rawArgs) {
  struct ForwardHHObjptrArgs* args = (struct ForwardHHObjptrArgs*)rawArgs;
  objptr op = remElem->object;

#if ASSERT
  HM_chunk fromChunk = HM_getChunkOf(objptrToPointer(remElem->from, NULL));
  HM_HierarchicalHeap fromHH = HM_getLevelHead(fromChunk);
  assert(HM_HH_getDepth(fromHH) <= args->toDepth);
#endif

  if (!isPinned(op)) {
    // If previously unpinned, then no need to remember this object.
    assert(HM_getLevelHead(fromChunk) == args->fromSpace[args->toDepth]);

    LOG(LM_HH_PROMOTION, LL_INFO,
      "forgetting remset entry from "FMTOBJPTR" to "FMTOBJPTR,
      remElem->from, op);

    return;
  }

  assert(isPinned(op));

  /* We could just look up the depth of `op`, with the normal technique
   * (getLevelHead, etc.), but this should be faster. The toDepth field
   * is set by the loop that calls this function */
  uint32_t opDepth = args->toDepth;
  HM_chunk chunk = HM_getChunkOf(objptrToPointer(op, NULL));

  if (NULL == args->toSpace[opDepth]) {
    args->toSpace[opDepth] = HM_HH_new(s, opDepth);
  }

#if ASSERT
  assert(opDepth <= args->maxDepth);
  HM_HierarchicalHeap hh = HM_getLevelHead(chunk);
  assert(args->fromSpace[opDepth] == hh);
  if (chunk->pinnedDuringCollection)
    assert(listContainsChunk(&(args->pinned[opDepth]), chunk));
  else
    assert(hhContainsChunk(args->fromSpace[opDepth], chunk));
#endif

#if 0
  /** If it's not in our from-space, then it's entangled.
    * KEEP THE ENTRY but don't do any of the other nasty stuff.
    *
    * SAM_NOTE: pinned chunks still have their HH set to the from-space,
    * despite living in separate chunklists.
    */
  if (opDepth > args->maxDepth || args->fromSpace[opDepth] != hh) {
    assert(s->controls->manageEntanglement);
    /** TODO: assert entangled here */

    HM_remember(HM_HH_getRemSet(args->toSpace[opDepth]), remElem);
    return;
  }
#endif

  assert(HM_getObjptrDepth(op) == opDepth);
  assert(HM_getLevelHead(chunk) == args->fromSpace[opDepth]);

  uint32_t unpinDepth = unpinDepthOf(op);
  uint32_t fromDepth = HM_getObjptrDepth(remElem->from);

  assert(fromDepth <= opDepth);

  if (opDepth <= unpinDepth) {
    unpinObject(op);
    assert(fromDepth == opDepth);

    LOG(LM_HH_PROMOTION, LL_INFO,
      "forgetting remset entry from "FMTOBJPTR" to "FMTOBJPTR,
      remElem->from, op);

    return;
  }

  if (fromDepth > unpinDepth) {
    /** If this particular remembered entry came from deeper than some other
      * down-pointer, then we don't need to keep it around. There will be some
      * other remembered entry coming from the unpinDepth level.
      *
      * But note that it is very important that the condition is a strict
      * inequality: we need to keep all remembered entries that came from the
      * same shallowest level. (CC-chaining depends on this.)
      */

    LOG(LM_HH_PROMOTION, LL_INFO,
      "forgetting remset entry from "FMTOBJPTR" to "FMTOBJPTR,
      remElem->from, op);

    return;
  }

  assert(fromDepth == unpinDepth);

  /* otherwise, object stays pinned, and we have to scavenge this remembered
   * entry into the toSpace. */

  HM_remember(HM_HH_getRemSet(args->toSpace[opDepth]), remElem);

  if (chunk->pinnedDuringCollection) {
    return;
  }

  chunk->pinnedDuringCollection = TRUE;
  assert(hhContainsChunk(args->fromSpace[opDepth], chunk));
  assert(HM_getLevelHead(chunk) == args->fromSpace[opDepth]);

  HM_unlinkChunkPreserveLevelHead(
    HM_HH_getChunkList(args->fromSpace[opDepth]),
    chunk);
  HM_appendChunk(&(args->pinned[opDepth]), chunk);

  assert(HM_getLevelHead(chunk) == args->fromSpace[opDepth]);
}

/* ========================================================================= */

void forwardObjptrsOfRemembered(GC_state s, HM_remembered remElem, void* rawArgs) {
  objptr op = remElem->object;

  assert(isPinned(op));

  struct GC_foreachObjptrClosure closure =
    {.fun = forwardHHObjptr, .env = rawArgs};

  foreachObjptrInObject(
    s,
    objptrToPointer(op, NULL),
    &trueObjptrPredicateClosure,
    &closure,
    FALSE
  );

  forwardHHObjptr(s, &(remElem->from), remElem->from, rawArgs);
}

/* ========================================================================= */

void forwardHHObjptr(
  GC_state s,
  objptr* opp,
  objptr op,
  void* rawArgs)
{
  struct ForwardHHObjptrArgs* args = ((struct ForwardHHObjptrArgs*)(rawArgs));
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
    assert(!isPinned(fop));
    *opp = fop;
    return;
  }

  assert(!hasFwdPtr(p));

  /** REALLY SUBTLE. CC clears out remset entries, but can't safely perform
    * unpinning. So, there could be objects that (for the purposes of LC) are
    * semantically unpinned, but just haven't been marked as such yet. Here,
    * we are lazily checking to see if this object should have been unpinned.
    */
  if (isPinned(op) && unpinDepthOf(op) < opDepth) {
    // This is a truly pinned object
    assert(listContainsChunk( &(args->pinned[opDepth]),
                              HM_getChunkOf(objptrToPointer(op, NULL))
    ));
    return;
  }
  else {
    // This object should have been previously unpinned
    unpinObject(op);
  }

  /* ========================================================================
   * if we get here, we have to actually scavenge the object:
   * we know this object is in the from-space, is not pinned, and is
   * in-scope of collection.
   */
  {
    assert(!isPinned(op));
    assert(!isObjptrInToSpace(op, args));
    assert(HM_getLevelHead(HM_getChunkOf(p)) == args->fromSpace[HM_getObjptrDepth(op)]);
    assert(HM_getObjptrDepth(op) >= args->minDepth);
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
      tgtHeap = HM_HH_new(s, opDepth);
      args->toSpace[opDepth] = tgtHeap;
    }
    assert(p == objptrToPointer(op, NULL));

    /* use the forwarding pointer */
    *opp = relocateObject(s, op, tgtHeap, args);
  }

  LOG(LM_HH_COLLECTION, LL_DEBUGMORE,
      "opp "FMTPTR" set to "FMTOBJPTR,
      ((uintptr_t)(opp)),
      *opp);
}

pointer copyObject(pointer p,
                   size_t objectSize,
                   size_t copySize,
                   HM_HierarchicalHeap tgtHeap) {

// check if you can add to existing chunk --> mightContain + size
// If not, allocate new chunk and copy.

  assert(HM_HH_isLevelHead(tgtHeap));
  assert(copySize <= objectSize);

  HM_chunkList tgtChunkList = HM_HH_getChunkList(tgtHeap);
  assert(NULL != tgtChunkList);

  /* get the chunk to allocate in */
  bool mustExtend = false;

  HM_chunk chunk = HM_getChunkListLastChunk(tgtChunkList);
  if(chunk == NULL || !chunk->mightContainMultipleObjects){
    mustExtend = true;
  }
  else {
    pointer frontier = HM_getChunkFrontier(chunk);
    pointer limit = HM_getChunkLimit(chunk);
    assert(frontier <= limit);
    mustExtend = ((size_t)(limit - frontier) < objectSize) ||
                      (frontier  + GC_SEQUENCE_METADATA_SIZE
                        >= (pointer)chunk + HM_BLOCK_SIZE);
  }

  if (mustExtend) {
    /* Need to allocate a new chunk. Safe to use the dechecker state of where
     * the object came from, as all objects in the same heap can be safely
     * reassigned to any dechecker state of that heap. */
    chunk = HM_allocateChunk(tgtChunkList, objectSize);
    if (NULL == chunk) {
      DIE("Ran out of space for Hierarchical Heap!");
    }
    chunk->decheckState = HM_getChunkOf(p)->decheckState;
    chunk->levelHead = HM_HH_getUFNode(tgtHeap);
  }

  pointer frontier = HM_getChunkFrontier(chunk);

  GC_memcpy(p, frontier, copySize);
  pointer newFrontier = frontier + objectSize;
  HM_updateChunkFrontierInList(tgtChunkList, chunk, newFrontier);
  // if (newFrontier >= (pointer)chunk + HM_BLOCK_SIZE) {
  //   /* size is arbitrary; just need a new chunk */
  //   chunk = HM_allocateChunk(tgtChunkList, GC_HEAP_LIMIT_SLOP);
  //   if (NULL == chunk) {
  //     DIE("Ran out of space for Hierarchical Heap!");
  //   }
  //   chunk->levelHead = tgtHeap;
  // }

  return frontier;
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
  HM_remembered remElem,
  void* args)
{
  objptr object = remElem->object;

  HM_HierarchicalHeap hh = (HM_HierarchicalHeap)args;

  assert(isPinned(object));
  assert(unpinDepthOf(object) < HM_HH_getDepth(hh));

  HM_chunk theChunk = HM_getChunkOf(objptrToPointer(object, NULL));

  assert(hhContainsChunk(hh, theChunk));
  assert(HM_getLevelHead(theChunk) == hh);

  assert(!hasFwdPtr(objptrToPointer(object, NULL)));
  assert(!hasFwdPtr(objptrToPointer(remElem->from, NULL)));

  HM_chunk fromChunk = HM_getChunkOf(objptrToPointer(remElem->from, NULL));
  HM_HierarchicalHeap fromHH = HM_getLevelHead(fromChunk);
  assert(HM_HH_getDepth(fromHH) <= HM_HH_getDepth(hh));
}

bool hhContainsChunk(HM_HierarchicalHeap hh, HM_chunk theChunk)
{
  return listContainsChunk(HM_HH_getChunkList(hh), theChunk);
}

#endif

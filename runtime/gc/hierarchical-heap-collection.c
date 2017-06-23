/* Copyright (C) 2015 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
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

/**********/
/* Macros */
/**********/
#if ASSERT
#define COPY_OBJECT_HH_VALUE ((struct HM_HierarchicalHeap*)(0xb000deadfee1dead))
#else
#define COPY_OBJECT_HH_VALUE (NULL)
#endif

/******************************/
/* Static Function Prototypes */
/******************************/
/**
 * Copies the object into the new level list of the hierarchical heap provided.
 *
 * @param hh The hierarchical heap to operate on.
 * @param p The pointer to copy
 * @param objectSize The size of the object
 * @param copySize The number of bytes to copy
 * @param level The level to copy into
 * @param fromChunkList The ChunkList that 'p' resides in.
 *
 * @return pointer to the copied object
 */
pointer copyObject(struct HM_HierarchicalHeap* hh,
                   pointer p,
                   size_t objectSize,
                   size_t copySize,
                   Word32 level,
                   void* fromChunkList);

/**
 * Populates 'holes' with the current global heap holes from all processors.
 *
 * @attention
 * Must be run within an enter/leave in order to ensure correctness.
 *
 * @param s The GC_state to use
 * @param holes The holes to populate.
 */
void populateGlobalHeapHoles(GC_state s, struct GlobalHeapHole* holes);

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
void HM_HHC_registerQueue(uint32_t processor, pointer queuePointer) {
  GC_state s = pthread_getspecific (gcstate_key);

  assert(processor < s->numberOfProcs);
  assert(!HM_HH_objptrInHierarchicalHeap(s, pointerToObjptr (queuePointer,
                                                             s->heap->start)));

  s->procStates[processor].wsQueue = pointerToObjptr (queuePointer,
                                                      s->heap->start);
}

void HM_HHC_registerQueueLock(uint32_t processor, pointer queueLockPointer) {
  GC_state s = pthread_getspecific (gcstate_key);

  assert(processor < s->numberOfProcs);
  assert(!HM_HH_objptrInHierarchicalHeap(s, pointerToObjptr (queueLockPointer,
                                                             s->heap->start)));

  s->procStates[processor].wsQueueLock = pointerToObjptr (queueLockPointer,
                                                          s->heap->start);
}
#endif /* MLTON_GC_INTERNAL_BASIS */

#if (defined (MLTON_GC_INTERNAL_BASIS))
void HM_HHC_collectLocal(void) {
  GC_state s = pthread_getspecific (gcstate_key);
  struct HM_HierarchicalHeap* hh = HM_HH_getCurrent(s);
  struct rusage ru_start;
  Pointer wsQueueLock = objptrToPointer(s->wsQueueLock, s->heap->start);
  bool queueLockHeld = FALSE;

  if (NONE == s->controls->hhCollectionLevel) {
    /* collection disabled */
    return;
  }

  if (Parallel_alreadyLockedByMe(wsQueueLock)) {
    /* in a scheduler critical section, so cannot collect */
    LOG(LM_HH_COLLECTION, LL_DEBUG,
        "Queue locked by mutator/scheduler");
    queueLockHeld = TRUE;
  }

  LOG(LM_HH_COLLECTION, LL_DEBUG,
      "START");

  Trace0(EVENT_GC_ENTER);

  if (needGCTime(s)) {
    startTiming (RUSAGE_THREAD, &ru_start);
  }
  s->cumulativeStatistics->numHHLocalGCs++;

  /* used needs to be set because the mutator has changed s->stackTop. */
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
  getThreadCurrent(s)->exnStack = s->exnStack;

  int processor = Proc_processorNumber (s);

  HM_debugMessage(s,
                  "[%d] HM_HH_collectLocal(): Starting Local collection on "
                  "HierarchicalHeap = %p\n",
                  processor,
                  ((void*)(hh)));
  HM_debugDisplayHierarchicalHeap(s, hh);

  /* initialize hh->newLevelList for the collection */
  hh->newLevelList = NULL;

  /* lock queue to prevent steals */
  if (!queueLockHeld) {
    Parallel_lockTake(wsQueueLock);
  }
  lockHH(hh);

  assertInvariants(s, hh, LIVE);
  assert(hh->thread == s->currentThread);

  /* copy roots */
  struct ForwardHHObjptrArgs forwardHHObjptrArgs = {
    .hh = hh,
    .minLevel = HM_HH_getHighestStolenLevel(s, hh) + 1,
    .maxLevel = hh->level,
    .bytesCopied = 0,
    .objectsCopied = 0,
    .stacksCopied = 0
  };

  if (SUPERLOCAL == s->controls->hhCollectionLevel) {
    forwardHHObjptrArgs.minLevel = hh->level;
  }

  LOG(LM_HH_COLLECTION, LL_DEBUG,
      "collecting hh %p (SL: %u L: %u):\n"
      "  local scope is %u -> %u\n"
      "  lchs %"PRIu64" lcs %"PRIu64,
      ((void*)(hh)),
      hh->stealLevel,
      hh->level,
      forwardHHObjptrArgs.minLevel,
      forwardHHObjptrArgs.maxLevel,
      hh->locallyCollectibleHeapSize,
      hh->locallyCollectibleSize);

  LOG(LM_HH_COLLECTION, LL_DEBUG, "START root copy");

  /* forward contents of stack */
  forwardHHObjptrArgs.objectsCopied = 0;
  foreachObjptrInObject(s,
                        objptrToPointer(getStackCurrentObjptr(s),
                                        s->heap->start),
                        FALSE,
                        trueObjptrPredicate,
                        NULL,
                        forwardHHObjptr,
                        &forwardHHObjptrArgs);
  LOG(LM_HH_COLLECTION, LL_DEBUG,
      "Copied %"PRIu64" objects from stack",
      forwardHHObjptrArgs.objectsCopied);

  /* forward contents of thread (hence including stack) */
  forwardHHObjptrArgs.objectsCopied = 0;
  foreachObjptrInObject(s,
                        objptrToPointer(getThreadCurrentObjptr(s),
                                        s->heap->start),
                        FALSE,
                        trueObjptrPredicate,
                        NULL,
                        forwardHHObjptr,
                        &forwardHHObjptrArgs);
  LOG(LM_HH_COLLECTION, LL_DEBUG,
      "Copied %"PRIu64" objects from thread",
      forwardHHObjptrArgs.objectsCopied);

  /* forward thread itself */
  forwardHHObjptrArgs.objectsCopied = 0;
  forwardHHObjptr(s, &(s->currentThread), &forwardHHObjptrArgs);
  LOG(LM_HH_COLLECTION, LL_DEBUG,
      (1 == forwardHHObjptrArgs.objectsCopied) ?
      "Copied thread from GC_state" : "Did not copy thread from GC_state");


#if ASSERT
  /* forward thread from hh */
  forwardHHObjptrArgs.objectsCopied = 0;
  forwardHHObjptr(s, &(hh->thread), &forwardHHObjptrArgs);
  LOG(LM_HH_COLLECTION, LL_DEBUG,
      (1 == forwardHHObjptrArgs.objectsCopied) ?
      "Copied thread from HH" : "Did not copy thread from HH");
  assert(hh->thread == s->currentThread);
#else
  /* update thread in hh */
  hh->thread = s->currentThread;
#endif

  /* forward contents of deque */
  forwardHHObjptrArgs.objectsCopied = 0;
  foreachObjptrInObject(s,
                        objptrToPointer(s->wsQueue,
                                        s->heap->start),
                        FALSE,
                        trueObjptrPredicate,
                        NULL,
                        forwardHHObjptr,
                        &forwardHHObjptrArgs);
  LOG(LM_HH_COLLECTION, LL_DEBUG,
      "Copied %"PRIu64" objects from deque",
      forwardHHObjptrArgs.objectsCopied);

  /* forward retVal pointer if necessary */
  if (NULL != hh->retVal) {
    objptr root = pointerToObjptr(hh->retVal, s->heap->start);

    forwardHHObjptrArgs.objectsCopied = 0;
    forwardHHObjptr(s, &root, &forwardHHObjptrArgs);
    LOG(LM_HH_COLLECTION, LL_DEBUG,
      "Copied %"PRIu64" objects from hh->retVal",
      forwardHHObjptrArgs.objectsCopied);

    hh->retVal = objptrToPointer(root, s->heap->start);
  }

  LOG(LM_HH_COLLECTION, LL_DEBUG, "END root copy");

  /* do copy-collection */
  forwardHHObjptrArgs.objectsCopied = 0;
  /*
   * I skip the stack and thread since they are already forwarded as roots
   * above
   */
  struct SSATOPredicateArgs ssatoPredicateArgs = {
    .expectedStackPointer = objptrToPointer(getStackCurrentObjptr(s),
                                            s->heap->start),
    .expectedThreadPointer = objptrToPointer(getThreadCurrentObjptr(s),
                                             s->heap->start)
  };
  HM_forwardHHObjptrsInLevelList(
      s,
      &(hh->newLevelList),
      &skipStackAndThreadObjptrPredicate,
      ((void*)(&ssatoPredicateArgs)),
      &forwardHHObjptrArgs);
  LOG(LM_HH_COLLECTION, LL_DEBUG,
      "Copied %"PRIu64" objects in copy-collection",
      forwardHHObjptrArgs.objectsCopied);
  LOG(LM_HH_COLLECTION, LL_DEBUG,
      "Copied %"PRIu64" stacks in copy-collection",
      forwardHHObjptrArgs.stacksCopied);

  assertInvariants(s, hh, LIVE);

  /*
   * RAM_NOTE: Add hooks to forwardHHObjptr and freeChunks to count from/toBytes
   * instead of iterating
   */
#if 0
  if (DEBUG_HEAP_MANAGEMENT or s->controls->HMMessages) {
    /* count number of from-bytes */
    size_t fromBytes = 0;
    for (void* chunkList = hh->levelList;
         (NULL != chunkList) && (HM_getChunkListLevel(chunkList) >=
                                 forwardHHObjptrArgs.minLevel);
         chunkList = getChunkInfo(chunkList)->split.levelHead.nextHead) {
      for (void* chunk = chunkList;
           NULL != chunk;
           chunk = getChunkInfo(chunk)->nextChunk) {
        fromBytes += HM_getChunkLimit(chunk) - HM_getChunkStart(chunk);
      }
    }

    /* count number of to-chunks */
    size_t toBytes = 0;
    for (void* chunkList = hh->newLevelList;
         NULL != chunkList;
         chunkList = getChunkInfo(chunkList)->split.levelHead.nextHead) {
      for (void* chunk = chunkList;
           NULL != chunk;
           chunk = getChunkInfo(chunk)->nextChunk) {
        toBytes += HM_getChunkLimit(chunk) - HM_getChunkStart(chunk);
      }
    }

    LOG(LM_HH_COLLECTION, LL_INFO,
        "Collection went from %zu bytes to %zu bytes",
        fromBytes,
        toBytes);
  }
#endif

  /* free old chunks */
  HM_freeChunks(&(hh->levelList), forwardHHObjptrArgs.minLevel);

  /* merge newLevelList back in */
  HM_updateLevelListPointers(hh->newLevelList, hh);
  HM_mergeLevelList(&(hh->levelList), hh->newLevelList, hh);

  /*
   * RAM_NOTE: Really should get this off of forwardHHObjptrArgs instead of
   * summing up
   */
  /* update locally collectible size */
  /* off-by-one loop to prevent underflow and infinite loop */
  hh->locallyCollectibleSize = 0;
  Word32 level;
  for (level = hh->level;
       level > (HM_HH_getHighestStolenLevel(s, hh) + 1);
       level--) {
    hh->locallyCollectibleSize += HM_getLevelSize(hh->levelList, level);
  }
  hh->locallyCollectibleSize += HM_getLevelSize(hh->levelList, level);

  /* update lastAllocatedChunk and associated */
  void* lastChunk = HM_getChunkListLastChunk(hh->levelList);
  if (NULL == lastChunk) {
    /* empty lists, so reset hh */
    hh->lastAllocatedChunk = NULL;
  } else {
    /* we have a last chunk */
    hh->lastAllocatedChunk = lastChunk;
  }

  assertInvariants(s, hh, LIVE);

  /* RAM_NOTE: This can be moved earlier? */
  /* unlock hh and queue */
  unlockHH(hh);
  if (!queueLockHeld) {
    Parallel_lockRelease(wsQueueLock);
  }

  HM_debugMessage(s,
                  "[%d] HM_HH_collectLocal(): Finished Local collection on "
                  "HierarchicalHeap = %p\n",
                  processor,
                  ((void*)(hh)));

  s->cumulativeStatistics->bytesHHLocaled += forwardHHObjptrArgs.bytesCopied;

  /* enter statistics if necessary */
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

  Trace0(EVENT_GC_LEAVE);

  LOG(LM_HH_COLLECTION, LL_DEBUG,
      "END");
}

void forwardHHObjptr (GC_state s,
                      objptr* opp,
                      void* rawArgs) {
  struct ForwardHHObjptrArgs* args = ((struct ForwardHHObjptrArgs*)(rawArgs));
  objptr op = *opp;
  pointer p = objptrToPointer (op, s->heap->start);

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

  if (!HM_HH_objptrInHierarchicalHeap(s, op)) {
    /* does not point to an HH objptr, so not in scope for collection */
    LOG(LM_HH_COLLECTION, LL_DEBUGMORE,
        "skipping opp = "FMTPTR"  op = "FMTOBJPTR"  p = "FMTPTR": not in HH.",
        (uintptr_t)opp,
        op,
        (uintptr_t)p);
    return;
  }

  struct HM_ObjptrInfo opInfo;
  HM_getObjptrInfo(s, op, &opInfo);

  assert((COPY_OBJECT_HH_VALUE != opInfo.hh) && ("op is in the toHeap!"));

  if (opInfo.hh != args->hh) {
    /*
     * opp does not point to an HH objptr in my HH, so just return.
     */
    LOG(LM_HH_COLLECTION, LL_DEBUGMORE,
        "skipping opp = "FMTPTR"  op = "FMTOBJPTR"  p = "FMTPTR": opInfo.hh (%p) != args->hh (%p).",
        (uintptr_t)opp,
        op,
        (uintptr_t)p,
        ((void*)(opInfo.hh)),
        ((void*)(args->hh)));

#if ASSERT
    GC_header header;
    GC_objectTypeTag tag;
    uint16_t bytesNonObjptrs;
    uint16_t numObjptrs;

    for (struct HM_HierarchicalHeap* cursor = opInfo.hh;
         cursor != NULL;
         cursor = HM_HH_objptrToStruct(s, cursor->parentHH)) {
      header = getHeader(p);
      splitHeader(s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);

      ASSERTPRINT(cursor != args->hh,
                  "Pointer %p (h: %lx, t: %s BNO: %"PRIu16" NO: %"PRIu16") resides in hh %p which is a descendant of collecting hh %p",
                  ((void*)(objptrToPointer(op, s->heap->start))),
                  header,
                  objectTypeTagToString(tag),
                  bytesNonObjptrs,
                  numObjptrs,
                  ((void*)(opInfo.hh)),
                  ((void*)(args->hh)));
    }

    bool found = FALSE;
    for (struct HM_HierarchicalHeap* cursor = args->hh;
         cursor != NULL;
         cursor = HM_HH_objptrToStruct(s, cursor->parentHH)) {
      if (cursor == opInfo.hh) {
        found = TRUE;
        break;
      }
    }
    ASSERTPRINT(found,
                "Pointer %p (h: %lx, t: %s BNO: %"PRIu16" NO: %"PRIu16") resides in hh %p which is not an ancestor of collecting hh %p",
                ((void*)(objptrToPointer(op, s->heap->start))),
                header,
                objectTypeTagToString(tag),
                bytesNonObjptrs,
                numObjptrs,
                ((void*)(opInfo.hh)),
                ((void*)(args->hh)));
#endif

    return;
  }

  if (hasFwdPtr(p)) {
    if (DEBUG_DETAILED) {
      fprintf (stderr, "  already FORWARDED\n");
    }

    /* if forwarded, must be in my own HierarchicalHeap! */
    assert(opInfo.hh == args->hh);

    /* should not have forwarded anything below 'args->minLevel'! */
    assert(opInfo.level >= args->minLevel);
  }

  if (not (hasFwdPtr(p))) {
    /* RAM_NOTE: This is more nuanced with non-local collection */
    if ((opInfo.hh != args->hh) ||
        /* cannot forward any object below 'args->minLevel' */
        (opInfo.level < args->minLevel)) {
      LOG(LM_HH_COLLECTION, LL_DEBUGMORE,
          "skipping opp = "FMTPTR"  op = "FMTOBJPTR"  p = "FMTPTR": level %d < minLevel %d.",
          (uintptr_t)opp,
          op,
          (uintptr_t)p,
          opInfo.level,
          args->minLevel);
      return;
    }
    /* maybe forward the object */
    GC_header header;
    GC_objectTypeTag tag;
    uint16_t bytesNonObjptrs;
    uint16_t numObjptrs;
    header = getHeader(p);
    splitHeader(s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);

    /* Compute the space taken by the metadata and object body. */
    size_t metaDataBytes;
    size_t objectBytes;
    size_t copyBytes;
    if ((NORMAL_TAG == tag) or (WEAK_TAG == tag)) { /* Fixed size object. */
      if (WEAK_TAG == tag) {
        die(__FILE__ ":%d: "
            "forwardHHObjptr() #define oes not support WEAK_TAG objects!",
            __LINE__);
      }
      metaDataBytes = GC_NORMAL_METADATA_SIZE;
      objectBytes = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
      copyBytes = objectBytes;
    } else if (ARRAY_TAG == tag) {
      metaDataBytes = GC_ARRAY_METADATA_SIZE;
      objectBytes = sizeofArrayNoMetaData (s, getArrayLength (p),
                                           bytesNonObjptrs, numObjptrs);
      copyBytes = objectBytes;
    } else {
      /* Stack. */
      bool current;
      size_t reservedNew;
      GC_stack stack;

      assert (STACK_TAG == tag);
      metaDataBytes = GC_STACK_METADATA_SIZE;
      stack = (GC_stack)p;

      /* RAM_NOTE: This changes with non-local collection */
      /* Check if the pointer is the current stack of my processor. */
      current = getStackCurrent(s) == stack;

      reservedNew = sizeofStackShrinkReserved (s, stack, current);
      if (reservedNew < stack->reserved) {
        LOG(LM_HH_COLLECTION, LL_DEBUG,
            "Shrinking stack of size %s bytes to size %s bytes, using %s bytes.",
            uintmaxToCommaString(stack->reserved),
            uintmaxToCommaString(reservedNew),
            uintmaxToCommaString(stack->used));
        stack->reserved = reservedNew;
      }
      objectBytes = sizeof (struct GC_stack) + stack->reserved;
      copyBytes = sizeof (struct GC_stack) + stack->used;
      args->stacksCopied++;
    }

    objectBytes += metaDataBytes;
    copyBytes += metaDataBytes;
    /* Copy the object. */
    if (opInfo.level > args->maxLevel) {
      assert(FALSE && "Entanglement Detected!");
      DIE("Pointer Invariant violated!");
    }

    pointer copyPointer = copyObject(args->hh,
                                     p - metaDataBytes,
                                     objectBytes,
                                     copyBytes,
                                     opInfo.level,
                                     opInfo.chunkList);

    args->bytesCopied += copyBytes;
    args->objectsCopied++;
    LOG(LM_HH_COLLECTION, LL_DEBUGMORE,
        "%p --> %p", ((void*)(p - metaDataBytes)), ((void*)(copyPointer)));

    if ((WEAK_TAG == tag) and (numObjptrs == 1)) {
#if 0
      /* RAM_NOTE: This is the saved code from forward...() */
      GC_weak w;

      w = (GC_weak)(s->forwardState.back + GC_NORMAL_HEADER_SIZE + offsetofWeak (s));
      if (DEBUG_WEAK)
        fprintf (stderr, "forwarding weak "FMTPTR" ",
                 (uintptr_t)w);
      if (isObjptr (w->objptr)
          and (not s->forwardState.amInMinorGC
               or isObjptrInNursery (s, w->objptr))) {
        if (DEBUG_WEAK)
          fprintf (stderr, "linking\n");
        w->link = s->weaks;
        s->weaks = w;
      } else {
        if (DEBUG_WEAK)
          fprintf (stderr, "not linking\n");
      }
#else
    die(__FILE__ ":%d: "
        "forwardHHObjptr() does not support WEAK_TAG objects!",
        __LINE__);
#endif
    }

    /* Store the forwarding pointer in the old object metadata. */
    *(getFwdPtrp(p)) = pointerToObjptr (copyPointer + metaDataBytes,
                                        s->heap->start);
    assert (hasFwdPtr(p));

    if (GC_HIERARCHICAL_HEAP_HEADER == header) {
      die(__FILE__ ":%d: "
          "forwardHHObjptr() does not support GC_HIERARCHICAL_HEAP_HEADER "
          "objects!",
          __LINE__);
    }
  }


  *opp = getFwdPtr(p);
  LOG(LM_HH_COLLECTION, LL_DEBUGMORE,
      "opp "FMTPTR" set to "FMTOBJPTR,
      ((uintptr_t)(opp)),
      *opp);

#if ASSERT
  /* args->hh->newLevelList has containingHH set to COPY_OBJECT_HH_VALUE */
  HM_getObjptrInfo(s, *opp, &opInfo);
  assert(COPY_OBJECT_HH_VALUE == opInfo.hh);
#endif
}
#endif /* MLTON_GC_INTERNAL_BASIS */

pointer copyObject(struct HM_HierarchicalHeap* hh,
                   pointer p,
                   size_t objectSize,
                   size_t copySize,
                   Word32 level,
                   void* fromChunkList) {
  /* get the saved level head */
  void* chunkList = HM_getChunkListToChunkList(fromChunkList);
#if ASSERT
  if (NULL == chunkList) {
    void* cursor;
    for (cursor = hh->newLevelList;
         (NULL != cursor) && (HM_getChunkListLevel(cursor) > level);
         cursor = getChunkInfo(cursor)->split.levelHead.nextHead) {
    }
    assert((NULL == cursor) || (HM_getChunkListLevel(cursor) != level));
  } else {
    assert(HM_getChunkListLevel(chunkList) == level);

    void* cursor;
    for (cursor = hh->newLevelList;
         (NULL != cursor) && (HM_getChunkListLevel(cursor) > level);
         cursor = getChunkInfo(cursor)->split.levelHead.nextHead) {
    }
    assert(chunkList == cursor);
  }
#endif

  /* get the chunk to allocate in */
  void* chunk;
  if (NULL == chunkList) {
    /* Level does not exist, so create it */
    chunk = HM_allocateLevelHeadChunk(&(hh->newLevelList),
                                      objectSize,
                                      level,
                                      COPY_OBJECT_HH_VALUE);
    if (NULL == chunk) {
      die(__FILE__ ":%d: Ran out of space for Hierarchical Heap!", __LINE__);
    }

    /* update toChunkList for fast access later */
    HM_setChunkListToChunkList(fromChunkList, chunk);
  } else {
    /* level exists, get the chunk from it */
    chunk = HM_getChunkListLastChunk(chunkList);
    void* frontier = HM_getChunkFrontier(chunk);
    void* limit = HM_getChunkLimit(chunk);

    if (((size_t)(((char*)(limit)) - ((char*)(frontier)))) < objectSize) {
      /* need to allocate a new chunk */
      chunk = HM_allocateChunk(chunkList, objectSize);
      if (NULL == chunk) {
        die(__FILE__ ":%d: Ran out of space for Hierarchical Heap!", __LINE__);
      }
    }
  }

  /* get frontier of chunk and do the copy */
  void* frontier = HM_getChunkFrontier(chunk);
  GC_memcpy(p, frontier, copySize);
  HM_updateChunkValues(chunk, ((void*)(((char*)(frontier)) + objectSize)));

  return frontier;
}

void populateGlobalHeapHoles(GC_state s, struct GlobalHeapHole* holes) {
  for (uint32_t i = 0; i < s->numberOfProcs; i++) {
    spinlock_lock(&(s->procStates[i].lock), Proc_processorNumber(s));

    pointer start = s->procStates[i].frontier;
    pointer end = s->procStates[i].limitPlusSlop + GC_BONUS_SLOP;

    if (HM_HH_objptrInHierarchicalHeap(s, pointerToObjptr(start,
                                                          s->heap->start))) {
#if 0
      assert(HM_HH_objptrInHierarchicalHeap(s,
                                            pointerToObjptr(end,
                                                            s->heap->start)));
#endif

      /* use the saved global frontier */
      start = s->procStates[i].globalFrontier;
      end = s->procStates[i].globalLimitPlusSlop + GC_BONUS_SLOP;
    }

    holes[i].start = start;
    holes[i].end = end;

    spinlock_unlock(&(s->procStates[i].lock));
  }
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

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
 * ObjptrPredicateFunction for skipping global heap objects we don't care about
 */
bool globalHeapObjptrPredicate(GC_state s, pointer p, void* ignored);

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

  if (NONE == s->controls->hhCollectionLevel) {
    /* collection disabled */
    return;
  }

  if (Parallel_alreadyLockedByMe(wsQueueLock)) {
    /* in a scheduler critical section, so cannot collect */
    LOG(LM_HH_COLLECTION, LL_DEBUG,
        "Queue locked by mutator/scheduler so cannot collect");
    return;
  }

  LOG(LM_HH_COLLECTION, LL_DEBUG,
      "START");

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
  Parallel_lockTake(wsQueueLock);
  lockHH(hh);

  assertInvariants(s, hh);

  /* copy roots */
  struct ForwardHHObjptrArgs forwardHHObjptrArgs = {
    .hh = hh,
    .minLevel = HM_HH_getHighestStolenLevel(s, hh) + 1,
    .maxLevel = hh->level,
    .bytesCopied = 0,
    .objectsCopied = 0
  };

  if (SUPERLOCAL == s->controls->hhCollectionLevel) {
    forwardHHObjptrArgs.minLevel = hh->level;
  }

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

  /* forward thread (and therefore stack) */
  forwardHHObjptrArgs.objectsCopied = 0;
  forwardHHObjptr(s, &(s->currentThread), &forwardHHObjptrArgs);
  LOG(LM_HH_COLLECTION, LL_DEBUG,
      "Copied %"PRIu64" objects from thread",
      forwardHHObjptrArgs.objectsCopied);

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

#if 0
  LOG(LM_HH_COLLECTION, LL_DEBUG,
      "START foreach %ld MB",
      (s->heap->frontier - s->heap->start) / (1024 * 1024));

#pragma message "Should be synchronized, but entrypoint for enter/leave is " \
  "GC_collect :("
  assert(s->numberOfProcs <= MAX_NUM_HOLES);
  struct GlobalHeapHole holes[MAX_NUM_HOLES];
  populateGlobalHeapHoles(s, holes);
  pointer frontier = ((pointer)(0));
  for (uint32_t i = 0; i < s->numberOfProcs; i++) {
    if (frontier < holes[i].start) {
      frontier = holes[i].start;
    }
  }

  forwardHHObjptrArgs.objectsCopied = 0;
  /* forward global heap */
  foreachObjptrInRange(s,
                       s->heap->start,
                       &frontier,
                       TRUE,
                       holes,
                       globalHeapObjptrPredicate,
                       NULL,
                       forwardHHObjptr,
                       &forwardHHObjptrArgs);
  LOG(LM_HH_COLLECTION, LL_DEBUG,
      "Copied %"PRIu64" objects from global heap",
      forwardHHObjptrArgs.objectsCopied);
#endif

  LOG(LM_HH_COLLECTION, LL_DEBUG, "END foreach");

  /* do copy-collection */
  forwardHHObjptrArgs.objectsCopied = 0;
  HM_forwardHHObjptrsInLevelList(s, &(hh->newLevelList), &forwardHHObjptrArgs);
  LOG(LM_HH_COLLECTION, LL_DEBUG,
      "Copied %"PRIu64" objects in copy-collection",
      forwardHHObjptrArgs.objectsCopied);

  assertInvariants(s, hh);

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

  assertInvariants(s, hh);

  /* RAM_NOTE: This can be moved earlier? */
  /* unlock hh and queue */
  unlockHH(hh);
  Parallel_lockRelease(objptrToPointer(s->wsQueueLock, s->heap->start));

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

  LOG(LM_HH_COLLECTION, LL_DEBUG,
      "END");
}

void forwardHHObjptr (GC_state s,
                      objptr* opp,
                      void* rawArgs) {
  struct ForwardHHObjptrArgs* args = ((struct ForwardHHObjptrArgs*)(rawArgs));
  objptr op = *opp;
  pointer p = objptrToPointer (op, s->heap->start);
  GC_header header;

  if (DEBUG_DETAILED) {
    fprintf (stderr,
             "forwardHHObjptr  opp = "FMTPTR"  op = "FMTOBJPTR"  p = "
             ""FMTPTR"\n",
             (uintptr_t)opp,
             op,
             (uintptr_t)p);
  }

  LOG(LM_HH_COLLECTION, LL_DEBUG,
      "opp = "FMTPTR"  op = "FMTOBJPTR"  p = "FMTPTR,
      (uintptr_t)opp,
      op,
      (uintptr_t)p);

  if (!HM_HH_objptrInHierarchicalHeap(s, op)) {
    /* does not point to an HH objptr, so not in scope for collection */
    LOG(LM_HH_COLLECTION, LL_DEBUG,
        "skipping opp = "FMTPTR"  op = "FMTOBJPTR"  p = "FMTPTR": not in HH.",
        (uintptr_t)opp,
        op,
        (uintptr_t)p);
    return;
  }

  struct HM_ObjptrInfo opInfo;
  bool gotObjptrInfo = HM_getObjptrInfo(s, op, &opInfo);

  if ((!gotObjptrInfo) || (opInfo.hh != args->hh)) {
    /*
     * Either did not successfully get the objptr info (which means I don't own
     * it) or opp does not point to an HH objptr in my HH, so just return.
     */
    if (!gotObjptrInfo) {
      LOG(LM_HH_COLLECTION, LL_DEBUG,
          "skipping opp = "FMTPTR"  op = "FMTOBJPTR"  p = "FMTPTR": could not get objptrInfo.",
          (uintptr_t)opp,
          op,
          (uintptr_t)p);
    }

    if (opInfo.hh != args->hh) {
      LOG(LM_HH_COLLECTION, LL_DEBUG,
          "skipping opp = "FMTPTR"  op = "FMTOBJPTR"  p = "FMTPTR": opInfo.hh (%p) != args->hh (%p).",
          (uintptr_t)opp,
          op,
          (uintptr_t)p,
          ((void*)(opInfo.hh)),
          ((void*)(args->hh)));
    }

    return;
  }

  header = getHeader (p);
  if (GC_FORWARDED == header) {
    if (DEBUG_DETAILED) {
      fprintf (stderr, "  already FORWARDED\n");
    }

    /* if forwarded, must be in my own HierarchicalHeap! */
    assert(opInfo.hh == args->hh);

    /* should not have forwarded anything below 'args->minLevel'! */
    assert(opInfo.level >= args->minLevel);
  }

  if (GC_FORWARDED != header) {
#pragma message "More nuanced with non-local collection"
    if ((opInfo.hh != args->hh) ||
        /* cannot forward any object below 'args->minLevel' */
        (opInfo.level < args->minLevel)) {
      LOG(LM_HH_COLLECTION, LL_DEBUG,
          "skipping opp = "FMTPTR"  op = "FMTOBJPTR"  p = "FMTPTR": level %d < minLevel %d.",
          (uintptr_t)opp,
          op,
          (uintptr_t)p,
          opInfo.level,
          args->minLevel);
      return;
    }
    /* maybe forward the object */
    GC_objectTypeTag tag;
    uint16_t bytesNonObjptrs;
    uint16_t numObjptrs;
    splitHeader(s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);

    /* Compute the space taken by the header and object body. */
    size_t headerBytes;
    size_t objectBytes;
    size_t copyBytes;
    if ((NORMAL_TAG == tag) or (WEAK_TAG == tag)) { /* Fixed size object. */
#pragma message "Implement when I can"
#if 0
#else
      if (WEAK_TAG == tag) {
        die(__FILE__ ":%d: "
            "forwardHHObjptr() #define oes not support WEAK_TAG objects!",
            __LINE__);
      }
#endif
      headerBytes = GC_NORMAL_HEADER_SIZE;
      objectBytes = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
      copyBytes = objectBytes;
    } else if (ARRAY_TAG == tag) {
      headerBytes = GC_ARRAY_HEADER_SIZE;
      objectBytes = sizeofArrayNoHeader(s,
                                        getArrayLength (p),
                                        bytesNonObjptrs,
                                        numObjptrs);
      copyBytes = objectBytes;
    } else {
      /* Stack. */
      bool current;
      size_t reservedNew;
      GC_stack stack;

      assert (STACK_TAG == tag);
      headerBytes = GC_STACK_HEADER_SIZE;
      stack = (GC_stack)p;

#pragma message "This changes with non-local collection"
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
      objectBytes = stack->reserved;
      copyBytes = sizeof (struct GC_stack) + stack->used;
    }

    objectBytes += headerBytes;
    copyBytes += headerBytes;
    /* Copy the object. */
    if (opInfo.level > args->maxLevel) {
      assert(FALSE && "Entanglement Detected!");
      DIE("Pointer Invariant violated!");
    }

    pointer copyPointer = copyObject(args->hh,
                                     p - headerBytes,
                                     objectBytes,
                                     copyBytes,
                                     opInfo.level,
                                     opInfo.chunkList);

    args->bytesCopied += copyBytes;
    args->objectsCopied++;
    LOG(LM_HH_COLLECTION, LL_DEBUG,
        "%p --> %p", ((void*)(p - headerBytes)), ((void*)(copyPointer)));

    if ((WEAK_TAG == tag) and (numObjptrs == 1)) {
#pragma message "Implement when I can"
#if 0
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

    /* Store the forwarding pointer in the old object. */
    *((GC_header*)(p - GC_HEADER_SIZE)) = GC_FORWARDED;
    *((objptr*)(p)) = pointerToObjptr(copyPointer + headerBytes,
                                      s->heap->start);

    if (GC_HIERARCHICAL_HEAP_HEADER == header) {
#pragma message "Shouldn't happen!"
#if 0
      /* update level chunk head containingHH pointers */
      HM_HH_updateLevelListPointers(*((objptr*)(p)));
#else
    die(__FILE__ ":%d: "
        "forwardHHObjptr() does not support GC_HIERARCHICAL_HEAP_HEADER "
        "objects!",
        __LINE__);
#endif
    }
  }


  *opp = *((objptr*)(p));
  LOG(LM_HH_COLLECTION, LL_DEBUG,
      "opp "FMTPTR" set to "FMTOBJPTR,
      ((uintptr_t)(opp)),
      *opp);

#if ASSERT
  /* args->hh->newLevelList has containingHH set to COPY_OBJECT_HH_VALUE */
  gotObjptrInfo = HM_getObjptrInfo(s, *opp, &opInfo);
  assert(gotObjptrInfo);
  assert(args->hh == opInfo.hh);
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
                                      hh);
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

#pragma message "Resolve when done"
#if 0
bool globalHeapObjptrPredicate(GC_state s, pointer p, void* ignored) {
  /* silence compliler */
  ((void)(s));
  ((void)(ignored));

  /* run through FALSE cases */
  GC_header header;
  header = getHeader(p);
  if ((GC_STACK_HEADER == header) ||
      (GC_THREAD_HEADER == header) ||
      (GC_HIERARCHICAL_HEAP_HEADER == header)) {
    return FALSE;
  }

  return TRUE;
}
#else
bool globalHeapObjptrPredicate(GC_state s, pointer p, void* ignored) {
  /* silence compliler */
  ((void)(s));
  ((void)(ignored));

  /* run through FALSE cases */
  GC_header header;
  header = getHeader(p);
  if ((objptrToPointer(s->wsQueue, s->heap->start) == p) ||
      (GC_STACK_HEADER == header) ||
      (GC_THREAD_HEADER == header) ||
      (GC_HIERARCHICAL_HEAP_HEADER == header)) {
    return FALSE;
  }

  return TRUE;
}
#endif

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

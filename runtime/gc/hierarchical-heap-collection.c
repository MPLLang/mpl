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
 * Copies the object into the destination level list
 *
 * @param destinationLevelList The level list to copy the object into
 * @param p The pointer to copy
 * @param size The number of bytes to copy
 * @param level The level to copy into
 *
 * @return pointer to the copied object
 */
pointer copyObject(void** destinationLevelList,
                   pointer p,
                   size_t size,
                   Word32 level);

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

  if (NONE == s->controls->hhCollectionLevel) {
    /* collection disabled */
    return;
  }

  if (needGCTime(s)) {
    startTiming (RUSAGE_THREAD, &ru_start);
  }
  s->cumulativeStatistics->numHHLocalGCs++;

  /* used needs to be set because the mutator has changed s->stackTop. */
  getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
  getThreadCurrent(s)->exnStack = s->exnStack;

  assertInvariants(s, hh);

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
  Parallel_lockTake(objptrToPointer(s->wsQueueLock, s->heap->start));

  /* copy roots */
  struct ForwardHHObjptrArgs forwardHHObjptrArgs = {
    .hh = hh,
    .minLevel = HM_HH_getHighestStolenLevel(s, hh) + 1,
    .maxLevel = hh->level,
    .log = TRUE
  };

  if (SUPERLOCAL == s->controls->hhCollectionLevel) {
    forwardHHObjptrArgs.minLevel = hh->level;
  }

  foreachObjptrInObject(s,
                        objptrToPointer(getStackCurrentObjptr(s),
                                        s->heap->start),
                        FALSE,
                        trueObjptrPredicate,
                        NULL,
                        forwardHHObjptr,
                        &forwardHHObjptrArgs);

  LOG(TRUE, TRUE, L_DEBUG,
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

  foreachObjptrInRange(s,
                       s->heap->start,
                       &frontier,
                       TRUE,
                       holes,
                       globalHeapObjptrPredicate,
                       NULL,
                       forwardHHObjptr,
                       &forwardHHObjptrArgs);

  LOG(TRUE, TRUE, L_DEBUG, "END foreach");

  /* do copy-collection */
  HM_forwardHHObjptrsInLevelList(s,
                                 &(hh->newLevelList),
                                 hh,
                                 forwardHHObjptrArgs.minLevel);

  assertInvariants(s, hh);

  /* free old chunks */
  HM_freeChunks(&(hh->levelList), forwardHHObjptrArgs.minLevel);

  /* merge newLevelList back in */
  HM_updateLevelListPointers(hh->newLevelList, hh);
  HM_mergeLevelList(&(hh->levelList), hh->newLevelList);

  /* update lastAllocatedChunk and associated */
  void* lastChunk = HM_getChunkListLastChunk(hh->levelList);
  if (NULL == lastChunk) {
    /* empty lists, so reset hh */
    hh->lastAllocatedChunk = NULL;
  } else {
    /* we have a last chunk */
    hh->lastAllocatedChunk = lastChunk;
  }

  /* RAM_NOTE: This can be moved earlier? */
  /* unlock queue */
  Parallel_lockRelease(objptrToPointer(s->wsQueueLock, s->heap->start));

  assertInvariants(s, hh);

  HM_debugMessage(s,
                  "[%d] HM_HH_collectLocal(): Finished Local collection on "
                  "HierarchicalHeap = %p\n",
                  processor,
                  ((void*)(hh)));

  /* enter timing info if necessary */
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

  if ((!HM_HH_objptrInHierarchicalHeap(s, *opp)) ||
      (HM_HH_getContaining(s, op) != args->hh)) {
    /* does not point to an HH objptr in my HH, so just return */
    return;
  }

  header = getHeader (p);
  if (GC_FORWARDED == header) {
    if (DEBUG_DETAILED) {
      fprintf (stderr, "  already FORWARDED\n");
    }

    /* if forwarded, must be in my own HierarchicalHeap! */
    assert(HM_HH_getContaining(s, op) == args->hh);

    /* should not have forwarded anything below 'args->minLevel'! */
    assert(HM_HH_getObjptrLevel(s, op) >= args->minLevel);
  }

  if (GC_FORWARDED != header) {
    Word32 level = HM_HH_getObjptrLevel(s, op);
#pragma message "More nuanced with non-local collection"
    if ((HM_HH_getContaining(s, op) != args->hh) ||
        /* cannot forward any object below 'args->minLevel' */
        (level < args->minLevel)) {
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
    } else if (ARRAY_TAG == tag) {
#pragma message "Implement when I can"
#if 0
      headerBytes = GC_ARRAY_HEADER_SIZE;
      objectBytes = sizeofArrayNoHeader (s, getArrayLength (p),
                                         bytesNonObjptrs, numObjptrs);
#else
    die(__FILE__ ":%d: "
        "forwardHHObjptr() does not support ARRAY_TAG objects!",
        __LINE__);
#endif
    } else {
      /* Stack. */
#pragma message "Implement when I can"
#if 0
      bool current;
      size_t reservedNew;
      GC_stack stack;

      assert (STACK_TAG == tag);
      headerBytes = GC_STACK_HEADER_SIZE;
      stack = (GC_stack)p;

      /* Check if the pointer is the current stack of any processor. */
      current = false;
      for (int proc = 0; proc < s->numberOfProcs; proc++) {
        current = current || (getStackCurrent(&s->procStates[proc]) == stack);
      }
      /* RAM_NOTE: used to have 'current &&= not isStackEmpty(stack)' here */

      reservedNew = sizeofStackShrinkReserved (s, stack, current);
      if (reservedNew < stack->reserved) {
        if (DEBUG_STACKS or s->controls->messages)
          fprintf (stderr,
                   "[GC: Shrinking stack of size %s bytes to size %s bytes, using %s bytes.]\n",
                   uintmaxToCommaString(stack->reserved),
                   uintmaxToCommaString(reservedNew),
                   uintmaxToCommaString(stack->used));
        stack->reserved = reservedNew;
      }
      objectBytes = sizeof (struct GC_stack) + stack->used;
#else
    die(__FILE__ ":%d: "
        "forwardHHObjptr() does not support STACK_TAG objects!",
        __LINE__);
#endif
    }

    size_t size = headerBytes + objectBytes;
    /* Copy the object. */
    pointer copyPointer = copyObject(&(args->hh->newLevelList),
                                     p - headerBytes,
                                     size,
                                     (level > args->maxLevel) ? (args->maxLevel) : (level));
    LOG(args->log, TRUE, L_DEBUG,
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
  if (DEBUG_DETAILED) {
    fprintf (stderr,
             "forwardHHObjptr --> *opp = "FMTPTR"\n",
             (uintptr_t)*opp);
  }

  /* args->hh->newLevelList has containingHH set to NULL */
  assert (NULL == HM_HH_getContaining(s, *opp));
}
#endif /* MLTON_GC_INTERNAL_BASIS */

pointer copyObject(void** destinationLevelList,
                   pointer p,
                   size_t size,
                   Word32 level) {
  void* cursor;
  for (cursor = *destinationLevelList;
       (NULL != cursor) && (HM_getChunkListLevel(cursor) > level);
       cursor = getChunkInfo(cursor)->split.levelHead.nextHead) {
  }

  void* chunk;
  void* frontier;
  if (NULL == cursor) {
    /* need to create a new level */
    void* chunkEnd;
    chunk = HM_allocateLevelHeadChunk(destinationLevelList,
                                      &chunkEnd,
                                      size,
                                      level,
                                      NULL);
    if (NULL == chunk) {
      die(__FILE__ ":%d: Ran out of space for Hierarchical Heap!", __LINE__);
    }
    frontier = HM_getChunkFrontier(chunk);
  } else {
    /* found the level, try to allocate in it */
    chunk = HM_getChunkListLastChunk(cursor);
    frontier = HM_getChunkFrontier(chunk);
    void* limit = HM_getChunkLimit(chunk);

    if (((size_t)(((char*)(limit)) - ((char*)(frontier)))) < size) {
      /* need to allocate a new chunk */
      void* chunkEnd;
      chunk = HM_allocateChunk(cursor, &chunkEnd, size);
      if (NULL == chunk) {
        die(__FILE__ ":%d: Ran out of space for Hierarchical Heap!", __LINE__);
      }
      frontier = HM_getChunkFrontier(chunk);
    }
  }

  GC_memcpy(p, frontier, size);
  HM_updateChunkValues(chunk, ((void*)(((char*)(frontier)) + size)));

  return frontier;
}

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

void populateGlobalHeapHoles(GC_state s, struct GlobalHeapHole* holes) {
  for (uint32_t i = 0; i < s->numberOfProcs; i++) {
    pointer start = s->procStates[i].frontier;
    pointer end = s->procStates[i].limitPlusSlop + GC_BONUS_SLOP;

    if (HM_HH_objptrInHierarchicalHeap(s, pointerToObjptr(start,
                                                          s->heap->start))) {
      assert(HM_HH_objptrInHierarchicalHeap(s,
                                            pointerToObjptr(end,
                                                            s->heap->start)));
      /* use the saved global frontier */
      start = s->procStates[i].globalFrontier;
      end = s->procStates[i].globalLimitPlusSlop + GC_BONUS_SLOP;
    }

    holes[i].start = start;
    holes[i].end = end;
  }
}

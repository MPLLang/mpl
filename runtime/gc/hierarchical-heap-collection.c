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

/**********************/
/* Struct Definitions */
/**********************/

struct TraceHHObjptrAscendHookArgs {
  HHObjptrFunction f;
  struct HHObjptrFunctionArgs* fArgs;
};

/******************************/
/* Static Function Prototypes */
/******************************/
/**
 * Calls 'f' on 'p' if it is a objptr to a object in the Hierarchical Heap
 *
 * @param s The GC_state to use
 * @param f The function to call
 * @param fArgs The args to pass to to 'f'
 * @param opp The objptr* to pass to 'f'
 */
static void callIfIsHHObjptr (GC_state s,
                              HHObjptrFunction f,
                              struct HHObjptrFunctionArgs* fArgs,
                              objptr* opp);

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
 * Forwards the object pointed to by 'opp' into 'destinationLevelList'
 *
 * @param s The GC_state to use
 * @param args The additional args for this call
 * @param opp The objptr to forward
 */
void forwardHHObjptr (GC_state s,
                      struct HHObjptrFunctionArgs* args,
                      objptr* opp);

/**
 * Calls 'f' on every object in the Hierarchcical Heap reachable from 'p'.
 *
 * @param s The GC_state to use
 * @param f The function to call
 * @param fArgs The args to pass to to 'f'
 * @param objectObjptr The objptr* to trace
 */
void traceForHHObjptr(GC_state s,
                      HHObjptrFunction f,
                      struct HHObjptrFunctionArgs* fArgs,
                      objptr* objectObjptr);

/**
 * The descend hook used by traceForHHObjptr()
 */
bool traceHHObjptrDescendHook(GC_state s, void* rawArgs, objptr object);

/**
 * The ascend hook used by traceForHHObjptr()
 */
void traceHHObjptrAscendHook(GC_state s, void* rawArgs, objptr* objectObjptr);

/************************/
/* Function Definitions */
/************************/
#if (defined (MLTON_GC_INTERNAL_BASIS))
void HM_HHC_registerQueue(int processor, pointer queuePointer) {
  GC_state s = pthread_getspecific (gcstate_key);

  assert(processor < s->numberOfProcs);
  assert(!HM_HH_objptrInHierarchicalHeap(s, pointerToObjptr (queuePointer,
                                                             s->heap->start)));

  s->procStates[processor].wsQueue = pointerToObjptr (queuePointer,
                                                      s->heap->start);
}

void HM_HHC_registerQueueLock(int processor, pointer queueLockPointer) {
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

  int processor = s->procStates ? Proc_processorNumber (s) : -1;

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
  struct HHObjptrFunctionArgs hhObjptrFunctionArgs = {
    .hh = hh,
    .minLevel = hh->lastSharedLevel + 1,
    .maxLevel = hh->level
  };

  if (SUPERLOCAL == s->controls->hhCollectionLevel) {
    hhObjptrFunctionArgs.minLevel = hh->level;
  }

  HM_HHC_foreachHHObjptrInObject(s,
                                 objptrToPointer(getStackCurrentObjptr(s),
                                                 s->heap->start),
                                 FALSE,
                                 forwardHHObjptr,
                                 &hhObjptrFunctionArgs);
  HM_HHC_foreachHHObjptrInObject(s,
                                 objptrToPointer(s->wsQueue, s->heap->start),
                                 TRUE,
                                 forwardHHObjptr,
                                 &hhObjptrFunctionArgs);

  /* do copy-collection */
  HM_foreachHHObjptrInLevelList(s,
                                &(hh->newLevelList),
                                forwardHHObjptr,
                                hh,
                                hh->lastSharedLevel + 1);

  assertInvariants(s, hh);

  /* free old chunks */
  HM_freeChunks(&(hh->levelList), hh->lastSharedLevel + 1);

  /* merge newLevelList back in */
  HM_updateLevelListPointers(hh->newLevelList, hh);
  HM_mergeLevelList(&(hh->levelList), hh->newLevelList);

  /* update lastAllocatedChunk and associated */
  void* lastChunk = HM_getChunkListLastChunk(hh->levelList);
  if (NULL == lastChunk) {
    /* empty lists, so reset hh */
    hh->savedFrontier = NULL;
    hh->limit = NULL;
    hh->lastAllocatedChunk = NULL;
  } else {
    /* we have a last chunk */
    hh->savedFrontier = HM_getChunkFrontier(lastChunk);
    hh->limit = HM_getChunkLimit(lastChunk);
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

pointer HM_HHC_foreachHHObjptrInObject(GC_state s,
                                       pointer p,
                                       bool traceObject,
                                       HHObjptrFunction f,
                                       struct HHObjptrFunctionArgs* fArgs) {
  GC_header header;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
  GC_objectTypeTag tag;

  assertInvariants(s, fArgs->hh);

  header = getHeader (p);
  splitHeader(s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);
  if (DEBUG_DETAILED)
    fprintf (stderr,
             "foreachHHObjptrInObject ("FMTPTR")"
             "  header = "FMTHDR
             "  tag = %s"
             "  bytesNonObjptrs = %d"
             "  numObjptrs = %d\n",
             (uintptr_t)p, header, objectTypeTagToString (tag),
             bytesNonObjptrs, numObjptrs);
  if (NORMAL_TAG == tag) {
    p += bytesNonObjptrs;
    pointer max = p + (numObjptrs * OBJPTR_SIZE);
    /* Apply f to all internal pointers. */
    for ( ; p < max; p += OBJPTR_SIZE) {
      if (DEBUG_DETAILED) {
        fprintf (stderr,
                 "  p = "FMTPTR"  *p = "FMTOBJPTR"\n",
                 (uintptr_t)p, *(objptr*)p);
      }

      if (traceObject) {
        traceForHHObjptr(s,
                         f,
                         fArgs,
                         ((objptr*)(p)));
      } else {
        callIfIsHHObjptr (s,
                          f,
                          fArgs,
                          (objptr*)p);
      }
    }
  } else if (WEAK_TAG == tag) {
#pragma message "Implement when I can"
#if 0
    p += bytesNonObjptrs;
    if (1 == numObjptrs) {
      if (not skipWeaks) {
        if (traceObject) {
          traceForHHObjptr(s,
                           f,
                           fArgs,
                           ((objptr*)(p)));
        } else {
          callIfIsHHObjptr (s,
                            f,
                            fArgs,
                            (objptr*)p);
        }
      }
      p += OBJPTR_SIZE;
    }
#else
    die(__FILE__ ":%d: "
        "foreachHHObjptrInObject() does not support Weak objects!",
        __LINE__);
#endif
  } else if (ARRAY_TAG == tag) {
    size_t bytesPerElement;
    size_t dataBytes;
    pointer last;
    GC_arrayLength numElements;

    numElements = getArrayLength (p);
    bytesPerElement = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
    dataBytes = numElements * bytesPerElement;
    if (dataBytes < OBJPTR_SIZE) {
      /* Very small (including empty) arrays have OBJPTR_SIZE bytes
       * space for the forwarding pointer.
       */
      dataBytes = OBJPTR_SIZE;
    } else if (0 == numObjptrs) {
      /* No objptrs to process. */
      ;
    } else {
      last = p + dataBytes;
      if (0 == bytesNonObjptrs)
        /* Array with only pointers. */
        for ( ; p < last; p += OBJPTR_SIZE) {
          if (traceObject) {
            traceForHHObjptr(s,
                             f,
                             fArgs,
                             ((objptr*)(p)));
          } else {
            callIfIsHHObjptr (s,
                              f,
                              fArgs,
                              (objptr*)p);
          }
        }
      else {
        /* Array with a mix of pointers and non-pointers. */
        size_t bytesObjptrs;

        bytesObjptrs = numObjptrs * OBJPTR_SIZE;

        /* For each array element. */
        for ( ; p < last; ) {
          pointer next;

          /* Skip the non-pointers. */
          p += bytesNonObjptrs;
          next = p + bytesObjptrs;
          /* For each internal pointer. */
          for ( ; p < next; p += OBJPTR_SIZE) {
            if (traceObject) {
              traceForHHObjptr(s,
                               f,
                               fArgs,
                               ((objptr*)(p)));
            } else {
              callIfIsHHObjptr (s,
                                f,
                                fArgs,
                                (objptr*)p);
            }
          }
        }
      }
      assert (p == last);
      p -= dataBytes;
    }
    p += alignWithExtra (s, dataBytes, GC_ARRAY_HEADER_SIZE);
  } else if (STACK_TAG == tag) {
    GC_stack stack;
    pointer top, bottom;
    unsigned int i;
    GC_returnAddress returnAddress;
    GC_frameLayout frameLayout;
    GC_frameOffsets frameOffsets;

    stack = (GC_stack)p;
    bottom = getStackBottom (s, stack);
    top = getStackTop (s, stack);
    if (DEBUG) {
      fprintf (stderr, "  bottom = "FMTPTR"  top = "FMTPTR"\n",
               (uintptr_t)bottom, (uintptr_t)top);
    }
    assert (stack->used <= stack->reserved);
    while (top > bottom) {
      /* Invariant: top points just past a "return address". */
      returnAddress = *((GC_returnAddress*)(top - GC_RETURNADDRESS_SIZE));
      if (DEBUG) {
        fprintf (stderr, "  top = "FMTPTR"  return address = "FMTRA"\n",
                 (uintptr_t)top, returnAddress);
      }
      frameLayout = getFrameLayoutFromReturnAddress (s, returnAddress);
      frameOffsets = frameLayout->offsets;
      top -= frameLayout->size;
      for (i = 0 ; i < frameOffsets[0] ; ++i) {
        if (DEBUG) {
          fprintf(stderr, "  offset %"PRIx16"  address "FMTOBJPTR"\n",
                  frameOffsets[i + 1], *(objptr*)(top + frameOffsets[i + 1]));
        }

        if (traceObject) {
          traceForHHObjptr(s,
                           f,
                           fArgs,
                           ((objptr*)(top + frameOffsets[i + 1])));
        } else {
          callIfIsHHObjptr (s,
                            f,
                            fArgs,
                            (objptr*)(top + frameOffsets[i + 1]));
        }
      }
    }
    assert(top == bottom);
    p += sizeof (struct GC_stack) + stack->reserved;
  }
  else if (HEADER_ONLY_TAG == tag) {
#pragma message "Implement when I can"
#if 0
#else
    die(__FILE__ ":%d: "
        "foreachHHObjptrInObject() does not support HEADER_ONLY objects!",
        __LINE__);
#endif
  }
  else if (FILL_TAG == tag) {
#pragma message "Implement when I can"
#if 0
    GC_smallGapSize bytes;
    bytes = *((GC_smallGapSize *)p);
    p += GC_SMALL_GAP_SIZE_SIZE;
    p += bytes;
#else
    die(__FILE__ ":%d: "
        "foreachHHObjptrInObject() does not support FILL_TAG objects!",
        __LINE__);
#endif
  }
  else {
    assert (0 and "unknown object tag type");
  }

  return p;
}
#endif /* MLTON_GC_INTERNAL_BASIS */

void callIfIsHHObjptr (GC_state s,
                       HHObjptrFunction f,
                       struct HHObjptrFunctionArgs* fArgs,
                       objptr* opp) {
  if (isObjptr(*opp) && HM_HH_objptrInHierarchicalHeap(s, *opp)) {
    f(s, fArgs, opp);
  }
}

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

void forwardHHObjptr (GC_state s,
                      struct HHObjptrFunctionArgs* args,
                      objptr* opp) {
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

  assert(HM_HH_objptrInHierarchicalHeap(s, *opp));

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

void traceForHHObjptr(GC_state s,
                      HHObjptrFunction f,
                      struct HHObjptrFunctionArgs* fArgs,
                      objptr* objectObjptr) {
  if (!isObjptr(*objectObjptr)) {
    /* nothing to do */
    return;
  }

  // construct ascendHook args
  struct TraceHHObjptrAscendHookArgs ascendHookArgs = {
    .f = f,
    .fArgs = fArgs
  };

  // trace
  dfsMarkByModeCustom(s,
                      objptrToPointer(*objectObjptr, s->heap->start),
                      MARK_MODE,
                      FALSE,
                      FALSE,
                      traceHHObjptrDescendHook,
                      NULL,
                      traceHHObjptrAscendHook,
                      &ascendHookArgs);
  dfsMarkByModeCustom(s,
                      objptrToPointer(*objectObjptr, s->heap->start),
                      UNMARK_MODE,
                      FALSE,
                      FALSE,
                      traceHHObjptrDescendHook,
                      NULL,
                      noopAscendHook,
                      NULL);

  // apply ascendHook to objectObjptr
  callIfIsHHObjptr (s, f, fArgs, objectObjptr);
}

bool traceHHObjptrDescendHook(GC_state s, void* rawArgs, objptr object) {
  // silence compiler about unused variables
  ((void)(rawArgs));

  /* run through FALSE cases */
  GC_header header;
  header = getHeader (objptrToPointer(object, s->heap->start));
  if (HM_HH_objptrInHierarchicalHeap(s, object) ||
      (GC_FORWARDED == header) ||
      (GC_HIERARCHICAL_HEAP_HEADER == header)) {
    return FALSE;
  }

  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
  GC_objectTypeTag tag;
  splitHeader(s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);
  if (STACK_TAG == tag) {
    return FALSE;
  }

  return TRUE;
}

void traceHHObjptrAscendHook(GC_state s, void* rawArgs, objptr* objectObjptr) {
  struct TraceHHObjptrAscendHookArgs* args =
      ((struct TraceHHObjptrAscendHookArgs*)(rawArgs));

  callIfIsHHObjptr (s, args->f, args->fArgs, objectObjptr);
}

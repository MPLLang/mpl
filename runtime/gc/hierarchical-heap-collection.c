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
 * Calls 'f' on 'p' if it is a objptr to a object in the Hierarchical Heap
 *
 * @param s The GC_state to use
 * @param f The function to call
 * @param destinationLevelList The level list to pass to 'f'
 * @param hh The struct HM_HierarchicalHeap to pass to 'f'
 * @param minLevel The minLevel to pass to 'f'
 * @param maxLevel The maxLevel to pass to 'f'
 * @param opp The objptr* to pass to 'f'
 */
static void callIfIsHHObjptr (GC_state s,
                              HHObjptrFunction f,
                              void** destinationLevelList,
                              struct HM_HierarchicalHeap* hh,
                              size_t minLevel,
                              size_t maxLevel,
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
 * @param destinationLevelList The levelList to forward to
 * @param hh The hierarchical heap I am forwarding for
 * @param minLevel The minimum level to copy from, inclusive
 * @param maxLevel The maximum level to copy to, inclusive
 * @param opp The objptr to forward
 */
void forwardHHObjptr (GC_state s,
                      void** destinationLevelList,
                      struct HM_HierarchicalHeap* hh,
                      size_t minLevel,
                      size_t maxLevel,
                      objptr* opp);

/**
 * Forwards the object pointers in the current hierarchical heap in the given
 * stack to the given level
 *
 * @param destinationLevelList The destination level list to place new chunks
 * containing the copied objects
 * @param s The GC_state to use
 * @param hh The HierarchicalHeap to copy objects for
 * @param minLevel The minimum level to copy from, inclusive
 * @param stack The stack to copy from
 */
static void forwardHHObjptrsInStack(GC_state s,
                                    void** destinationLevelList,
                                    struct HM_HierarchicalHeap* hh,
                                    size_t minLevel,
                                    pointer stack);

/**
 * Forwards the object pointers in the current hierarchical heap in the level
 * list.
 *
 * @param destinationLevelList The level list to forward
 * @param hh The HierarchicalHeap to copy objects for
 * @param minLevel The minimum level to copy from, inclusive
 */
static void forwardHHObjptrsInLevelList(GC_state s,
                                        void** destinationLevelList,
                                        struct HM_HierarchicalHeap* hh,
                                        size_t minLevel);

/************************/
/* Function Definitions */
/************************/
#if (defined (MLTON_GC_INTERNAL_BASIS))
void HM_HHC_registerQueue(pointer queuePointer) { }
#endif /* MLTON_GC_INTERNAL_BASIS */

#if (defined (MLTON_GC_INTERNAL_BASIS))
void HM_HHC_collectLocal(void) {
  GC_state s = pthread_getspecific (gcstate_key);
  struct HM_HierarchicalHeap* hh = HM_HH_getCurrent(s);

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

  /* copy to newLevelList */
  void* newLevelList = NULL;
  forwardHHObjptrsInStack(s,
                          &newLevelList,
                          hh,
                          hh->level,
                          objptrToPointer(getStackCurrentObjptr(s),
                                          s->heap->start));

  forwardHHObjptrsInLevelList(s, &newLevelList, hh, hh->level);

  assertInvariants(s, hh);

  /* free old chunks */
  HM_freeChunks(&(hh->levelList), hh->level);

  /* merge newLevelList back in */
  HM_updateLevelListPointers(newLevelList, hh);
  HM_mergeLevelList(&(hh->levelList), newLevelList);

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

  assertInvariants(s, hh);

  HM_debugMessage(s,
                  "[%d] HM_HH_collectLocal(): Finished Local collection on "
                  "HierarchicalHeap = %p\n",
                  processor,
                  ((void*)(hh)));
}

void* HM_HHC_foreachHHObjptrInObject(GC_state s,
                                     pointer p,
                                     HHObjptrFunction f,
                                     void** destinationLevelList,
                                     struct HM_HierarchicalHeap* hh,
                                     size_t minLevel,
                                     size_t maxLevel) {
  GC_header header;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
  GC_objectTypeTag tag;

  assertInvariants(s, hh);

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
      if (DEBUG_DETAILED)
        fprintf (stderr,
                 "  p = "FMTPTR"  *p = "FMTOBJPTR"\n",
                 (uintptr_t)p, *(objptr*)p);
      callIfIsHHObjptr (s,
                        f,
                        destinationLevelList,
                        hh,
                        minLevel,
                        maxLevel,
                        (objptr*)p);
    }
  } else if (WEAK_TAG == tag) {
#pragma message "Implement when I can"
#if 0
    p += bytesNonObjptrs;
    if (1 == numObjptrs) {
      if (not skipWeaks)
        callIfIsHHObjptr (s,
                          f,
                          destinationLevelList,
                          hh,
                          minLevel,
                          maxLevel,
                          (objptr*)p);
      p += OBJPTR_SIZE;
    }
#else
    die(__FILE__ ":%d: "
        "foreachHHObjptrInObject() does not support Weak objects!",
        __LINE__);
#endif
  } else if (ARRAY_TAG == tag) {
#pragma message "Implement when I can"
#if 0
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
        for ( ; p < last; p += OBJPTR_SIZE)
          callIfIsHHObjptr (s,
                            f,
                            destinationLevelList,
                            hh,
                            minLevel,
                            maxLevel,
                            (objptr*)p);
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
          for ( ; p < next; p += OBJPTR_SIZE)
            callIfIsHHObjptr (s,
                              f,
                              destinationLevelList,
                              hh,
                              minLevel,
                              maxLevel,
                              (objptr*)p);
        }
      }
      assert (p == last);
      p -= dataBytes;
    }
    p += alignWithExtra (s, dataBytes, GC_ARRAY_HEADER_SIZE);
#else
    die(__FILE__ ":%d: "
        "foreachHHObjptrInObject() does not support Array objects!",
        __LINE__);
#endif
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
        if (DEBUG)
          fprintf(stderr, "  offset %"PRIx16"  address "FMTOBJPTR"\n",
                  frameOffsets[i + 1], *(objptr*)(top + frameOffsets[i + 1]));
        callIfIsHHObjptr (s,
                          f,
                          destinationLevelList,
                          hh,
                          minLevel,
                          maxLevel,
                          (objptr*)(top + frameOffsets[i + 1]));
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
                       void** destinationLevelList,
                       struct HM_HierarchicalHeap* hh,
                       size_t minLevel,
                       size_t maxLevel,
                       objptr* opp) {
  if (isObjptr(*opp) && HM_HH_objptrInHierarchicalHeap(s, *opp)) {
    f (s, destinationLevelList, hh, minLevel, maxLevel, opp);
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
                      void** destinationLevelList,
                      struct HM_HierarchicalHeap* hh,
                      size_t minLevel,
                      size_t maxLevel,
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
  if (DEBUG_DETAILED && (GC_FORWARDED == header)) {
    fprintf (stderr, "  already FORWARDED\n");

    /* if forwarded, must be in my own HierarchicalHeap! */
    assert(HM_HH_getContaining(s, op) == hh);

    /* should not have forwarded anything below 'minLevel'! */
#pragma message "Enable when ready"
#if 0
    assert(HM_HH_getLevel(s, op) >= minLevel);
#endif
  }

  if (GC_FORWARDED != header) {
    Word32 level = HM_HH_getObjptrLevel(s, op);
#pragma message "More nuanced with non-local collection"
    if ((HM_HH_getContaining(s, op) != hh) ||
        /* cannot forward any object below 'minLevel' */
        (level < minLevel)) {
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
    pointer copyPointer = copyObject(destinationLevelList,
                                     p - headerBytes,
                                     size,
                                     (level > maxLevel) ? (maxLevel) : (level));

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

  /* destinationLevelList has containingHH set to NULL */
  assert (NULL == HM_HH_getContaining(s, *opp));
}

void forwardHHObjptrsInStack(GC_state s,
                             void** destinationLevelList,
                             struct HM_HierarchicalHeap* hh,
                             size_t minLevel,
                             pointer stack) {
  HM_HHC_foreachHHObjptrInObject(s,
                                 stack,
                                 forwardHHObjptr,
                                 destinationLevelList,
                                 hh,
                                 minLevel,
                                 hh->level);
}

void forwardHHObjptrsInLevelList(GC_state s,
                                 void** destinationLevelList,
                                 struct HM_HierarchicalHeap* hh,
                                 size_t minLevel) {
  HM_foreachHHObjptrInLevelList(s,
                                destinationLevelList,
                                forwardHHObjptr,
                                hh,
                                minLevel);
}

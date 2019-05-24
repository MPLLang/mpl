/* Copyright (C) 2011-2012,2014,2016 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* ---------------------------------------------------------------- */
/*                          Initialization                          */
/* ---------------------------------------------------------------- */

size_t sizeofInitialBytesLive (GC_state s) {
  uint32_t i;
  size_t dataBytes;
  size_t total;

  total = 0;
  for (i = 0; i < s->vectorInitsLength; ++i) {
    dataBytes =
      s->vectorInits[i].elementSize
      * s->vectorInits[i].length;
    total += align(GC_ARRAY_METADATA_SIZE + dataBytes, s->alignment);
  }
  return total;
}

void initVectors(GC_state s, struct HM_HierarchicalHeap *hh) {
  struct GC_vectorInit *inits;
  HM_chunk currentChunk;
  pointer frontier;
  pointer limit;
  uint32_t i;

  assert(isFrontierAligned(s, s->frontier));
  inits = s->vectorInits;
  frontier = s->frontier;
  limit = s->limitPlusSlop;

  currentChunk = HM_getChunkOf(frontier);
  assert(currentChunk == HM_getChunkListLastChunk(HM_HH_LEVEL(hh, 0)));
  assert(HM_HH_getLevel(s, hh) == 0);

  for (i = 0; i < s->vectorInitsLength; i++) {
    size_t elementSize;
    size_t dataBytes;
    size_t objectSize;
    uint32_t typeIndex;

    elementSize = inits[i].elementSize;
    dataBytes = elementSize * inits[i].length;
    objectSize = align(GC_ARRAY_METADATA_SIZE + dataBytes, s->alignment);

#if ASSERT
    assert(limit == HM_getChunkLimit(currentChunk));
    assert(frontier >= HM_getChunkFrontier(currentChunk));
    assert(frontier <= limit);
#endif

    /* Extend with a new chunk, if there is not enough free space or if we have
     * crossed a block boundary. */
    if (limit - frontier < objectSize ||
        !inFirstBlockOfChunk(currentChunk, frontier)) {
      HM_HH_updateValues(hh, frontier);
      if (!HM_HH_extend(hh, objectSize)) {
        DIE("Ran out of space for Hierarchical Heap!");
      }
      s->frontier = HM_HH_getFrontier(hh);
      s->limitPlusSlop = HM_HH_getLimit(hh);
      s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;

      frontier = s->frontier;
      limit = s->limitPlusSlop;

      currentChunk = HM_getChunkOf(frontier);
      assert(currentChunk == HM_getChunkListLastChunk(HM_HH_LEVEL(hh, 0)));
    }

    assert(isFrontierAligned(s, frontier));
    assert(limit - frontier >= objectSize);
    assert(inFirstBlockOfChunk(currentChunk, frontier));

    *((GC_arrayCounter*)(frontier)) = 0;
    frontier = frontier + GC_ARRAY_COUNTER_SIZE;
    *((GC_arrayLength*)(frontier)) = inits[i].length;
    frontier = frontier + GC_ARRAY_LENGTH_SIZE;
    switch (elementSize) {
    case 1:
      typeIndex = WORD8_VECTOR_TYPE_INDEX;
      break;
    case 2:
      typeIndex = WORD16_VECTOR_TYPE_INDEX;
      break;
    case 4:
      typeIndex = WORD32_VECTOR_TYPE_INDEX;
      break;
    case 8:
      typeIndex = WORD64_VECTOR_TYPE_INDEX;
      break;
    default:
      die ("unknown element size in vectorInit: %"PRIuMAX"",
           (uintmax_t)elementSize);
    }
    *((GC_header*)(frontier)) = buildHeaderFromTypeIndex (typeIndex);
    frontier = frontier + GC_HEADER_SIZE;
    // *((objptr*)(frontier)) = BOGUS_OBJPTR;
    // frontier = frontier + OBJPTR_SIZE;
    s->globals[inits[i].globalIndex] = pointerToObjptr(frontier, NULL);
    if (DEBUG_DETAILED)
      fprintf (stderr, "allocated vector at "FMTPTR"\n",
               (uintptr_t)(s->globals[inits[i].globalIndex]));
    memcpy (frontier, inits[i].words, dataBytes);
    frontier += objectSize - GC_ARRAY_METADATA_SIZE;
  }

  s->frontier = frontier;

  /* If the last allocation passed a block boundary, we need to extend to have
   * a valid frontier. Extending with GC_HEAP_LIMIT_SLOP is arbitrary. */
  if (!inFirstBlockOfChunk(currentChunk, frontier)) {
    HM_HH_updateValues(hh, frontier);
    if (!HM_HH_extend(hh, GC_HEAP_LIMIT_SLOP)) {
      DIE("Ran out of space for Hierarchical Heap!");
    }
    s->frontier = HM_HH_getFrontier(hh);
    s->limitPlusSlop = HM_HH_getLimit(hh);
    s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;

    frontier = s->frontier;
    limit = s->limitPlusSlop;

    currentChunk = HM_getChunkOf(frontier);
    assert(currentChunk == HM_getChunkListLastChunk(HM_HH_LEVEL(hh, 0)));
  }

  assert(isFrontierAligned(s, s->frontier));
  assert(inFirstBlockOfChunk(currentChunk, s->frontier));
}

GC_thread initThreadAndHeap(GC_state s, Word32 level) {
  size_t stackSize = sizeofStackWithMetaData(s, sizeofStackInitialReserved(s));
  size_t threadSize = sizeofThread(s);
  size_t totalSize = stackSize + threadSize;

  struct HM_HierarchicalHeap *hh = HM_HH_new(s);
  hh->level = level;
  assert(HM_HH_getLevel(s, hh) == level);
  HM_HH_extend(hh, totalSize);

  pointer frontier = HM_HH_getFrontier(hh);

  assert(HM_HH_getLimit(hh) - frontier >= totalSize);
  assert(inFirstBlockOfChunk(HM_getChunkOf(frontier), frontier+totalSize));

  *((GC_header*)(frontier)) = GC_STACK_HEADER;
  GC_stack stack = (GC_stack)(frontier + GC_HEADER_SIZE);

  *((GC_header*)(frontier + stackSize)) = GC_THREAD_HEADER;
  GC_thread thread = (GC_thread)(frontier + stackSize + GC_HEADER_SIZE + offsetofThread(s));

  stack->reserved = sizeofStackInitialReserved(s);
  stack->used = 0;
  thread->inGlobalHeapCounter = 0;
  thread->currentProcNum = -1;
  thread->bytesNeeded = 0;
  thread->exnStack = BOGUS_EXN_STACK;
  thread->stack = pointerToObjptr((pointer)stack, NULL);
  thread->hierarchicalHeap = hh;

  s->frontier = frontier + totalSize;
  s->limitPlusSlop = HM_HH_getLimit(hh);
  s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;

#if ASSERT
  HM_chunk current = HM_getChunkOf(s->frontier);
  assert(current == HM_getChunkListLastChunk(HM_HH_LEVEL(hh, level)));
  assert(inFirstBlockOfChunk(current, s->frontier));
  assert(s->frontier >= HM_getChunkFrontier(current));
  assert(s->limitPlusSlop == HM_getChunkLimit(current));
  assert(s->limit == s->limitPlusSlop - GC_HEAP_LIMIT_SLOP);
#endif

  switchToThread(s, pointerToObjptr((pointer)thread - offsetofThread(s), NULL));

  return thread;
}

void initWorld(GC_state s) {
  for (int i = 0; i < s->globalsLength; ++i)
    s->globals[i] = BOGUS_OBJPTR;

  GC_thread thread = initThreadAndHeap(s, 0);
  struct HM_HierarchicalHeap *hh = thread->hierarchicalHeap;

  HM_allocateChunk(s->globalHeap, GC_HEAP_LIMIT_SLOP);

  /* Copy vectors into the heap, implicitly updating
   * s->{frontier,limit,limitPlusSlop} */
  initVectors(s, hh);

  HM_HH_maybeResizeLCHS(s, hh);

#if ASSERT
  HM_chunk current = HM_getChunkOf(s->frontier);
  assert(current == HM_getChunkListLastChunk(HM_HH_LEVEL(hh, 0)));
  assert(inFirstBlockOfChunk(current, s->frontier));
  assert(s->frontier >= HM_getChunkFrontier(current));
  assert(s->limitPlusSlop == HM_getChunkLimit(current));
  assert(s->limit == s->limitPlusSlop - GC_HEAP_LIMIT_SLOP);
#endif

  /* SAM_NOTE: some of these statistics may be maintained incorrectly
   * elsewhere in the runtime. */
  // GC_profileAllocInc(s, HM_getChunkListSize(s->globalHeap));
  s->cumulativeStatistics->bytesAllocated += HM_getChunkListSize(HM_HH_LEVEL(hh, 0));
  s->lastMajorStatistics->bytesLive = sizeofInitialBytesLive(s);

  // switchToThread(s, pointerToObjptr((pointer)thread - offsetofThread(s), NULL));
}

void duplicateWorld (GC_state d, GC_state s) {
  d->lastMajorStatistics->bytesLive = 0;

  GC_thread thread = initThreadAndHeap(d, 1);
  struct HM_HierarchicalHeap *hh = thread->hierarchicalHeap;
  HM_HH_maybeResizeLCHS(d, hh);

  HM_allocateChunk(d->globalHeap, GC_HEAP_LIMIT_SLOP);

  /* Now copy stats, heap data from original */
  d->cumulativeStatistics->maxHeapSize = s->cumulativeStatistics->maxHeapSize;
  d->heap = s->heap;
  d->secondaryHeap = s->secondaryHeap;
  d->generationalMaps = s->generationalMaps;

  // HM_chunk chunk = HM_allocateChunk(d->globalHeap, GC_HEAP_LIMIT_SLOP);
  // d->frontier = HM_getChunkFrontier(chunk);
  // d->limitPlusSlop = HM_getChunkLimit(chunk);
  // d->limit = d->limitPlusSlop - GC_HEAP_LIMIT_SLOP;

  // switchToThread (d, pointerToObjptr((pointer)thread - offsetofThread (d), NULL));
}

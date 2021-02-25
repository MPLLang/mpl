/* Copyright (C) 2020 Sam Westrick.
 * Copyright (C) 2011-2012,2014,2016,2020 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/* ---------------------------------------------------------------- */
/*                          Initialization                          */
/* ---------------------------------------------------------------- */

size_t sizeofInitialBytesLive (GC_state s) {
  size_t total;

  total = 0;
  total += s->staticHeaps.dynamic.size;
  total += sizeofStackWithMetaData (s, sizeofStackInitialReserved (s)) + sizeofThread (s);
  return total;
}

void initDynHeap(GC_state s, GC_thread thread) {
  assert(0 == thread->currentDepth);

  HM_chunk currentChunk;
  HM_chunkList currentChunkList;
  pointer frontier, limit;
  pointer start = s->staticHeaps.dynamic.start;
  pointer end = start + s->staticHeaps.dynamic.size;
  pointer p = start;
  size_t metaDataSize = 0, objectSize = 0;

  // While there are segments of the initial dynamic heap to be copied
  // into the root hierarchical heap.
  while (1) {
    currentChunk = thread->currentChunk;
    currentChunkList = HM_HH_getChunkList(HM_getLevelHead(currentChunk));
    frontier = HM_getChunkFrontier(currentChunk);
    assert(isFrontierAligned(s, frontier));
    limit = HM_getChunkLimit(currentChunk);
    assert(frontier <= limit);

    // Find the end of this segement of the initial dynamic heap to
    // copy into the current chunk of the root hierarchical heap.
    // `start` is the start of the segment.
    // `p` is the candidate end of segment.
    while (1) {
      if (p >= end) {
        // This segment is the last to be copied.
        break;
      }
      pointer q = advanceToObjectData (s, p);
#if ASSERT
      GC_header header = getHeader (q);
      assert (header == GC_REAL32_VECTOR_HEADER
              || header == GC_REAL64_VECTOR_HEADER
              || header == GC_WORD8_VECTOR_HEADER
              || header == GC_WORD16_VECTOR_HEADER
              || header == GC_WORD32_VECTOR_HEADER
              || header == GC_WORD64_VECTOR_HEADER);
#endif
      sizeofObjectAux (s, q, &metaDataSize, &objectSize);
      pointer r = q + objectSize;
      if (!inFirstBlockOfChunk(currentChunk, frontier + (q - start))
          || frontier + (r - start) > limit) {
        // Next object does not fit into current chunk.
        break;
      }
      // Next object fits into current chunk; advance `p`.
      p = r;
    }

    // Copy segment `[start,p)` into current segment.
    memcpy (frontier, start, p - start);
    // Adjust global objptrs that referenced an object in the segment.
    for (uint32_t i = 0; i < s->globalsLength; i++) {
      pointer g = objptrToPointer(s->globals[i], NULL);
      if (start <= g && g < p) {
        g = (g - start) + frontier;
        s->globals[i] = pointerToObjptr(g, NULL);
      }
    }
    // Advance frontier.
    frontier += p - start;
    HM_updateChunkFrontierInList(currentChunkList, currentChunk, frontier);

    if (p >= end) {
      // This segment was the last to be copied.
      break;
    }

    // Initialize search for next segment.
    start = p;
    // `p` points to the beginning of an object that did not fit in
    // the last chunk; extend hierarchical heap with a chunk
    // sufficient to hold the next object.
    if (!HM_HH_extend(s, thread, metaDataSize + objectSize)) {
      DIE("Ran out of space for Hierarchical Heap!");
    }
  }

  /* If the last allocation passed a block boundary, we need to extend to have
   * a valid frontier. Extending with GC_HEAP_LIMIT_SLOP is arbitrary. */
  if (!inFirstBlockOfChunk(currentChunk, frontier + GC_SEQUENCE_METADATA_SIZE)) {
    if (!HM_HH_extend(s, thread, GC_HEAP_LIMIT_SLOP)) {
      DIE("Ran out of space for Hierarchical Heap!");
    }
    currentChunk = thread->currentChunk;
    frontier = HM_getChunkFrontier(currentChunk);
    assert(isFrontierAligned(s, frontier));
    limit = HM_getChunkLimit(currentChunk);
    assert(frontier <= limit);
  }

  s->frontier = frontier;
  s->limitPlusSlop = limit;
  s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;

  assert(isFrontierAligned(s, s->frontier));
  assert(inFirstBlockOfChunk(currentChunk, s->frontier + GC_SEQUENCE_METADATA_SIZE));
}

GC_thread initThreadAndHeap(GC_state s, uint32_t depth) {
  GC_thread thread = newThreadWithHeap(s, sizeofStackInitialReserved(s), depth);

  s->frontier = HM_HH_getFrontier(thread);
  s->limitPlusSlop = HM_HH_getLimit(thread);
  s->limit = s->limitPlusSlop - GC_HEAP_LIMIT_SLOP;

#if ASSERT
  HM_chunk current = HM_getChunkOf(s->frontier);
  assert(HM_HH_getDepth(thread->hierarchicalHeap) == depth);
  assert(current == thread->currentChunk);
  assert(current->mightContainMultipleObjects);
  assert(inFirstBlockOfChunk(current, s->frontier));
  assert(s->frontier >= HM_getChunkFrontier(current));
  assert(s->limitPlusSlop == HM_getChunkLimit(current));
  assert(s->limit == s->limitPlusSlop - GC_HEAP_LIMIT_SLOP);
#endif

  switchToThread(s, pointerToObjptr((pointer)thread - offsetofThread(s), NULL));

  return thread;
}


void initWorld(GC_state s) {
  GC_thread thread = initThreadAndHeap(s, 0);
  struct HM_HierarchicalHeap *hh = thread->hierarchicalHeap;

  /* Copy initial dynamic heap, implicitly updating
   * s->{frontier,limit,limitPlusSlop} */
  initDynHeap(s, thread);

  size_t currentSize = HM_getChunkListSize(HM_HH_getChunkList(hh));

  /* SAM_NOTE: some of these statistics may be maintained incorrectly
   * elsewhere in the runtime. */
  s->cumulativeStatistics->bytesAllocated += currentSize;
  s->lastMajorStatistics->bytesLive = sizeofInitialBytesLive(s);

#if ASSERT
  HM_chunk current = HM_getChunkOf(s->frontier);
  assert(HM_HH_getDepth(hh) == 0);
  assert(current == thread->currentChunk);
  assert(current->mightContainMultipleObjects);
  assert(inFirstBlockOfChunk(current, s->frontier));
  assert(s->frontier >= HM_getChunkFrontier(current));
  assert(s->limitPlusSlop == HM_getChunkLimit(current));
  assert(s->limit == s->limitPlusSlop - GC_HEAP_LIMIT_SLOP);
#endif
}

void duplicateWorld (GC_state d, GC_state s) {
  d->lastMajorStatistics->bytesLive = 0;

  initThreadAndHeap(d, 0);

  /* Now copy stats, heap data from original */
  d->cumulativeStatistics->maxHeapSize = s->cumulativeStatistics->maxHeapSize;
}

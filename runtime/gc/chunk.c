/* Copyright (C) 2018 Sam Westrick
 * Copyright (C) 2015 Ram Raghunathan.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#include "chunk.h"

/******************************/
/* Static Function Prototypes */
/******************************/

static void HM_assertChunkListInvariants(HM_chunkList chunkList);

/**
 * A function to pass to ChunkPool_iteratedFree() for batch freeing of chunks
 * from a level list
 *
 * @param arg a struct FreeLevelListIteratorArgs* cast to void*
 *
 * @return pointer to chunk if it exists, NULL otherwise.
 */
// void* HM_freeLevelListIterator(void* arg);

/************************/
/* Function Definitions */
/************************/
#if (defined (MLTON_GC_INTERNAL_FUNCS))

size_t HM_BLOCK_SIZE;
size_t HM_ALLOC_SIZE;

HM_chunk mmapNewChunk(size_t chunkWidth);
HM_chunk mmapNewChunk(size_t chunkWidth) {
  assert(isAligned(chunkWidth, HM_BLOCK_SIZE));
  size_t bs = HM_BLOCK_SIZE;
  pointer start = (pointer)GC_mmapAnon(NULL, chunkWidth + bs);
  if (MAP_FAILED == start) {
    return NULL;
  }
  start = (pointer)(uintptr_t)align((uintptr_t)start, bs);
  HM_chunk result = HM_initializeChunk(start, start + chunkWidth);

  LOG(LM_CHUNK, LL_INFO,
    "Mapped a new region of size %zu",
    chunkWidth + bs);

  return result;
}

void HM_configChunks(GC_state s) {
  assert(isAligned(s->controls->minChunkSize, GC_MODEL_MINALIGN));
  assert(s->controls->minChunkSize >= GC_HEAP_LIMIT_SLOP);
  assert(isAligned(s->controls->allocChunkSize, s->controls->minChunkSize));
  HM_BLOCK_SIZE = s->controls->minChunkSize;
  HM_ALLOC_SIZE = s->controls->allocChunkSize;

  HM_chunk firstChunk = mmapNewChunk(HM_BLOCK_SIZE * 16);
  HM_appendChunk(getFreeListExtraSmall(s), firstChunk);
}

static void HM_prependChunk(HM_chunkList list, HM_chunk chunk) {
  chunk->nextChunk = list->firstChunk;
  if (list->firstChunk != NULL) {
    list->firstChunk->prevChunk = chunk;
  }
  if (list->lastChunk == NULL) {
    list->lastChunk = chunk;
  }
  list->firstChunk = chunk;
  list->size += HM_getChunkSize(chunk);
}

void HM_appendChunk(HM_chunkList list, HM_chunk chunk) {
  chunk->prevChunk = list->lastChunk;
  if (list->lastChunk != NULL) {
    list->lastChunk->nextChunk = chunk;
  }
  if (list->firstChunk == NULL) {
    list->firstChunk = chunk;
  }
  list->lastChunk = chunk;
  list->size += HM_getChunkSize(chunk);
}


/* Set up and return a pointer to a new chunk between start and end. Note that
 * the returned pointer is equal to start, and thus each of
 * {start, end, end - start} must be aligned on the block size. */
HM_chunk HM_initializeChunk(pointer start, pointer end) {
  assert(start != NULL);
  assert(end != NULL);
  assert(isAligned((size_t)start, HM_BLOCK_SIZE));
  assert(isAligned((size_t)end, HM_BLOCK_SIZE));
  assert(start + HM_BLOCK_SIZE <= end);
  HM_chunk chunk = (HM_chunk)start;

  chunk->frontier = start + sizeof(struct HM_chunk);
  chunk->limit = end;
  chunk->nextChunk = NULL;
  chunk->prevChunk = NULL;
  chunk->nextAdjacent = NULL;
  chunk->prevAdjacent = NULL;
  chunk->levelHead = NULL;
  chunk->startGap = 0;
  chunk->mightContainMultipleObjects = TRUE;
  chunk->magic = CHUNK_MAGIC;

#if ASSERT
  /* clear out memory to quickly catch some memory safety errors */
  memset(chunk->frontier, 0xAE, (size_t)(chunk->limit - chunk->frontier));
#endif

  return chunk;
}

/* SAM_NOTE: Disabled for now because I disabled chunk coalescing, so the
 * function would be unused. But we could use this later if we re-enable
 * coalescing, perhaps with a different freelist architecture. */
#if 0
void HM_coalesceChunks(HM_chunk left, HM_chunk right) {
  assert(left->nextAdjacent == right);
  assert(right->prevAdjacent == left);
  assert(left->limit == (pointer)right);

  left->limit = right->limit;
  left->nextAdjacent = right->nextAdjacent;

  if (right->nextAdjacent != NULL) {
    right->nextAdjacent->prevAdjacent = left;
  }
}
#endif

static HM_chunk splitChunkAt(HM_chunkList list, HM_chunk chunk, pointer splitPoint) {
  assert(HM_getChunkStart(chunk) <= chunk->frontier);
  assert(chunk->frontier <= chunk->limit);
  assert(chunk->frontier <= splitPoint);
  assert(splitPoint + sizeof(struct HM_chunk) <= chunk->limit);
  assert(isAligned((uintptr_t)splitPoint, HM_BLOCK_SIZE));
  assert(isAligned((uintptr_t)(chunk->limit - splitPoint), HM_BLOCK_SIZE));

  pointer limit = chunk->limit;
  chunk->limit = splitPoint;
  HM_chunk result = HM_initializeChunk(splitPoint, limit);
  result->levelHead = chunk->levelHead;

  if (NULL == chunk->nextChunk) {
    assert(list->lastChunk == chunk);
    list->lastChunk = result;
  } else {
    chunk->nextChunk->prevChunk = result;
  }

  result->prevChunk = chunk;
  result->nextChunk = chunk->nextChunk;
  chunk->nextChunk = result;

  if (chunk->nextAdjacent != NULL) {
    chunk->nextAdjacent->prevAdjacent = result;
  }
  result->nextAdjacent = chunk->nextAdjacent;
  result->prevAdjacent = chunk;
  chunk->nextAdjacent = result;

  assert(chunk->nextChunk == result);
  assert(chunk->nextAdjacent == result);
  assert(result->prevChunk == chunk);
  assert(result->prevAdjacent == chunk);
  assert(chunk->limit == (pointer)result);
  if (result->nextAdjacent != NULL) {
    assert(result->limit == (pointer)result->nextAdjacent);
    assert(result->nextAdjacent->prevAdjacent == result);
  }

  return result;
}

HM_chunk HM_splitChunk(HM_chunkList list, HM_chunk chunk, size_t bytesRequested) {
  assert(HM_getChunkStart(chunk) <= chunk->frontier);
  assert(chunk->frontier <= chunk->limit);
  assert((size_t)(chunk->limit - chunk->frontier) >= bytesRequested);

  size_t totalSize = bytesRequested + sizeof(struct HM_chunk);
  totalSize = align(totalSize, HM_BLOCK_SIZE);

  pointer limit = chunk->limit;
  pointer splitPoint = limit - totalSize;

  if (splitPoint < chunk->frontier) {
    // not enough space to split this chunk
    return NULL;
  }

  return splitChunkAt(list, chunk, splitPoint);
}

static HM_chunk splitChunkFront(HM_chunkList list, HM_chunk chunk, size_t bytesRequested) {
  assert(HM_getChunkStart(chunk) <= chunk->frontier);
  assert(chunk->frontier <= chunk->limit);
  assert((size_t)(chunk->limit - chunk->frontier) >= bytesRequested);

  pointer splitPoint = (pointer)(uintptr_t)align((uintptr_t)(chunk->frontier + bytesRequested), HM_BLOCK_SIZE);
  assert(chunk->frontier <= splitPoint);
  assert(splitPoint <= chunk->limit);
  assert((size_t)(splitPoint - chunk->frontier) >= bytesRequested);
  assert(isAligned((uintptr_t)splitPoint, HM_BLOCK_SIZE));

  if (splitPoint + HM_BLOCK_SIZE > chunk->limit) {
    // not enough space to split this chunk
    return NULL;
  }

  return splitChunkAt(list, chunk, splitPoint);
}

static inline bool chunkHasBytesFree(HM_chunk chunk, size_t bytes) {
  return chunk != NULL && (size_t)(chunk->limit - HM_getChunkStart(chunk)) >= bytes;
}

/* SAM_NOTE: Disabled this for now, as well as chunk coalescing (see in
 * HM_getFreeChunk below). In the past, HM_getLevelHead returned a chunklist,
 * but now we identify level heads with heap records, and there is no
 * associated heap for free lists. Of course, reusing chunk lists for freelists
 * is already a hack, so maybe it would be better to have an alternative
 * freelist representation. */
/*
static inline bool chunkIsInList(HM_chunk chunk, HM_chunkList list) {
  assert(list != NULL);
  return chunk != NULL &&
         chunk->levelHead != NULL &&
         HM_getLevelHead(chunk) == list;
}
*/

HM_chunk HM_getFreeChunk(GC_state s, size_t bytesRequested) {
  HM_chunk chunk = getFreeListSmall(s)->firstChunk;

  // can increase this number to cycle through more chunks
  int remainingToCheck = 2;
  while (chunk != NULL && remainingToCheck > 0) {
    /* SAM_NOTE: Disabled for now, because chunkIsInList check has been
     * deprecated.
     *
     * Coalescing wasn't getting much benefit anyway. Later, if we really need
     * coalescing, a better approach would be to incrementally sort the freelist
     * and look for physically adjacent chunks. The pointers {next,prev}Adjacent
     * would still be necessary for this! So I'm leaving them in. */
#if 0
    if (s->controls->freeListCoalesce) {
      HM_unlinkChunk(chunk);
      if (chunkIsInList(chunk->prevAdjacent, s->freeListSmall)) {
        assert(chunk->prevAdjacent->nextAdjacent == chunk);
        HM_unlinkChunk(chunk->prevAdjacent);
        chunk = chunk->prevAdjacent;
        HM_coalesceChunks(chunk, chunk->nextAdjacent);
      }
      if (chunkIsInList(chunk->nextAdjacent, s->freeListSmall)) {
        HM_unlinkChunk(chunk->nextAdjacent);
        HM_coalesceChunks(chunk, chunk->nextAdjacent);
      }
      HM_prependChunk(s->freeListSmall, chunk);
    }
#endif
    // chunks in freeListSmall might have frontiers/gaps that haven't been reset
    chunk->startGap = 0;
    chunk->frontier = HM_getChunkStart(chunk);

    /* if this chunk is good, then we're done. */
    if (chunkHasBytesFree(chunk, bytesRequested)) {
      assert(chunk->frontier == HM_getChunkStart(chunk));
      chunk->mightContainMultipleObjects = TRUE;
      splitChunkFront(getFreeListSmall(s), chunk, bytesRequested);
      HM_unlinkChunk(getFreeListSmall(s), chunk);
      return chunk;
    }

    /* otherwise, rotate this chunk onto the end and keep searching. */
    HM_unlinkChunk(getFreeListSmall(s), chunk);
    HM_appendChunk(getFreeListSmall(s), chunk);
    remainingToCheck--;
    chunk = getFreeListSmall(s)->firstChunk;
  }

  chunk = getFreeListLarge(s)->firstChunk;
  /* chunks in freeListLarge should always have properly set frontiers */
  assert(chunk == NULL || chunk->frontier == HM_getChunkStart(chunk));

  /* if this chunk is good, we're done. */
  if (chunkHasBytesFree(chunk, bytesRequested)) {
    chunk->mightContainMultipleObjects = TRUE;
    splitChunkFront(getFreeListLarge(s), chunk, bytesRequested);
    HM_unlinkChunk(getFreeListLarge(s), chunk);
    return chunk;
  }

  /* otherwise, dump it into the freelist of small chunks and mmap a fresh
   * large chunk. */

  if (chunk != NULL) {
    HM_unlinkChunk(getFreeListLarge(s), chunk);
    HM_appendChunk(getFreeListSmall(s), chunk);
  }

  size_t bytesNeeded = align(bytesRequested + sizeof(struct HM_chunk), HM_BLOCK_SIZE);
  size_t allocSize = max(bytesNeeded, s->nextChunkAllocSize);
  chunk = mmapNewChunk(allocSize);
  if (NULL != chunk) {
    /* success; on next mmap, get even more. */
    if (s->nextChunkAllocSize < (SIZE_MAX / 2)) {
      s->nextChunkAllocSize *= 2;
    }
  } else {
    /* the mmap failed. try again where we only request exactly what we need,
     * and if this still fails, then we're really out of memory and need to
     * abort. */

    LOG(LM_ALLOCATION, LL_INFO,
        "mmap of size %zu failed; trying again for %zu bytes",
        allocSize,
        bytesNeeded);

    chunk = mmapNewChunk(bytesNeeded);
    if (NULL == chunk) {
      DIE("Out of memory. Unable to allocate new chunk of size %zu.", bytesNeeded);
    }
    /* also, on next mmap, don't try to allocate so much. */
    if (s->nextChunkAllocSize > 2 * s->controls->allocChunkSize) {
      s->nextChunkAllocSize /= 2;
    }
  }

  HM_prependChunk(getFreeListLarge(s), chunk);
  assert(chunk->frontier == HM_getChunkStart(chunk));
  assert(chunkHasBytesFree(chunk, bytesRequested));
  chunk->mightContainMultipleObjects = TRUE;
  splitChunkFront(getFreeListLarge(s), chunk, bytesRequested);
  HM_unlinkChunk(getFreeListLarge(s), chunk);
  return chunk;
}

HM_chunk HM_allocateChunk(HM_chunkList list, size_t bytesRequested) {
  GC_state s = pthread_getspecific(gcstate_key);
  HM_chunk chunk = HM_getFreeChunk(s, bytesRequested);

  if (NULL == chunk) {
    DIE("Out of memory. Unable to allocate chunk of size %zu.",
        bytesRequested);
    return NULL;
  }

  s->cumulativeStatistics->bytesAllocated += HM_getChunkSize(chunk);

  assert(chunk->frontier == HM_getChunkStart(chunk));
  assert(chunk->mightContainMultipleObjects);
  assert((size_t)(chunk->limit - chunk->frontier) >= bytesRequested);

  HM_appendChunk(list, chunk);

  return chunk;
}

HM_chunkList HM_newChunkList(void) {
  GC_state s = pthread_getspecific(gcstate_key);

  size_t bytesNeeded = sizeof(struct HM_chunkList);
  HM_chunk sourceChunk = HM_getChunkListLastChunk(getFreeListExtraSmall(s));
  if (NULL == sourceChunk ||
      (size_t)(sourceChunk->limit - sourceChunk->frontier) < bytesNeeded) {
    sourceChunk = HM_allocateChunk(getFreeListExtraSmall(s), bytesNeeded);
  }
  pointer frontier = HM_getChunkFrontier(sourceChunk);
  HM_updateChunkValues(sourceChunk, frontier+bytesNeeded);
  HM_chunkList list = (HM_chunkList)frontier;

  HM_initChunkList(list);
  return list;
}

void HM_initChunkList(HM_chunkList list) {
  list->firstChunk = NULL;
  list->lastChunk = NULL;
  list->size = 0;
}

void HM_unlinkChunk(HM_chunkList list, HM_chunk chunk) {

  if (NULL == chunk->prevChunk) {
    assert(list->firstChunk == chunk);
    list->firstChunk = chunk->nextChunk;
  } else {
    assert(list->firstChunk != chunk);
    chunk->prevChunk->nextChunk = chunk->nextChunk;
  }

  if (NULL == chunk->nextChunk) {
    assert(list->lastChunk == chunk);
    list->lastChunk = chunk->prevChunk;
  } else {
    assert(list->lastChunk != chunk);
    chunk->nextChunk->prevChunk = chunk->prevChunk;
  }

  list->size -= HM_getChunkSize(chunk);

  chunk->levelHead = NULL;
  chunk->prevChunk = NULL;
  chunk->nextChunk = NULL;

#if ASSERT
  HM_assertChunkListInvariants(list);
#endif

}

void HM_forwardHHObjptrsInChunkList(
  GC_state s,
  HM_chunk chunk,
  pointer start,
  ObjptrPredicateFunction predicate,
  void* predicateArgs,
  ForeachObjptrFunction forwardHHObjptrFunc,
  struct ForwardHHObjptrArgs* forwardHHObjptrArgs)
{
  assert(NULL != chunk);
  assert(HM_getChunkStart(chunk) <= start);
  assert(start <= HM_getChunkFrontier(chunk));

  pointer p = start;
  size_t i = 0;

  while (NULL != chunk) {

    /* Can I use foreachObjptrInRange() for this? */
    while (p != chunk->frontier) {
      assert(p < chunk->frontier);
      p = advanceToObjectData(s, p);

      forwardHHObjptrArgs->containingObject = pointerToObjptr(p, NULL);
      p = foreachObjptrInObject(s,
                                p,
                                FALSE,
                                predicate,
                                predicateArgs,
                                forwardHHObjptrFunc,
                                forwardHHObjptrArgs);
      if ((i++ % 1024) == 0) {
        Trace3(EVENT_COPY,
               (EventInt)forwardHHObjptrArgs->bytesCopied,
               (EventInt)forwardHHObjptrArgs->objectsCopied,
               (EventInt)forwardHHObjptrArgs->stacksCopied);
      }
    }

    Trace3(EVENT_COPY,
           (EventInt)forwardHHObjptrArgs->bytesCopied,
           (EventInt)forwardHHObjptrArgs->objectsCopied,
           (EventInt)forwardHHObjptrArgs->stacksCopied);

    chunk = chunk->nextChunk;
    if (chunk != NULL) {
      p = HM_getChunkStart(chunk);
    }
  }

  forwardHHObjptrArgs->containingObject = BOGUS_OBJPTR;
}

pointer HM_getChunkFrontier(HM_chunk chunk) {
  return chunk->frontier;
}

pointer HM_getChunkLimit(HM_chunk chunk) {
  return chunk->limit;
}

size_t HM_getChunkSize(HM_chunk chunk) {
  return chunk->limit - (pointer)chunk;
}

pointer HM_getChunkStart(HM_chunk chunk) {
  return (pointer)chunk + sizeof(struct HM_chunk) + chunk->startGap;
}

pointer HM_shiftChunkStart(HM_chunk chunk, size_t bytes) {
  pointer oldStart = HM_getChunkStart(chunk);

  /* if we've already committed to a particular chunk start (by allocating a
   * GC-traceable object and moving the frontier) then the gap cannot be
   * shifted. */
  if (HM_getChunkFrontier(chunk) != oldStart)
    return NULL;

  /* the gap must end on an 8-byte boundary */
  size_t bytesAligned8 = align(bytes, 8);

  /* startGaps have to be small! */
  if (bytesAligned8 > UINT8_MAX ||
      bytesAligned8 + chunk->startGap > UINT8_MAX)
    return NULL;

  chunk->startGap = chunk->startGap + (uint8_t)bytesAligned8;
  chunk->frontier = HM_getChunkStart(chunk);

  return oldStart;
}

HM_chunk HM_getChunkListLastChunk(HM_chunkList list) {
  if (NULL == list) {
    return NULL;
  }

  return list->lastChunk;
}

HM_chunk HM_getChunkListFirstChunk(HM_chunkList list) {
  if (NULL == list) {
    return NULL;
  }

  return list->firstChunk;
}

size_t HM_getChunkListSize(HM_chunkList list) {
  assert(list != NULL);
  return list->size;
}

HM_HierarchicalHeap HM_getLevelHead(HM_chunk chunk) {
  assert(chunk != NULL);
  assert(chunk->levelHead != NULL);
  HM_HierarchicalHeap cursor = chunk->levelHead;
  while (cursor->representative != NULL) {
    cursor = cursor->representative;
  }
  return cursor;
}

HM_HierarchicalHeap HM_getLevelHeadPathCompress(HM_chunk chunk) {
  HM_HierarchicalHeap levelHead = HM_getLevelHead(chunk);
  assert(levelHead != NULL);

  /* fast path */
  if (chunk->levelHead == levelHead) {
    return levelHead;
  }

  HM_HierarchicalHeap cursor = chunk->levelHead;
  chunk->levelHead = levelHead;

  while (cursor != levelHead) {
    HM_HierarchicalHeap representative = cursor->representative;
    cursor->representative = levelHead;
    cursor = representative;
  }

  return levelHead;
}

void HM_appendChunkList(HM_chunkList list1, HM_chunkList list2) {
  LOG(LM_CHUNK, LL_DEBUGMORE,
      "Appending %p into %p",
      ((void*)(list2)),
      ((void*)(list1)));

  assert(NULL != list1);

  if (NULL == list2) {
    /* nothing to append */
    return;
  }

  if (list1->lastChunk == NULL) {
    assert(list1->firstChunk == NULL);
    list1->firstChunk = list2->firstChunk;
  } else {
    assert(list1->lastChunk->nextChunk == NULL);
    list1->lastChunk->nextChunk = list2->firstChunk;
  }

  if (list2->firstChunk != NULL) {
    list2->firstChunk->prevChunk = list1->lastChunk;
    list1->lastChunk = list2->lastChunk;
  }

  list1->size += list2->size;

#if ASSERT
  list2->lastChunk = NULL;
#endif

  HM_assertChunkListInvariants(list1);
}

void HM_updateChunkValues(HM_chunk chunk, pointer frontier) {
  assert(chunk->frontier <= frontier && frontier <= chunk->limit);
  chunk->frontier = frontier;
}

#endif /* MLTON_GC_INTERNAL_FUNCS */

#if ASSERT
void HM_assertChunkListInvariants(HM_chunkList chunkList) {
  size_t size = 0;
  HM_chunk chunk = chunkList->firstChunk;
  while (NULL != chunk) {
    assert(HM_getChunkStart(chunk) <= chunk->frontier);
    assert(chunk->frontier <= chunk->limit);
    size += HM_getChunkSize(chunk);
    if (chunk->nextChunk == NULL) {
      break;
    }
    assert(chunk->nextChunk->prevChunk == chunk);
    chunk = chunk->nextChunk;
  }

  assert(chunkList->lastChunk == chunk);
}
#else
void HM_assertChunkListInvariants(HM_chunkList chunkList) {
  ((void)(chunkList));
}
#endif /* ASSERT */

uint32_t HM_getObjptrDepth(objptr op) {
  return HM_getLevelHead(HM_getChunkOf(objptrToPointer(op, NULL)))->depth;
}

uint32_t HM_getObjptrDepthPathCompress(objptr op) {
  return HM_getLevelHeadPathCompress(HM_getChunkOf(objptrToPointer(op, NULL)))->depth;
}

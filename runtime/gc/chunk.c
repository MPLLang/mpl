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
  assert(isAligned(s->controls->blockSize, GC_MODEL_MINALIGN));
  assert(s->controls->blockSize >= GC_HEAP_LIMIT_SLOP);
  assert(isAligned(s->controls->allocChunkSize, s->controls->blockSize));
  HM_BLOCK_SIZE = s->controls->blockSize;
  HM_ALLOC_SIZE = s->controls->allocChunkSize;
}

void HM_prependChunk(HM_chunkList list, HM_chunk chunk) {
  chunk->nextChunk = list->firstChunk;
  if (list->firstChunk != NULL) {
    list->firstChunk->prevChunk = chunk;
  }
  if (list->lastChunk == NULL) {
    list->lastChunk = chunk;
  }
  list->firstChunk = chunk;
  list->size += HM_getChunkSize(chunk);
  list->usedSize += HM_getChunkUsedSize(chunk);
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
  list->usedSize += HM_getChunkUsedSize(chunk);
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
  // chunk->nextAdjacent = NULL;
  // chunk->prevAdjacent = NULL;
  chunk->levelHead = NULL;
  chunk->startGap = 0;
  chunk->mightContainMultipleObjects = TRUE;
  chunk->tmpHeap = NULL;
  chunk->magic = CHUNK_MAGIC;

#if ASSERT
  /* clear out memory to quickly catch some memory safety errors */
  // memset(chunk->frontier, 0xAE, (size_t)(chunk->limit - chunk->frontier));
#endif

  return chunk;
}


HM_chunk HM_getFreeChunk(GC_state s, size_t bytesRequested) {
  size_t chunkWidth =
    align(bytesRequested + sizeof(struct HM_chunk), HM_BLOCK_SIZE);
  size_t numBlocks = chunkWidth / HM_BLOCK_SIZE;
  Blocks start = allocateBlocks(s, numBlocks);
  SuperBlock container = start->container;
  numBlocks = start->numBlocks;
  HM_chunk result =
    HM_initializeChunk((pointer)start, (pointer)start + chunkWidth);
  result->container = container;
  result->numBlocks = numBlocks;
  return result;
}

void HM_freeChunk(GC_state s, HM_chunk chunk) {
  size_t numBlocks = chunk->numBlocks;
  SuperBlock container = chunk->container;
  Blocks bs = (Blocks)chunk;
  bs->numBlocks = numBlocks;
  bs->container = container;
  freeBlocks(s, bs);
}

void HM_freeChunksInList(GC_state s, HM_chunkList list) {
  HM_chunk chunk = list->firstChunk;
  while (chunk != NULL) {
    HM_chunk next = chunk->nextChunk;
    HM_freeChunk(s, chunk);
    chunk = next;
  }
  HM_initChunkList(list);
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

void HM_initChunkList(HM_chunkList list) {
  list->firstChunk = NULL;
  list->lastChunk = NULL;
  list->size = 0;
  list->usedSize = 0;
}

void HM_unlinkChunk(HM_chunkList list, HM_chunk chunk) {

// #if ASSERT
//   HM_assertChunkListInvariants(list);
// #endif

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
  list->usedSize -= HM_getChunkUsedSize(chunk);

  chunk->levelHead = NULL;
  chunk->prevChunk = NULL;
  chunk->nextChunk = NULL;

// #if ASSERT
//   HM_assertChunkListInvariants(list);
// #endif

}

void HM_forwardHHObjptrsInChunkList(
  GC_state s,
  HM_chunk chunk,
  pointer start,

  GC_objptrPredicateFun predicate,
  void *predicateArgs,
  GC_foreachObjptrFun forwardHHObjptrFunc,
  struct ForwardHHObjptrArgs* forwardHHObjptrArgs)
{
  assert(NULL != chunk);
  assert(HM_getChunkStart(chunk) <= start);
  assert(start <= HM_getChunkFrontier(chunk));

  pointer p = start;
  size_t i = 0;

  struct GC_foreachObjptrClosure forwardHHObjptrClosure =
    {.fun = forwardHHObjptrFunc, .env = forwardHHObjptrArgs};
  struct GC_objptrPredicateClosure predicateClosure =
    {.fun = predicate, .env = predicateArgs};

  while (NULL != chunk) {

    /* Can I use foreachObjptrInRange() for this? */
    while (p != chunk->frontier) {
      assert(p < chunk->frontier);
      p = advanceToObjectData(s, p);

      forwardHHObjptrArgs->containingObject = pointerToObjptr(p, NULL);
      p = foreachObjptrInObject(s,
                                p,
                                &predicateClosure,
                                &forwardHHObjptrClosure,
                                FALSE);
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

void HM_foreachObjInChunk(GC_state s, HM_chunk chunk, HM_foreachObjClosure f) {
    pointer p = HM_getChunkStart(chunk);
    pointer frontier = HM_getChunkFrontier(chunk);
    while (p < frontier) {
      p = advanceToObjectData(s, p);
      f->fun(s, p, f->env);
      p += sizeofObjectNoMetaData(s, p);
    }
}

void HM_foreachObjInChunkList(GC_state s, HM_chunkList list,
                                        HM_foreachObjClosure f) {
  HM_chunk chunk = HM_getChunkListFirstChunk(list);
  while(chunk!=NULL) {
    HM_foreachObjInChunk(s, chunk, f);
    chunk = chunk->nextChunk;
  }
}

void checkObj(GC_state s, pointer p, void*args) {
  assert(!hasFwdPtr(p));
  (void)s;
  (void)p;
  (void)args;
}

void traverseEachObjInChunkList(GC_state s, HM_chunkList list) {
  #if ASSERT
  struct HM_foreachObjClosure checkObjClosure = {
    .fun  = checkObj,
    .env = NULL
  };
  HM_foreachObjInChunkList(s, list, &checkObjClosure);
  #endif

  (void)s;
  (void)list;
}

size_t HM_getChunkUsedSize(HM_chunk chunk) {
  return (size_t)chunk->frontier - (size_t)HM_getChunkStart(chunk);
}

size_t HM_getChunkSize(HM_chunk chunk) {
  return chunk->limit - (pointer)chunk;
}

size_t HM_getChunkSizePastFrontier(HM_chunk chunk) {
  assert(chunk->frontier <= chunk->limit);
  return (size_t)chunk->limit - (size_t)chunk->frontier;
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

pointer HM_getChunkStartGap(HM_chunk chunk) {
  if (0 == chunk->startGap) {
    return NULL;
  }
  return (pointer)chunk + sizeof(struct HM_chunk);
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

size_t HM_getChunkListUsedSize(HM_chunkList list) {
  assert(list != NULL);
  return list->usedSize;
}

HM_HierarchicalHeap HM_getLevelHead(HM_chunk chunk) {
  assert(chunk != NULL);
  assert(chunk->levelHead != NULL);
  HM_UnionFindNode cursor = chunk->levelHead;
  while (cursor->representative != NULL) {
    cursor = cursor->representative;
  }
  assert(NULL != cursor->payload);
  return cursor->payload;
}

HM_HierarchicalHeap HM_getLevelHeadPathCompress(HM_chunk chunk) {
  HM_HierarchicalHeap levelHead = HM_getLevelHead(chunk);
  assert(levelHead != NULL);
  HM_UnionFindNode topNode = HM_HH_getUFNode(levelHead);
  assert(topNode != NULL);

  /* fast path */
  if (chunk->levelHead == topNode) {
    return levelHead;
  }

  HM_UnionFindNode cursor = chunk->levelHead;
  chunk->levelHead = topNode;

  while (cursor != topNode) {
    assert(cursor != NULL);
    HM_UnionFindNode representative = cursor->representative;
    cursor->representative = topNode;

    // if (representative == NULL) {
    //   DIE("whoops HM_getLevelHeadPathCompress:\ncursor %p\ncursor->payload  %p\ntopNode %p\nlevelHead %p\nchunk %p\ndepth %zu\nthread %p",
    //     (void*)cursor,
    //     (void*)cursor->payload,
    //     (void*)topNode,
    //     (void*)levelHead,
    //     (void*)chunk,
    //     (size_t)HM_HH_getDepth(levelHead),
    //     (void*)getThreadCurrent((GC_state)pthread_getspecific(gcstate_key)));
    // }

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
  list1->usedSize += list2->usedSize;

#if ASSERT
  list2->lastChunk = NULL;
#endif

  HM_assertChunkListInvariants(list1);
}

void HM_updateChunkFrontierInList(
  HM_chunkList list,
  HM_chunk chunk,
  pointer frontier)
{
  pointer oldFrontier = chunk->frontier;
  assert(oldFrontier <= frontier && frontier <= chunk->limit);

  chunk->frontier = frontier;

  if (NULL != list) {
    list->usedSize += (size_t)frontier - (size_t)oldFrontier;
  }
}

void HM_updateChunkFrontier(HM_chunk chunk, pointer frontier) {
  HM_updateChunkFrontierInList(NULL, chunk, frontier);
}


#endif /* MLTON_GC_INTERNAL_FUNCS */

#if 0
//ASSERT
void HM_assertChunkListInvariants(HM_chunkList chunkList) {
  // return;
  size_t size = 0;
  size_t usedSize = 0;
  HM_chunk chunk = chunkList->firstChunk;
  while (NULL != chunk) {
    assert(HM_getChunkStart(chunk) <= chunk->frontier);
    assert(chunk->frontier <= chunk->limit);
    size += HM_getChunkSize(chunk);
    usedSize += HM_getChunkUsedSize(chunk);
    if (chunk->nextChunk == NULL) {
      break;
    }
    assert(chunk->nextChunk->prevChunk == chunk);
    chunk = chunk->nextChunk;
  }
  assert(chunkList->lastChunk == chunk);
  assert(chunkList->size == size);
  assert(chunkList->usedSize == usedSize);
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

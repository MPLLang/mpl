/* Copyright (C) 2018 Sam Westrick
 * Copyright (C) 2015 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#include "chunk.h"

/******************************/
/* Static Function Prototypes */
/******************************/

#if ASSERT
/**
 * This function asserts the chunk invariants
 *
 * @attention
 * If an assertion fails, this function aborts the program, as per the assert()
 * macro.
 *
 * @param chunk The chunk to assert invariants for.
 * @param hh The hierarchical heap 'chunk' belongs to.
 * @param levelHeadChunk The head chunk of the level 'chunk' belongs to
 */
static void HM_assertChunkInvariants(HM_chunk chunk,
                                     HM_chunkList levelHead);

static HM_chunkList getLevelHead(HM_chunk chunk);
#endif

/**
 * This function asserts the chunk list invariants
 *
 * @attention
 * If an assertion fails, this function aborts the program, as per the assert()
 * macro.
 *
 * @param chunkList The chunk list to assert invariants for.
 * @param hh The hierarchical heap the chunks in 'chunkList' belong to.
 */
static void HM_assertChunkListInvariants(HM_chunkList chunkList,
                                         const struct HM_HierarchicalHeap* hh);

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

void HM_configChunks(GC_state s) {
  assert(isAligned(s->controls->minChunkSize, GC_MODEL_MINALIGN));
  assert(s->controls->minChunkSize >= GC_HEAP_LIMIT_SLOP);
  assert(isAligned(s->controls->allocChunkSize, s->controls->minChunkSize));
  HM_BLOCK_SIZE = s->controls->minChunkSize;
  HM_ALLOC_SIZE = s->controls->allocChunkSize;
}

void HM_prependChunk(HM_chunkList levelHead, HM_chunk chunk) {
  assert(HM_isLevelHead(levelHead));
  assert(HM_isUnlinked(chunk));

  chunk->levelHead = levelHead;
  chunk->nextChunk = levelHead->firstChunk;
  if (levelHead->firstChunk != NULL) {
    levelHead->firstChunk->prevChunk = chunk;
  }
  if (levelHead->lastChunk == NULL) {
    levelHead->lastChunk = chunk;
  }
  levelHead->firstChunk = chunk;
  levelHead->size += HM_getChunkSize(chunk);
}

void HM_appendChunk(HM_chunkList levelHead, HM_chunk chunk) {
  assert(HM_isLevelHead(levelHead));
  assert(HM_isUnlinked(chunk));

  chunk->levelHead = levelHead;
  chunk->prevChunk = levelHead->lastChunk;
  if (levelHead->lastChunk != NULL) {
    levelHead->lastChunk->nextChunk = chunk;
  }
  if (levelHead->firstChunk == NULL) {
    levelHead->firstChunk = chunk;
  }
  levelHead->lastChunk = chunk;
  levelHead->size += HM_getChunkSize(chunk);
}


/* Set up and return a pointer to a new chunk between start and end. Note that
 * the returned pointer is equal to start, and thus each of
 * {start, end, end - start} must be aligned on the block size. */
HM_chunk HM_initializeChunk(pointer start, pointer end);
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
  chunk->mightContainMultipleObjects = TRUE;
  chunk->magic = CHUNK_MAGIC;

#if ASSERT
  /* clear out memory to quickly catch some memory safety errors */
  memset(chunk->frontier, 0xAE, (size_t)(chunk->limit - chunk->frontier));
#endif

  assert(HM_isUnlinked(chunk));
  return chunk;
}

void HM_coalesceChunks(HM_chunk left, HM_chunk right) {
  assert(left->nextAdjacent == right);
  assert(right->prevAdjacent == left);
  assert(left->limit == (pointer)right);
  assert(HM_isUnlinked(left));
  assert(HM_isUnlinked(right));

  left->limit = right->limit;
  left->nextAdjacent = right->nextAdjacent;

  if (right->nextAdjacent != NULL) {
    right->nextAdjacent->prevAdjacent = left;
  }

// #if ASSERT
//   memset((void*)right, 0xBF, sizeof(struct HM_chunk));
// #endif
}

HM_chunk splitChunkAt(HM_chunk chunk, pointer splitPoint);
HM_chunk splitChunkAt(HM_chunk chunk, pointer splitPoint) {
  assert(HM_getChunkStart(chunk) <= chunk->frontier);
  assert(chunk->frontier <= chunk->limit);
  assert(chunk->frontier <= splitPoint);
  assert(splitPoint + sizeof(struct HM_chunk) <= chunk->limit);
  assert(isAligned((uintptr_t)splitPoint, HM_BLOCK_SIZE));
  assert(isAligned((uintptr_t)(chunk->limit - splitPoint), HM_BLOCK_SIZE));

  HM_chunkList levelHead = HM_getLevelHeadPathCompress(chunk);

  pointer limit = chunk->limit;
  chunk->limit = splitPoint;
  HM_chunk result = HM_initializeChunk(splitPoint, limit);
  result->levelHead = levelHead;

  if (NULL == chunk->nextChunk) {
    assert(levelHead->lastChunk == chunk);
    levelHead->lastChunk = result;
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

// #if ASSERT
//   HM_assertChunkListInvariants(levelHead, getHierarchicalHeapCurrent(s));
// #endif

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

HM_chunk HM_splitChunk(HM_chunk chunk, size_t bytesRequested) {
  assert(HM_getChunkStart(chunk) <= chunk->frontier);
  assert(chunk->frontier <= chunk->limit);
  assert((size_t)(chunk->limit - chunk->frontier) >= bytesRequested);
  assert(!HM_isUnlinked(chunk));

  size_t totalSize = bytesRequested + sizeof(struct HM_chunk);
  totalSize = align(totalSize, HM_BLOCK_SIZE);

  pointer limit = chunk->limit;
  pointer splitPoint = limit - totalSize;

  if (splitPoint < chunk->frontier) {
    // not enough space to split this chunk
    return NULL;
  }

  return splitChunkAt(chunk, splitPoint);
}

HM_chunk HM_splitChunkFront(HM_chunk chunk, size_t bytesRequested);
HM_chunk HM_splitChunkFront(HM_chunk chunk, size_t bytesRequested) {
  assert(HM_getChunkStart(chunk) <= chunk->frontier);
  assert(chunk->frontier <= chunk->limit);
  assert((size_t)(chunk->limit - chunk->frontier) >= bytesRequested);
  assert(!HM_isUnlinked(chunk));

  pointer splitPoint = (pointer)(uintptr_t)align((uintptr_t)(chunk->frontier + bytesRequested), HM_BLOCK_SIZE);
  assert(chunk->frontier <= splitPoint);
  assert(splitPoint <= chunk->limit);
  assert((size_t)(splitPoint - chunk->frontier) >= bytesRequested);
  assert(isAligned((uintptr_t)splitPoint, HM_BLOCK_SIZE));

  if (splitPoint + HM_BLOCK_SIZE > chunk->limit) {
    // not enough space to split this chunk
    return NULL;
  }

  return splitChunkAt(chunk, splitPoint);
}

HM_chunk mmapNewChunk(size_t chunkWidth);
HM_chunk mmapNewChunk(size_t chunkWidth) {
  assert(isAligned(chunkWidth, HM_BLOCK_SIZE));
  size_t bs = HM_BLOCK_SIZE;
  pointer start = (pointer)GC_mmapAnon(NULL, chunkWidth + bs);
  if (MAP_FAILED == start) {
    DIE("Out of memory. Unable to allocate new chunk of size %zu.", chunkWidth);
    return NULL;
  }
  start = (pointer)(uintptr_t)align((uintptr_t)start, bs);
  HM_chunk result = HM_initializeChunk(start, start + chunkWidth);

  LOG(LM_CHUNK, LL_INFO,
    "Mapped a new region of size %zu",
    chunkWidth + bs);

  return result;
}

static inline bool chunkHasBytesFree(HM_chunk chunk, size_t bytes) {
  return chunk != NULL && (size_t)(chunk->limit - HM_getChunkStart(chunk)) >= bytes;
}

static inline bool chunkIsInList(HM_chunk chunk, HM_chunkList list) {
  assert(list != NULL);
  return chunk != NULL &&
         chunk->levelHead != NULL &&
         HM_getLevelHead(chunk) == list;
}

HM_chunk HM_getFreeChunk(GC_state s, size_t bytesRequested);
HM_chunk HM_getFreeChunk(GC_state s, size_t bytesRequested) {
  HM_chunk chunk = s->freeListSmall->firstChunk;

  // can increase this number to cycle through more chunks
  int remainingToCheck = 2;
  while (chunk != NULL && remainingToCheck > 0) {
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
    /* chunks in freeListSmall might have frontiers that haven't been reset */
    chunk->frontier = HM_getChunkStart(chunk);
    if (chunkHasBytesFree(chunk, bytesRequested)) goto finish;
    HM_unlinkChunk(chunk);
    HM_appendChunk(s->freeListSmall, chunk);
    remainingToCheck--;
    chunk = s->freeListSmall->firstChunk;
  }

  chunk = s->freeListLarge->firstChunk;
  /* chunks in freeListLarge should always have properly set frontiers */
  assert(chunk == NULL || chunk->frontier == HM_getChunkStart(chunk));
  if (chunkHasBytesFree(chunk, bytesRequested)) goto finish;

  if (chunk != NULL) {
    HM_unlinkChunk(chunk);
    HM_appendChunk(s->freeListSmall, chunk);
  }

  size_t bytesNeeded = align(bytesRequested + sizeof(struct HM_chunk), HM_BLOCK_SIZE);
  size_t allocSize = max(bytesNeeded, s->nextChunkAllocSize);
  s->nextChunkAllocSize = 2 * allocSize;
  chunk = mmapNewChunk(allocSize);
  HM_prependChunk(s->freeListLarge, chunk);

finish:
  // chunk->frontier = HM_getChunkStart(chunk);
  assert(chunk->frontier == HM_getChunkStart(chunk));
  chunk->mightContainMultipleObjects = TRUE;
  HM_splitChunkFront(chunk, bytesRequested);
  HM_unlinkChunk(chunk);
  return chunk;
}

HM_chunk HM_allocateChunk(HM_chunkList levelHead, size_t bytesRequested) {
  assert(HM_isLevelHead(levelHead));
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

  HM_appendChunk(levelHead, chunk);

  LOG(LM_CHUNK, LL_DEBUG,
      "Allocate chunk %p at level %u",
      (void*)chunk,
      levelHead->level);

  return chunk;
}

HM_chunkList HM_newChunkList(struct HM_HierarchicalHeap* hh, Word32 level) {

  // SAM_NOTE: replace with custom arena allocation if a performance bottleneck
  HM_chunkList list = (HM_chunkList) malloc(sizeof(struct HM_chunkList));

  if (list == NULL) {
    DIE("Out of memory. Unable to allocate new chunk list.");
    return NULL;
  }

  list->firstChunk = NULL;
  list->lastChunk = NULL;
  list->parent = NULL;
  list->rememberedSet = NULL;
  list->containingHH = hh;
  list->size = 0;
  list->isInToSpace = (hh == COPY_OBJECT_HH_VALUE);
  list->level = level;

  return list;
}

void HM_unlinkChunk(HM_chunk chunk) {
  HM_chunkList levelHead = HM_getLevelHeadPathCompress(chunk);

  if (NULL == chunk->prevChunk) {
    assert(levelHead->firstChunk == chunk);
    levelHead->firstChunk = chunk->nextChunk;
  } else {
    assert(levelHead->firstChunk != chunk);
    chunk->prevChunk->nextChunk = chunk->nextChunk;
  }

  if (NULL == chunk->nextChunk) {
    assert(levelHead->lastChunk == chunk);
    levelHead->lastChunk = chunk->prevChunk;
  } else {
    assert(levelHead->lastChunk != chunk);
    chunk->nextChunk->prevChunk = chunk->prevChunk;
  }

  levelHead->size -= HM_getChunkSize(chunk);

  chunk->levelHead = NULL;
  chunk->prevChunk = NULL;
  chunk->nextChunk = NULL;

#if ASSERT
  HM_assertChunkListInvariants(levelHead, levelHead->containingHH);
#endif

  assert(HM_isUnlinked(chunk));
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

Word64 HM_getChunkSize(HM_chunk chunk) {
  return chunk->limit - (pointer)chunk;
}

pointer HM_getChunkStart(HM_chunk chunk) {
  return (pointer)chunk + sizeof(struct HM_chunk);
}

Word32 HM_getChunkListLevel(HM_chunkList levelHead) {
  assert(HM_isLevelHead(levelHead));
  return levelHead->level;
}

HM_chunk HM_getChunkListLastChunk(HM_chunkList levelHead) {
  if (NULL == levelHead) {
    return NULL;
  }

  assert(HM_isLevelHead(levelHead));
  return levelHead->lastChunk;
}

HM_chunk HM_getChunkListFirstChunk(HM_chunkList levelHead) {
  if (NULL == levelHead) {
    return NULL;
  }

  assert(HM_isLevelHead(levelHead));
  return levelHead->firstChunk;
}

Word64 HM_getChunkListSize(HM_chunkList levelHead) {
  assert(levelHead != NULL);
  assert(HM_isLevelHead(levelHead));
  return levelHead->size;
}

HM_chunkList HM_getLevelHead(HM_chunk chunk) {
  assert(chunk != NULL);
  assert(chunk->levelHead != NULL);
  HM_chunkList cursor = chunk->levelHead;
  while (cursor->parent != NULL) {
    cursor = cursor->parent;
  }
  return cursor;
}

HM_chunkList HM_getLevelHeadPathCompress(HM_chunk chunk) {
  HM_chunkList levelHead = HM_getLevelHead(chunk);
  assert(levelHead != NULL);

  /* fast path */
  if (chunk->levelHead == levelHead) {
    return levelHead;
  }

  HM_chunkList cursor = chunk->levelHead;
  chunk->levelHead = levelHead;

  /* SAM_NOTE: TODO: free levelheads with reference counting */
  while (cursor != levelHead) {
    HM_chunkList parent = cursor->parent;
    cursor->parent = levelHead;
    cursor = parent;
  }

  return levelHead;
}

void HM_getObjptrInfo(__attribute__((unused)) GC_state s,
                      objptr object,
                      struct HM_ObjptrInfo* info)
{
  HM_chunk chunk = HM_getChunkOf(objptrToPointer(object, NULL));
  assert(NULL != chunk);

  HM_chunkList chunkList = HM_getLevelHeadPathCompress(chunk);

  assert(HM_isLevelHead(chunkList));
  info->hh = chunkList->containingHH;
  info->chunkList = chunkList;
  info->level = chunkList->level;
}

void HM_appendChunkList(HM_chunkList list1, HM_chunkList list2) {
  LOG(LM_CHUNK, LL_DEBUGMORE,
      "Appending %p into %p",
      ((void*)(list2)),
      ((void*)(list1)));

  assert(NULL != list1);
  assert(HM_isLevelHead(list1));

  if (NULL == list2) {
    /* nothing to append */
    return;
  }

  assert(HM_isLevelHead(list2));

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
  list2->parent = list1;

  if (list1->rememberedSet == NULL) {
    list1->rememberedSet = list2->rememberedSet;
  } else {
    // recursive call will not recurse
    HM_appendChunkList(list1->rememberedSet, list2->rememberedSet);
  }

#if ASSERT
  list2->rememberedSet = NULL;
  list2->lastChunk = NULL;
  list2->containingHH = NULL;
#endif

  HM_assertChunkListInvariants(list1, list1->containingHH);
}

#if ASSERT
// void HM_assertChunkInLevelList(HM_chunkList levelList, HM_chunk chunk) {
//   for (HM_chunkList chunkList = levelList;
//        NULL != chunkList;
//        chunkList = chunkList->nextHead) {
//     for (HM_chunk cursor = chunkList->firstChunk;
//          NULL != cursor;
//          cursor = cursor->nextChunk) {
//       if (chunk == cursor) {
//         /* found! */
//         return;
//       }
//     }
//   }

//   /* If I get here, I couldn't find the chunk */
//   ASSERTPRINT(FALSE,
//               "Could not find chunk %p!",
//               (void*)chunk);
// }

void HM_assertLevelListInvariants(const struct HM_HierarchicalHeap* hh,
                                  Word32 stealLevel,
                                  bool inToSpace) {
  ((void)(stealLevel));
  Word32 previousLevel = ~((Word32)(0));
  FOR_LEVEL_DECREASING_IN_RANGE(chunkList, i, hh, 0, HM_MAX_NUM_LEVELS, {
    assert(HM_isLevelHead(chunkList));

    Word32 level = chunkList->level;
    assert(level == i);

    struct HM_HierarchicalHeap* levelListHH = chunkList->containingHH;
    assert(levelListHH == hh);

    assert(chunkList->isInToSpace == inToSpace);

    assert(level < previousLevel);
    previousLevel = level;

    HM_assertChunkListInvariants(chunkList, levelListHH);
  });
}
#else
// void HM_assertChunkInLevelList(HM_chunkList levelList, HM_chunk chunk) {
//   ((void)(levelList));
//   ((void)(chunk));
// }

void HM_assertLevelListInvariants(const struct HM_HierarchicalHeap* hh,
                                  Word32 stealLevel,
                                  bool inToSpace) {
  ((void)(hh));
  ((void)(stealLevel));
  ((void)(inToSpace));
}
#endif /* ASSERT */

void HM_updateChunkValues(HM_chunk chunk, pointer frontier) {
  assert(chunk->frontier <= frontier && frontier <= chunk->limit);
  chunk->frontier = frontier;
}

#endif /* MLTON_GC_INTERNAL_FUNCS */

#if ASSERT
HM_chunkList getLevelHead(HM_chunk chunk) {
  HM_chunkList cursor = chunk->levelHead;
  assert(NULL != cursor);
  while (cursor->parent != NULL) {
    cursor = cursor->parent;
    assert(NULL != cursor);
  }

  assert(HM_isLevelHead(cursor));
  return cursor;
}
#endif

#if ASSERT
void HM_assertChunkInvariants(HM_chunk chunk,
                              HM_chunkList levelHead) {
  assert(HM_getChunkStart(chunk) <= chunk->frontier && chunk->frontier <= chunk->limit);
  assert(levelHead == getLevelHead(chunk));
}

void HM_assertChunkListInvariants(HM_chunkList chunkList,
                                  const struct HM_HierarchicalHeap* hh) {
  assert(HM_isLevelHead(chunkList));
  Word64 size = 0;
  HM_chunk chunk = chunkList->firstChunk;
  while (NULL != chunk) {
    HM_assertChunkInvariants(chunk, chunkList);
    size += HM_getChunkSize(chunk);
    if (chunk->nextChunk == NULL) {
      break;
    }
    assert(chunk->nextChunk->prevChunk == chunk);
    chunk = chunk->nextChunk;
  }

  if (chunkList->rememberedSet != NULL) {
    /* this call won't recurse again */
    HM_assertChunkListInvariants(chunkList->rememberedSet, NULL);
    assert(chunkList->size == size + chunkList->rememberedSet->size);
  } else {
    assert(chunkList->size == size);
  }

  assert(chunkList->containingHH == hh);
  assert(chunkList->lastChunk == chunk);
}
#else
void HM_assertChunkListInvariants(HM_chunkList chunkList,
                                  const struct HM_HierarchicalHeap* hh) {
  ((void)(chunkList));
  ((void)(hh));
}
#endif /* ASSERT */

struct HM_HierarchicalHeap *HM_getObjptrHH(GC_state s, objptr object) {
  struct HM_ObjptrInfo objInfo;
  HM_getObjptrInfo(s, object, &objInfo);
  return objInfo.hh;
}

Word32 HM_getObjptrLevel(objptr op) {
  return HM_getLevelHead(HM_getChunkOf(objptrToPointer(op, NULL)))->level;
}

bool HM_isObjptrInToSpace(__attribute__((unused)) GC_state s,
                          objptr object)
{
  /* SAM_NOTE: why is this commented out? why are there two ways to check if
   * an object is in the toSpace? Does promotion use one, while collection
   * uses the other? */
  /* return HM_getObjptrLevelHeadChunk(s, object)->split.levelHead.isInToSpace; */
  HM_chunk c = HM_getChunkOf(objptrToPointer(object, NULL));
  return HM_getLevelHeadPathCompress(c)->containingHH == COPY_OBJECT_HH_VALUE;
}


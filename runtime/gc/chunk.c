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
/**
 * This function appends 'chunkList' to 'destinationChunkList'.
 *
 * @note
 * 'chunkList's level head is demoted to a regular chunk and its level is
 * switched to 'destinationChunkList's
 *
 * @param destinationChunkList The head of the chunk list to append to. Must be
 * the full level chunk list.
 * @param chunkList the chunk list to append. Must be a full level chunk list,
 * <em>not</em> in a level list
 * @param sentinel The sentinel value to populate the chunkList with in ASSERT
 * builds.
 */
static void appendChunkList(HM_chunkList destinationChunkList,
                            HM_chunkList chunkList,
                            ARG_USED_FOR_ASSERT size_t sentinel);

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

static void HM_assertFreeListInvariants(HM_chunk freeList);

/**
 * A function to pass to ChunkPool_iteratedFree() for batch freeing of chunks
 * from a level list
 *
 * @param arg a struct FreeLevelListIteratorArgs* cast to void*
 *
 * @return pointer to chunk if it exists, NULL otherwise.
 */
// void* HM_freeLevelListIterator(void* arg);

#if ASSERT
void assertObjptrInHH(objptr op) {
  assert(HM_getChunkOf(objptrToPointer(op, NULL)));
}
#else
void assertObjptrInHH(objptr op) {
  ((void)op);
}
#endif

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
  levelHead->lastChunk = chunk;
  levelHead->size += HM_getChunkSize(chunk);
}


/* Set up and return a pointer to a new chunk between start and end. Note that
 * the returned pointer is equal to start, and thus each of
 * {start, end, end - start} must be aligned on the block size. */
HM_chunk HM_initializeChunk(pointer start, pointer end);
HM_chunk HM_initializeChunk(pointer start, pointer end) {
  assert(isAligned((size_t)start, HM_BLOCK_SIZE));
  assert(isAligned((size_t)end, HM_BLOCK_SIZE));
  assert(start + HM_BLOCK_SIZE <= end);
  HM_chunk chunk = (HM_chunk)start;

  chunk->frontier = start + sizeof(struct HM_chunk);
  chunk->limit = end;
  chunk->nextChunk = NULL;
  chunk->prevChunk = NULL;
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

/* Takes at least `bytesRequested` amount of space from victim *chunkp,
 * returning a pointer to the resulting chunk and updating *chunkp appropriately.
 * (chunkp is passed as argument to permit updating the owning freelist.)
 * The victim chunk must be a normal empty chunk in a freelist.
 *
 * If the victim is large enough, this will be accomplished by splitting it into
 * two chunks. Otherwise, this function will simply unlink the victim and return
 * it. */
HM_chunk HM_takeFromChunk(HM_chunk * chunkp, size_t bytesRequested);
HM_chunk HM_takeFromChunk(HM_chunk * chunkp, size_t bytesRequested) {
  assert(chunkp != NULL);
  HM_chunk chunk = *chunkp;
  assert(chunk != NULL);
  assert((size_t)(chunk->limit - chunk->frontier) >= bytesRequested);

  size_t totalSize = bytesRequested + sizeof(struct HM_chunk);
  totalSize = align(totalSize, HM_BLOCK_SIZE);

  pointer limit = chunk->limit;
  pointer requestedSplitPoint = limit - totalSize;
  if (requestedSplitPoint < chunk->frontier) {
    /* can't split; have to take the whole chunk. */
    *chunkp = chunk->nextChunk;
    if (NULL != chunk->nextChunk) {
      chunk->nextChunk->prevChunk = chunk->prevChunk;
    }
    chunk->nextChunk = NULL;
    chunk->prevChunk = (HM_chunk)(0xfeeb1efab1edbeef);
    return chunk;
  }

  /* This chunk is hogging too much space! Time to steal. */
  chunk->limit = requestedSplitPoint;
  return HM_initializeChunk(requestedSplitPoint, limit);
}

HM_chunk HM_splitChunk(HM_chunk chunk, size_t bytesRequested) {
  assert((size_t)(chunk->limit - chunk->frontier) >= bytesRequested);

  size_t totalSize = bytesRequested + sizeof(struct HM_chunk);
  totalSize = align(totalSize, HM_BLOCK_SIZE);

  pointer limit = chunk->limit;
  pointer splitPoint = limit - totalSize;
  assert(splitPoint >= chunk->frontier);

  HM_chunkList levelHead = HM_getLevelHeadPathCompress(chunk);

  chunk->limit = splitPoint;
  HM_chunk result = HM_initializeChunk(splitPoint, limit);

  result->nextChunk = chunk->nextChunk;
  result->prevChunk = chunk;
  chunk->nextChunk = result;
  if (NULL != result->nextChunk) {
    result->nextChunk->prevChunk = result;
  }

  result->levelHead = levelHead;
  if (chunk == levelHead->lastChunk) {
    levelHead->lastChunk = result;
  }

// #if ASSERT
//   HM_assertChunkListInvariants(levelHead, getHierarchicalHeapCurrent(s));
// #endif

  return result;
}

HM_chunk HM_getFreeChunk(struct HM_HierarchicalHeap* hh, size_t bytesRequested);
HM_chunk HM_getFreeChunk(struct HM_HierarchicalHeap* hh, size_t bytesRequested) {
  HM_chunk *chunkp = &(hh->freeList);
  HM_chunk chunk = *chunkp;
  int skipped = 0;
  while (NULL != chunk && (size_t)(chunk->limit - chunk->frontier) < bytesRequested) {
    chunkp = &(chunk->nextChunk);
    chunk = *chunkp;
    skipped++;
  }
  LOG(LM_CHUNK, LL_INFO,
    "Skipped over %d free chunks looking for %zu bytes",
    skipped,
    bytesRequested);

  if (NULL == chunk) {
    /* No sufficient free chunk was found, so need to allocate a new one. Also,
     * need to amortize future allocations by populating the free list. */
    size_t bs = HM_BLOCK_SIZE;
    size_t desiredSize = align(bytesRequested + HM_ALLOC_SIZE, bs);
    pointer start = (pointer)GC_mmapAnon(NULL, desiredSize + bs);
    if (NULL == start) {
      return NULL;
    }
    start = (pointer)(uintptr_t)align((size_t)start, bs);
    chunk = HM_initializeChunk(start, start + desiredSize);
    /* put it at the front of the list */
    chunkp = &(hh->freeList);
    chunk->nextChunk = *chunkp;
    if (NULL != chunk->nextChunk) {
      chunk->nextChunk->prevChunk = chunk;
    }
    *chunkp = chunk;
    LOG(LM_CHUNK, LL_INFO,
      "Mapped a new region of size %zu",
      desiredSize + bs);
  }

  chunk = HM_takeFromChunk(chunkp, bytesRequested);
  HM_assertFreeListInvariants(hh->freeList);
  return chunk;
}

HM_chunk HM_allocateChunk(HM_chunkList levelHead, size_t bytesRequested) {
  assert(HM_isLevelHead(levelHead));
  // SAM_NOTE: can't use hh here, because it might just be a sentinel value :( :( :( :(
  HM_chunk chunk = HM_getFreeChunk(getHierarchicalHeapCurrent(pthread_getspecific(gcstate_key)), bytesRequested);

  if (NULL == chunk) {
    return NULL;
  }

  /* insert into list and update levelHead */
  chunk->levelHead = levelHead;
  chunk->nextChunk = NULL;
  chunk->prevChunk = levelHead->lastChunk;
  if (NULL != levelHead->lastChunk) {
    levelHead->lastChunk->nextChunk = chunk;
  }
  levelHead->lastChunk = chunk;
  levelHead->size += HM_getChunkSize(chunk);

  LOG(LM_CHUNK, LL_DEBUG,
      "Allocate chunk %p at level %u",
      (void*)chunk,
      levelHead->level);

  return chunk;
}

HM_chunk HM_allocateLevelHeadChunk(HM_chunkList * levelList,
                                   size_t bytesRequested,
                                   Word32 level,
                                   struct HM_HierarchicalHeap* hh)
{
  // SAM_NOTE: can't use hh here, because it might just be a sentinel value :( :( :( :(
  HM_chunk chunk = HM_getFreeChunk(getHierarchicalHeapCurrent(pthread_getspecific(gcstate_key)), bytesRequested);

  if (NULL == chunk) {
    return NULL;
  }

  chunk->nextChunk = NULL;
  chunk->prevChunk = NULL;

  // SAM_NOTE: TODO: replace with arena allocation if this becomes a
  // performance bottleneck
  HM_chunkList levelHead = (HM_chunkList) malloc(sizeof(struct HM_chunkList));

  chunk->levelHead = levelHead;
  levelHead->firstChunk = chunk;
  levelHead->lastChunk = chunk;
  levelHead->parent = levelHead;
  levelHead->nextHead = NULL;
  levelHead->containingHH = hh;
  levelHead->toChunkList = NULL;
  levelHead->size = (size_t)(chunk->limit - (pointer)chunk);
  levelHead->isInToSpace = (hh == COPY_OBJECT_HH_VALUE);
  levelHead->level = level;

  /* insert into level list */
  HM_mergeLevelList(levelList,
                    levelHead,
                    hh,
                    false);

  LOG(LM_CHUNK, LL_DEBUG,
      "Allocate chunk %p at level %u",
      (void*)chunk,
      level);

  return chunk;
}

void HM_unlinkChunk(HM_chunk chunk) {
  HM_chunkList levelHead = HM_getLevelHeadPathCompress(chunk);

  chunk->prevChunk->nextChunk = chunk->nextChunk;
  if (NULL != chunk->nextChunk) {
    chunk->nextChunk->prevChunk = chunk->prevChunk;
  }

  assert(!levelHead->isInToSpace);
  assert(levelHead->containingHH != COPY_OBJECT_HH_VALUE);
  levelHead->size -= HM_getChunkSize(chunk);
  levelHead->containingHH->locallyCollectibleSize -= HM_getChunkSize(chunk);
  if (levelHead->lastChunk == chunk) {
    levelHead->lastChunk = chunk->prevChunk;
  }

  chunk->levelHead = NULL;
  chunk->prevChunk = NULL;
  chunk->nextChunk = NULL;

#if ASSERT
  HM_assertChunkListInvariants(levelHead, levelHead->containingHH);
#endif

}

/* SAM_NOTE: TODO: put levelHeads at the front of the freelists so that we can
 * implement this in constant time (using lastChunk field) */
void HM_mergeFreeList(HM_chunk *parentFreeList, HM_chunk freeList) {
  if (NULL == freeList) {
    return;
  }

  if (NULL == *parentFreeList) {
    *parentFreeList = freeList;
    return;
  }

  HM_chunk chunk = *parentFreeList;
  while (chunk->nextChunk != NULL) {
    chunk = chunk->nextChunk;
  }
  chunk->nextChunk = freeList;
  freeList->prevChunk = chunk;

  HM_assertFreeListInvariants(*parentFreeList);
}

void HM_forwardHHObjptrsInChunkList(
  GC_state s,
  pointer start,
  ObjptrPredicateFunction predicate,
  void* predicateArgs,
  struct ForwardHHObjptrArgs* forwardHHObjptrArgs)
{
  HM_chunk chunk = HM_getChunkOf(start);

  pointer p = start;
  size_t i = 0;

  if (chunk == NULL) {
    DIE("could not find chunk of %p", (void*)chunk);
  }

  while (NULL != chunk) {

    /* Can I use foreachObjptrInRange() for this? */
    while (p != chunk->frontier) {
      p = advanceToObjectData(s, p);

      p = foreachObjptrInObject(s,
                                p,
                                FALSE,
                                predicate,
                                predicateArgs,
                                forwardHHObjptr,
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
}

void HM_forwardHHObjptrsInLevelList(
  GC_state s,
  HM_chunkList * levelList,
  ObjptrPredicateFunction predicate,
  void* predicateArgs,
  struct ForwardHHObjptrArgs* forwardHHObjptrArgs,
  bool expectEntanglement)
{
  Word32 savedMaxLevel = forwardHHObjptrArgs->maxLevel;
  forwardHHObjptrArgs->maxLevel = 0;

  for (HM_chunkList levelHead = *levelList;
       NULL != levelHead;
       levelHead = levelHead->nextHead) {
    LOCAL_USED_FOR_ASSERT void* savedLevelList = *levelList;

    assert(levelHead->firstChunk != NULL);

    LOG(LM_HH_COLLECTION, LL_DEBUG,
        "Sweeping level %u in %p",
        levelHead->level,
        (void*)levelList);

    /* RAM_NOTE: Changing of maxLevel here is redundant sometimes */
    if (expectEntanglement) {
      forwardHHObjptrArgs->maxLevel = savedMaxLevel;
    } else {
      forwardHHObjptrArgs->maxLevel = levelHead->level;
    }

    HM_forwardHHObjptrsInChunkList(
      s,
      HM_getChunkStart(levelHead->firstChunk),
      predicate,
      predicateArgs,
      forwardHHObjptrArgs);

    /* asserts that no new lower level has been created */
    assert(savedLevelList == *levelList);
  }

  forwardHHObjptrArgs->maxLevel = savedMaxLevel;
}

void HM_freeChunks(HM_chunkList * levelList, HM_chunk * freeList, Word32 minLevel) {
  // struct FreeLevelListIteratorArgs iteratorArgs = {
  //   .levelList = levelList,
  //   .chunkList = NULL,
  //   .minLevel = minLevel
  // };

  LOG(LM_CHUNK, LL_DEBUGMORE,
      "START FreeChunks levelList = %p, minLevel = %u",
      ((void*)(levelList)),
      minLevel);

  HM_chunkList list = *levelList;
  while (list != NULL && list->level >= minLevel) {
    HM_chunk chunk = list->firstChunk;
    if (NULL == chunk) {
      list = list->nextHead;
      continue;
    }
    list->firstChunk = chunk->nextChunk;
    // list->firstChunk->prevChunk = NULL;

    chunk->frontier = (pointer)chunk + sizeof(struct HM_chunk);
    chunk->prevChunk = NULL;
    chunk->nextChunk = *freeList;
    chunk->mightContainMultipleObjects = TRUE;
    if (NULL != chunk->nextChunk) {
      chunk->nextChunk->prevChunk = chunk;
    }
    *freeList = chunk;

#if ASSERT
    /* clear out memory to quickly catch some memory safety errors */
    pointer start = HM_getChunkStart(chunk);
    size_t length = (size_t)(chunk->limit - start);
    memset(start, 0xBF, length);
#endif
  }

  *levelList = list;

  // for (HM_chunk chunk = HM_freeLevelListIterator(&iteratorArgs);
  //      NULL != chunk;
  //      chunk = HM_freeLevelListIterator(&iteratorArgs)) {
  //   /* SAM_NOTE: TODO: get rid of this spaghetti code. */
  //   chunk->frontier = (pointer)chunk + sizeof(struct HM_chunk);
  //   chunk->prevChunk = NULL;
  //   chunk->nextChunk = *freeList;
  //   chunk->mightContainMultipleObjects = TRUE;
  //   if (NULL != chunk->nextChunk) {
  //     chunk->nextChunk->prevChunk = chunk;
  //   }
  //   *freeList = chunk;
  // }

  HM_assertFreeListInvariants(*freeList);
  LOG(LM_CHUNK, LL_DEBUGMORE,
      "END FreeChunks levelList = %p, minLevel = %u",
      (void*)levelList,
      minLevel);
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

HM_chunkList HM_getChunkListToChunkList(HM_chunkList levelHead) {
  assert(NULL != levelHead);
  assert(HM_isLevelHead(levelHead));

  return levelHead->toChunkList;
}

/* SAM_NOTE: TODO: presumably looking up the level list with a linear search is
 * not very expensive since the number of levels in well-behaved parallel
 * programs is small. That being said... can't we store them in a dynamically-
 * sized array? */
Word64 HM_getLevelSize(HM_chunkList levelList, Word32 level) {
  HM_chunkList cursor = levelList;
  assert(NULL == cursor || cursor->parent == cursor);
  while (cursor != NULL && cursor->level > level) {
    assert(HM_isLevelHead(cursor));
    cursor = cursor->nextHead;
  }

  if ((NULL == cursor) || cursor->level != level) {
    return 0;
  }

  return cursor->size;
}

void HM_setChunkListToChunkList(HM_chunkList levelHead, HM_chunkList toChunkList) {
  assert(NULL != levelHead);
  assert(HM_isLevelHead(levelHead));

  levelHead->toChunkList = toChunkList;
  LOG(LM_CHUNK, LL_DEBUGMORE,
      "Set toChunkList of chunk %p to %p",
      (void*)levelHead,
      (void*)toChunkList);
}

HM_chunkList HM_getLevelHeadPathCompress(HM_chunk chunk) {
  HM_chunkList levelHead = chunk->levelHead;
  while (levelHead->parent != levelHead) {
    levelHead = levelHead->parent;
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

void HM_getObjptrInfo(GC_state s,
                      objptr object,
                      struct HM_ObjptrInfo* info) {
  assertObjptrInHH(object);

  HM_chunk chunk = HM_getChunkOf(objptrToPointer(object, s->heap->start));
  assert(NULL != chunk);

  HM_chunkList chunkList = HM_getLevelHeadPathCompress(chunk);

  assert(HM_isLevelHead(chunkList));
  info->hh = chunkList->containingHH;
  info->chunkList = chunkList;
  info->level = chunkList->level;
}

Word32 HM_getHighestLevel(HM_chunkList levelList) {
  if (NULL == levelList) {
    return CHUNK_INVALID_LEVEL;
  }

  return levelList->level;
}

void HM_mergeLevelList(
  HM_chunkList * destinationLevelList,
  HM_chunkList levelList,
  struct HM_HierarchicalHeap * const hh,
  bool resetToFromSpace)
{
  LOG(LM_CHUNK, LL_DEBUG,
      "Merging %p into %p",
      ((void*)(levelList)),
      ((void*)(*destinationLevelList)));

  HM_chunkList newLevelList = NULL;

  /* construct newLevelList */
  {
    HM_chunkList * previousChunkList = &newLevelList;
    HM_chunkList cursor1 = *destinationLevelList;
    HM_chunkList cursor2 = levelList;
    while ((NULL != cursor1) && (NULL != cursor2)) {
      size_t level1 = cursor1->level;
      size_t level2 = cursor2->level;
      assert(HM_isLevelHead(cursor1));
      assert(HM_isLevelHead(cursor2));

      if (level1 > level2) {
        /* append the first list */
        *previousChunkList = cursor1;

        /* advance cursor1 */
        cursor1 = cursor1->nextHead;
      } else if (level1 < level2) {
        /* append the second list */
        *previousChunkList = cursor2;

        /* advance cursor2 */
        cursor2 = cursor2->nextHead;
      } else {
        /* level1 == level2 */
        /* advance cursor 2 early since appendChunkList will unlink it */
        void* savedCursor2 = cursor2;
        cursor2 = cursor2->nextHead;

        /* merge second list into first before inserting */
        appendChunkList(cursor1, savedCursor2, 0xcafed00dbaadf00d);

        /* append the first list */
        *previousChunkList = cursor1;

        /* advance cursor1 */
        cursor1 = cursor1->nextHead;
      }

      /* set HH of this chunk list */
      (*previousChunkList)->containingHH = hh;

      /* advance previousChunkList */
      previousChunkList =
          &((*previousChunkList)->nextHead);
    }

    if (NULL != cursor1) {
      assert(NULL == cursor2);

      /* append the remainder of cursor1 */
      *previousChunkList = cursor1;
    } else if (NULL != cursor2) {
      assert(NULL == cursor1);

      /* append the remainder of cursor2 */
      *previousChunkList = cursor2;
    }

    /* set HH for remaining chunk lists */
    for (HM_chunkList chunkList = *previousChunkList;
         NULL != chunkList;
         chunkList = chunkList->nextHead) {
      chunkList->containingHH = hh;
    }
  }

  /* mark every chunk as in from-space since they have been merged */
  if (resetToFromSpace) {
    for (HM_chunkList chunkList = newLevelList;
         chunkList != NULL;
         chunkList = chunkList->nextHead) {
      chunkList->isInToSpace = false;
    }
  }

#if ASSERT
  if (newLevelList) {
    bool toSpace = newLevelList->containingHH
      == COPY_OBJECT_HH_VALUE;
    HM_assertLevelListInvariants(newLevelList,
                                 hh,
                                 HM_HH_INVALID_LEVEL,
                                 toSpace);
  }
#endif

  /* update destinationChunkList */
  *destinationLevelList = newLevelList;
}

void HM_promoteChunks(HM_chunkList * levelList, size_t level) {
  LOG(LM_CHUNK, LL_DEBUG,
      "Promoting level %zu in level list %p",
      level,
      ((void*)(*levelList)));

  const struct HM_HierarchicalHeap* hh =
    (*levelList)->containingHH;

  HM_assertLevelListInvariants(*levelList, hh, HM_HH_INVALID_LEVEL, false);

  /* find the pointer to level list of level 'level' */
  HM_chunkList * cursor;
  for (cursor = levelList;

#if ASSERT
       (NULL != *cursor) &&
#endif
                ((*cursor)->level > level);

       cursor = &((*cursor)->nextHead)) {
    assert(HM_isLevelHead(*cursor));
  }
  assert(HM_isLevelHead(*cursor));

  assert(NULL != *cursor);
  if ((*cursor)->level < level) {
    /* no chunks to promote */
    HM_assertLevelListInvariants(*levelList, hh, HM_HH_INVALID_LEVEL, false);
    return;
  }

  HM_chunkList chunkList = *cursor;
  /* unlink level list */
  *cursor = chunkList->nextHead;

  if ((NULL != *cursor) && (level - 1 == (*cursor)->level)) {
    /* need to merge into cursor */
    appendChunkList(*cursor, chunkList, 0xcafed00dbaadd00d);
  } else {
    /* need to reassign levelList to level - 1 */
    assert((NULL == *cursor) || (level - 1 > (*cursor)->level));
    chunkList->level = level - 1;

    /* insert chunkList where *cursor is */
    chunkList->nextHead = *cursor;
    *cursor = chunkList;
  }

  HM_assertLevelListInvariants(*levelList, hh, HM_HH_INVALID_LEVEL, false);
}

#if ASSERT
void HM_assertChunkInLevelList(HM_chunkList levelList, HM_chunk chunk) {
  for (HM_chunkList chunkList = levelList;
       NULL != chunkList;
       chunkList = chunkList->nextHead) {
    for (HM_chunk cursor = chunkList->firstChunk;
         NULL != cursor;
         cursor = cursor->nextChunk) {
      if (chunk == cursor) {
        /* found! */
        return;
      }
    }
  }

  /* If I get here, I couldn't find the chunk */
  ASSERTPRINT(FALSE,
              "Could not find chunk %p!",
              (void*)chunk);
}

void HM_assertLevelListInvariants(HM_chunkList levelList,
                                  const struct HM_HierarchicalHeap* hh,
                                  Word32 stealLevel,
                                  bool inToSpace) {
  Word32 previousLevel = ~((Word32)(0));
  for (HM_chunkList chunkList = levelList;
       NULL != chunkList;
       chunkList = chunkList->nextHead) {
    Word32 level = chunkList->level;
    struct HM_HierarchicalHeap* levelListHH =
      chunkList->containingHH;

    assert(chunkList->isInToSpace == inToSpace);

    assert(HM_isLevelHead(chunkList));
    assert(level < previousLevel);
    ASSERTPRINT((HM_HH_INVALID_LEVEL == stealLevel) || (level > stealLevel),
      "stealLevel %d; level %d",
      stealLevel,
      level);
    previousLevel = level;

    assert(hh == levelListHH);

    HM_assertChunkListInvariants(chunkList, levelListHH);
  }
}
#else
void HM_assertChunkInLevelList(HM_chunkList levelList, HM_chunk chunk) {
  ((void)(levelList));
  ((void)(chunk));
}

void HM_assertLevelListInvariants(HM_chunkList levelList,
                                  const struct HM_HierarchicalHeap* hh,
                                  Word32 stealLevel,
                                  bool inToSpace) {
  ((void)(levelList));
  ((void)(hh));
  ((void)(stealLevel));
  ((void)(inToSpace));
}
#endif /* ASSERT */

void HM_updateChunkValues(HM_chunk chunk, pointer frontier) {
  assert(chunk->frontier <= frontier && frontier <= chunk->limit);
  chunk->frontier = frontier;
}

void HM_updateLevelListPointers(HM_chunkList levelList,
                                struct HM_HierarchicalHeap* hh) {
  for (HM_chunkList cursor = levelList;
       NULL != cursor;
       cursor = cursor->nextHead) {
    cursor->containingHH = hh;
  }
}
#endif /* MLTON_GC_INTERNAL_FUNCS */

#if ASSERT
HM_chunkList getLevelHead(HM_chunk chunk) {
  HM_chunkList cursor = chunk->levelHead;
  assert(NULL != cursor);
  while (cursor->parent != cursor) {
    cursor = cursor->parent;
    assert(NULL != cursor);
  }

  assert(HM_isLevelHead(cursor));
  return cursor;
}
#endif

void appendChunkList(HM_chunkList destinationChunkList,
                     HM_chunkList chunkList,
                     ARG_USED_FOR_ASSERT size_t sentinel) {
  LOG(LM_CHUNK, LL_DEBUGMORE,
      "Appending %p into %p",
      ((void*)(chunkList)),
      ((void*)(destinationChunkList)));

  assert (NULL != destinationChunkList);
  assert(HM_isLevelHead(destinationChunkList));
  assert(HM_isLevelHead(chunkList));

  if (NULL == chunkList) {
    /* nothing to append */
    return;
  }

  /* append list */
  HM_chunk lastDestinationChunk = HM_getChunkListLastChunk(destinationChunkList);
  assert(NULL == lastDestinationChunk->nextChunk);
  lastDestinationChunk->nextChunk = chunkList->firstChunk;

  /* update level head chunk */
  HM_chunk lastChunk = HM_getChunkListLastChunk(chunkList);
  destinationChunkList->lastChunk = lastChunk;
  destinationChunkList->size += chunkList->size;

  /* demote chunkList's level head chunk */
#if ASSERT
  chunkList->nextHead = ((void*)(sentinel));
  chunkList->lastChunk = ((void*)(sentinel));
  chunkList->containingHH = ((struct HM_HierarchicalHeap*)(sentinel));
  chunkList->toChunkList = ((void*)(sentinel));
#endif

  chunkList->firstChunk->prevChunk = lastDestinationChunk;
  chunkList->parent = destinationChunkList;

  HM_assertChunkListInvariants(destinationChunkList,
                               destinationChunkList->containingHH);
}

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
  while (NULL != chunk->nextChunk) {
    assert(chunk->nextChunk->prevChunk == chunk);
    HM_assertChunkInvariants(chunk, chunkList);
    size += HM_getChunkSize(chunk);
    chunk = chunk->nextChunk;
  }
  HM_assertChunkInvariants(chunk, chunkList);
  size += HM_getChunkSize(chunk);

  assert(chunkList->containingHH == hh);
  assert(chunkList->size == size);
  assert(chunkList->lastChunk == chunk);
}
#else
void HM_assertChunkListInvariants(HM_chunkList chunkList,
                                  const struct HM_HierarchicalHeap* hh) {
  ((void)(chunkList));
  ((void)(hh));
}
#endif /* ASSERT */

#if ASSERT
void HM_assertFreeListInvariants(HM_chunk freeList) {
  HM_chunk chunk = freeList;
  while (chunk != NULL) {
    assert(chunk->magic == CHUNK_MAGIC);
    assert(chunk->frontier == (pointer)chunk + sizeof(struct HM_chunk));
    assert(chunk->limit >= chunk->frontier);
    assert(chunk->mightContainMultipleObjects == TRUE);
    if (chunk->nextChunk != NULL) {
      assert(chunk->nextChunk->prevChunk == chunk);
    }
    chunk = chunk->nextChunk;
  }
}
#else
void HM_assertFreeListInvariants(HM_chunk freeList) {
  ((void)freeList);
}
#endif

struct HM_HierarchicalHeap *HM_getObjptrHH(GC_state s, objptr object) {
  struct HM_ObjptrInfo objInfo;
  HM_getObjptrInfo(s, object, &objInfo);
  return objInfo.hh;
}

rwlock_t *HM_getObjptrHHLock(GC_state s, objptr object) {
  return &HM_getObjptrHH(s, object)->lock;
}

bool HM_isObjptrInToSpace(GC_state s, objptr object) {
  /* SAM_NOTE: why is this commented out? why are there two ways to check if
   * an object is in the toSpace? Does promotion use one, while collection
   * uses the other? */
  /* return HM_getObjptrLevelHeadChunk(s, object)->split.levelHead.isInToSpace; */
  HM_chunk c = HM_getChunkOf(objptrToPointer(object, s->heap->start));
  return HM_getLevelHeadPathCompress(c)->containingHH == COPY_OBJECT_HH_VALUE;
}


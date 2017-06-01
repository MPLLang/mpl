/* Copyright (C) 2015 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file chunk.c
 *
 * @author Ram Raghunathan
 *
 * This file implements the management interface defined in chunk.h
 */

#include "chunk.h"

/***********/
/* Structs */
/***********/
struct FreeLevelListIteratorArgs {
  void** levelList;
  void* chunkList;
  Word32 minLevel;
};

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
static void appendChunkList(void* destinationChunkList,
                            void* chunkList,
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
static void HM_assertChunkInvariants(const void* chunk,
                                     const struct HM_HierarchicalHeap* hh,
                                     const void* levelHeadChunk);
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
static void HM_assertChunkListInvariants(const void* chunkList,
                                         const struct HM_HierarchicalHeap* hh);

/**
 * A function to pass to ChunkPool_iteratedFree() for batch freeing of chunks
 * from a level list
 *
 * @param arg a struct FreeLevelListIteratorArgs* cast to void*
 *
 * @return pointer to chunk if it exists, NULL otherwise.
 */
void* HM_freeLevelListIterator(void* arg);

/**
 * This function retrieves the ChunkInfo object from a chunk
 *
 * @param chunk The chunk to retrieve the object from
 *
 * @return the ChunkInfo struct pointer
 */
static struct HM_ChunkInfo* getChunkInfo(void* chunk);

/**
 * Same as getChunkInfo() except for const-correctness. See getChunkInfo() for
 * more details
 */
static const struct HM_ChunkInfo* getChunkInfoConst(const void* chunk);

#if ASSERT
/**
 * Gets the level's head chunk for a given chunk.
 *
 * @param chunk The chunk to get the level head chunk for
 *
 * @return the head chunk of the level 'chunk' belongs to
 */
static const void* getLevelHeadChunk(const void* chunk);
#endif

/************************/
/* Function Definitions */
/************************/
#if (defined (MLTON_GC_INTERNAL_FUNCS))
void* HM_allocateChunk(void* levelHeadChunk, size_t allocableSize) {
  size_t totalSize = allocableSize + sizeof(struct HM_ChunkInfo);
  void* chunk = ChunkPool_allocate(&totalSize);

  if (NULL == chunk) {
    return NULL;
  }

#if ASSERT
  /* clear out memory to quickly catch some memory safety errors */
  void* start = HM_getChunkStart(chunk);
  size_t length = ((size_t)(chunk)) + totalSize - ((size_t)(start));
  memset(start, 0xAE, length);
#endif

  struct HM_ChunkInfo* chunkInfo = getChunkInfo(chunk);
#if ASSERT
  chunkInfo->split.levelHead.nextHead = ((void*)(0xcafebabedeadbeef));
  chunkInfo->split.levelHead.lastChunk = ((void*)(0xcafebabedeadbeef));
  chunkInfo->split.levelHead.containingHH =
      ((struct HM_HierarchicalHeap*)(0xcafebabedeadbeef));
  chunkInfo->split.levelHead.toChunkList = ((void*)(0xcafebabedeadbeef));
#endif

  chunkInfo->frontier = HM_getChunkStart(chunk);
  chunkInfo->limit = ((void*)(((char*)(chunk)) + ChunkPool_chunkSize(chunk)));
  chunkInfo->level = CHUNK_INVALID_LEVEL;
  chunkInfo->split.normal.levelHead = levelHeadChunk;

  /* assert that it is initialized correctly */
  assert(chunkInfo->limit != chunk);
  assert(totalSize == HM_getChunkSize(chunk));

  /* insert into list and update levelHeadChunk */
  chunkInfo->nextChunk = NULL;
  if (NULL == getChunkInfo(levelHeadChunk)->nextChunk) {
    /* empty list */
    getChunkInfo(levelHeadChunk)->nextChunk = chunk;
    getChunkInfo(levelHeadChunk)->split.levelHead.lastChunk = chunk;
  } else {
    void* lastChunk = getChunkInfo(levelHeadChunk)->split.levelHead.lastChunk;
    getChunkInfo(lastChunk)->nextChunk = chunk;
    getChunkInfo(levelHeadChunk)->split.levelHead.lastChunk = chunk;
  }
  getChunkInfo(levelHeadChunk)->split.levelHead.size += totalSize;

  LOG(LM_CHUNK, LL_DEBUG,
      "Allocate chunk %p at level %u",
      ((void*)(chunk)),
      getChunkInfo(levelHeadChunk)->level);

  return chunk;
}

void* HM_allocateLevelHeadChunk(void** levelList,
                                size_t allocableSize,
                                Word32 level,
                                struct HM_HierarchicalHeap* hh) {
  size_t totalSize = allocableSize + sizeof(struct HM_ChunkInfo);
  void* chunk = ChunkPool_allocate(&totalSize);

  if (NULL == chunk) {
    return NULL;
  }

#if ASSERT
  /* clear out memory to quickly catch some memory safety errors */
  void* start = HM_getChunkStart(chunk);
  size_t length = ((size_t)(chunk)) + totalSize - ((size_t)(start));
  memset(start, 0xAE, length);
#endif

  /* setup chunk info */
  struct HM_ChunkInfo* chunkInfo = getChunkInfo(chunk);
  chunkInfo->frontier = HM_getChunkStart(chunk);
  chunkInfo->limit = ((void*)(((char*)(chunk)) + ChunkPool_chunkSize(chunk)));
  chunkInfo->nextChunk = NULL;
  chunkInfo->level = level;
  chunkInfo->split.levelHead.nextHead = NULL;
  chunkInfo->split.levelHead.lastChunk = chunk;
  chunkInfo->split.levelHead.containingHH = hh;
  chunkInfo->split.levelHead.toChunkList = NULL;
  chunkInfo->split.levelHead.size = totalSize;

  /* assert that it is initialized correctly */
  assert(chunkInfo->limit != chunk);
  assert(totalSize == HM_getChunkSize(chunk));

  /* insert into level list */
  HM_mergeLevelList(levelList, chunk, hh);

  LOG(LM_CHUNK, LL_DEBUG,
      "Allocate chunk %p at level %u",
      ((void*)(chunk)),
      level);

  return chunk;
}

void HM_forwardHHObjptrsInLevelList(
    GC_state s,
    void** levelList,
    ObjptrPredicateFunction predicate,
    void* predicateArgs,
    struct ForwardHHObjptrArgs* forwardHHObjptrArgs) {
  Word32 savedMaxLevel = forwardHHObjptrArgs->maxLevel;
  forwardHHObjptrArgs->maxLevel = 0;

  for (void* levelHead = *levelList;
       NULL != levelHead;
       levelHead = getChunkInfo(levelHead)->split.levelHead.nextHead) {
    for (void* chunk = levelHead;
         NULL != chunk;
         chunk = getChunkInfo(chunk)->nextChunk) {
      /* Can I use foreachObjptrInRange() for this? */
      for (pointer p = HM_getChunkStart(chunk);
           p != getChunkInfo(chunk)->frontier;) {
        LOCAL_USED_FOR_ASSERT void* savedLevelList = *levelList;

        p = advanceToObjectData(s, p);
        forwardHHObjptrArgs->maxLevel = getChunkInfo(levelHead)->level;
        p = foreachObjptrInObject(s,
                                  p,
                                  FALSE,
                                  predicate,
                                  predicateArgs,
                                  forwardHHObjptr,
                                  forwardHHObjptrArgs);

        /* asserts that no new lower level has been created */
        assert(savedLevelList == *levelList);
      }
    }
  }

  forwardHHObjptrArgs->maxLevel = savedMaxLevel;
}

void HM_freeChunks(void** levelList, Word32 minLevel) {
  struct FreeLevelListIteratorArgs iteratorArgs = {
    .levelList = levelList,
    .chunkList = NULL,
    .minLevel = minLevel
  };
  LOG(LM_CHUNK, LL_DEBUGMORE,
      "START FreeChunks levelList = %p, minLevel = %u",
      ((void*)(iteratorArgs.levelList)),
      iteratorArgs.minLevel);
  LOCAL_USED_FOR_ASSERT bool result =
      ChunkPool_iteratedFree(HM_freeLevelListIterator, &iteratorArgs);
  assert(result);
  LOG(LM_CHUNK, LL_DEBUGMORE,
      "END FreeChunks levelList = %p, minLevel = %u",
      ((void*)(iteratorArgs.levelList)),
      iteratorArgs.minLevel);
}

void* HM_getChunkFrontier(void* chunk) {
  return getChunkInfo(chunk)->frontier;
}

void* HM_getChunkLimit(void* chunk) {
  return getChunkInfo(chunk)->limit;
}

Word64 HM_getChunkSize(const void* chunk) {
  return (((Word64)(getChunkInfoConst(chunk)->limit)) - ((Word64)(chunk)));
}

void* HM_getChunkStart(void* chunk) {
  return ((void*)(((char*)(chunk)) + sizeof(struct HM_ChunkInfo)));
}

Word32 HM_getChunkListLevel(void* chunkList) {
  assert(CHUNK_INVALID_LEVEL != getChunkInfo(chunkList)->level);
  return ((Word32)(getChunkInfo(chunkList)->level));
}

void* HM_getChunkListLastChunk(void* chunkList) {
  if (NULL == chunkList) {
    return NULL;
  }

  assert(CHUNK_INVALID_LEVEL != getChunkInfo(chunkList)->level);
  return getChunkInfo(chunkList)->split.levelHead.lastChunk;
}

void* HM_getChunkListToChunkList(void* chunkList) {
  assert(NULL != chunkList);
  assert(CHUNK_INVALID_LEVEL != getChunkInfo(chunkList)->level);

  return getChunkInfo(chunkList)->split.levelHead.toChunkList;
}

Word64 HM_getLevelSize(void* levelList, Word32 level) {
  void* cursor;
  for (cursor = levelList;
       (cursor != NULL) && (getChunkInfo(cursor)->level > level);
       cursor = getChunkInfo(cursor)->split.levelHead.nextHead) { }

  if ((NULL == cursor) || (getChunkInfo(cursor)->level != level)) {
    return 0;
  }

  return getChunkInfo(cursor)->split.levelHead.size;
}

void HM_setChunkListToChunkList(void* chunkList, void* toChunkList) {
  assert(NULL != chunkList);
  assert(CHUNK_INVALID_LEVEL != getChunkInfo(chunkList)->level);

  getChunkInfo(chunkList)->split.levelHead.toChunkList = toChunkList;
  LOG(LM_CHUNK, LL_DEBUGMORE,
      "Set toChunkList of chunk %p to %p",
      chunkList,
      toChunkList);
}

void HM_getObjptrInfo(GC_state s,
                      objptr object,
                      struct HM_ObjptrInfo* info) {
  assert(HM_HH_objptrInHierarchicalHeap(s, object));

  void* chunk = ChunkPool_find(objptrToPointer(object, s->heap->start));
  assert(NULL != chunk);

  void* chunkList;
  for(chunkList = chunk;
      (NULL != chunkList) &&
                  (CHUNK_INVALID_LEVEL == getChunkInfo(chunkList)->level);
      chunkList = getChunkInfo(chunkList)->split.normal.levelHead) { }

  if (NULL == chunkList) {
    DIE("Couldn't get objptrinfo for %p",
        ((void*)(object)));
  }

  /* now that I have the chunkList, path compress */
  void* parentChunk = NULL;
  while (chunkList != chunk) {
    assert(CHUNK_INVALID_LEVEL == getChunkInfo(chunk)->level);

    parentChunk = getChunkInfo(chunk)->split.normal.levelHead;
    getChunkInfo(chunk)->split.normal.levelHead = chunkList;
    chunk = parentChunk;
  }

  info->hh = getChunkInfo(chunkList)->split.levelHead.containingHH;
  info->chunkList = chunkList;
  info->level = getChunkInfo(chunkList)->level;
}

Word32 HM_getHighestLevel(const void* levelList) {
  if (NULL == levelList) {
    return CHUNK_INVALID_LEVEL;
  }

  ASSERTPRINT(CHUNK_INVALID_LEVEL != getChunkInfoConst(levelList)->level,
              "Chunk %p is not a level head chunk!",
              ((const void*)(levelList)));
  return (getChunkInfoConst(levelList)->level);
}

void HM_mergeLevelList(void** destinationLevelList,
                       void* levelList,
                       struct HM_HierarchicalHeap * const hh) {
  LOG(LM_CHUNK, LL_DEBUG,
      "Merging %p into %p",
      ((void*)(levelList)),
      ((void*)(*destinationLevelList)));

  void* newLevelList = NULL;

  /* construct newLevelList */
  {
    void** previousChunkList = &newLevelList;
    void* cursor1 = *destinationLevelList;
    void* cursor2 = levelList;
    while ((NULL != cursor1) && (NULL != cursor2)) {
      size_t level1 = getChunkInfo(cursor1)->level;
      size_t level2 = getChunkInfo(cursor2)->level;
      assert(CHUNK_INVALID_LEVEL != level1);
      assert(CHUNK_INVALID_LEVEL != level2);

      if (level1 > level2) {
        /* append the first list */
        *previousChunkList = cursor1;

        /* advance cursor1 */
        cursor1 = getChunkInfo(cursor1)->split.levelHead.nextHead;
      } else if (level1 < level2) {
        /* append the second list */
        *previousChunkList = cursor2;

        /* advance cursor2 */
        cursor2 = getChunkInfo(cursor2)->split.levelHead.nextHead;
      } else {
        /* level1 == level2 */
        /* advance cursor 2 early since appendChunkList will unlink it */
        void* savedCursor2 = cursor2;
        cursor2 = getChunkInfo(cursor2)->split.levelHead.nextHead;

        /* merge second list into first before inserting */
        appendChunkList(cursor1, savedCursor2, 0xcafed00dbaadf00d);

        /* append the first list */
        *previousChunkList = cursor1;

        /* advance cursor1 */
        cursor1 = getChunkInfo(cursor1)->split.levelHead.nextHead;
      }

      /* set HH of this chunk list */
      getChunkInfo(*previousChunkList)->split.levelHead.containingHH = hh;

      /* advance previousChunkList */
      previousChunkList =
          &(getChunkInfo(*previousChunkList)->split.levelHead.nextHead);
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
    for (void* chunkList = *previousChunkList;
         NULL != chunkList;
         chunkList = getChunkInfo(chunkList)->split.levelHead.nextHead) {
      getChunkInfo(chunkList)->split.levelHead.containingHH = hh;
    }
  }

  HM_assertLevelListInvariants(newLevelList, hh, HM_HH_INVALID_LEVEL);

  /* update destinationChunkList */
  *destinationLevelList = newLevelList;
}

void HM_promoteChunks(void** levelList, size_t level) {
  LOG(LM_CHUNK, LL_DEBUG,
      "Promoting level %zu in level list %p",
      level,
      ((void*)(*levelList)));

  const struct HM_HierarchicalHeap* hh =
      getChunkInfoConst(*levelList)->split.levelHead.containingHH;

  HM_assertLevelListInvariants(*levelList, hh, HM_HH_INVALID_LEVEL);

  /* find the pointer to level list of level 'level' */
  void** cursor;
  for (cursor = levelList;

#if ASSERT
       (NULL != *cursor) &&
#endif
                (getChunkInfo(*cursor)->level > level);

       cursor = &(getChunkInfo(*cursor)->split.levelHead.nextHead)) {
    assert(CHUNK_INVALID_LEVEL != getChunkInfo(*cursor)->level);
  }
  assert(CHUNK_INVALID_LEVEL != getChunkInfo(*cursor)->level);

  assert(NULL != *cursor);
  if (getChunkInfo(*cursor)->level < level) {
    /* no chunks to promote */
    HM_assertLevelListInvariants(*levelList, hh, HM_HH_INVALID_LEVEL);
    return;
  }

  void* chunkList = *cursor;
  /* unlink level list */
  *cursor = getChunkInfo(chunkList)->split.levelHead.nextHead;

  if ((NULL != *cursor) && (level - 1 == getChunkInfo(*cursor)->level)) {
    /* need to merge into cursor */
    appendChunkList(*cursor, chunkList, 0xcafed00dbaadd00d);
  } else {
    /* need to reassign levelList to level - 1 */
    assert((NULL == *cursor) || (level - 1 > getChunkInfo(*cursor)->level));
    getChunkInfo(chunkList)->level = level - 1;

    /* insert chunkList where *cursor is */
    getChunkInfo(chunkList)->split.levelHead.nextHead = *cursor;
    *cursor = chunkList;
  }

  HM_assertLevelListInvariants(*levelList, hh, HM_HH_INVALID_LEVEL);
}

#if ASSERT
void HM_assertChunkInLevelList(const void* levelList, const void* chunk) {
  for (const void* chunkList = levelList;
       NULL != chunkList;
       chunkList = getChunkInfoConst(chunkList)->split.levelHead.nextHead) {
    for (const void* cursor = chunkList;
         NULL != cursor;
         cursor = getChunkInfoConst(cursor)->nextChunk) {
      if (chunk == cursor) {
        /* found! */
        return;
      }
    }
  }

  /* If I get here, I couldn't find the chunk */
  ASSERTPRINT(FALSE,
              "Could not find chunk %p!",
              chunk);
}

void HM_assertLevelListInvariants(const void* levelList,
                                  const struct HM_HierarchicalHeap* hh,
                                  Word32 stealLevel) {
  Word32 previousLevel = ~((Word32)(0));
  for (const void* chunkList = levelList;
       NULL != chunkList;
       chunkList = getChunkInfoConst(chunkList)->split.levelHead.nextHead) {
    Word32 level = getChunkInfoConst(chunkList)->level;
    struct HM_HierarchicalHeap* levelListHH =
        getChunkInfoConst(chunkList)->split.levelHead.containingHH;

    assert(CHUNK_INVALID_LEVEL != level);
    assert(level < previousLevel);
    assert((HM_HH_INVALID_LEVEL == stealLevel) || (level > stealLevel));
    previousLevel = level;

    assert(hh == levelListHH);

    HM_assertChunkListInvariants(chunkList, levelListHH);
  }
}
#else
void HM_assertChunkInLevelList(const void* levelList, const void* chunk) {
  ((void)(levelList));
  ((void)(chunk));
}

void HM_assertLevelListInvariants(const void* levelList,
                                  const struct HM_HierarchicalHeap* hh,
                                  Word32 stealLevel) {
  ((void)(levelList));
  ((void)(hh));
  ((void)(stealLevel));
}
#endif /* ASSERT */

void HM_updateChunkValues(void* chunk, void* frontier) {
  assert(ChunkPool_find(((char*)(frontier)) - 1) == chunk);
  getChunkInfo(chunk)->frontier = frontier;
}

void HM_updateLevelListPointers(void* levelList,
                                struct HM_HierarchicalHeap* hh) {
  for (void* cursor = levelList;
       NULL != cursor;
       cursor = getChunkInfo(cursor)->split.levelHead.nextHead) {
    getChunkInfo(cursor)->split.levelHead.containingHH = hh;
  }
}
#endif /* MLTON_GC_INTERNAL_FUNCS */

void appendChunkList(void* destinationChunkList,
                     void* chunkList,
                     ARG_USED_FOR_ASSERT size_t sentinel) {
  LOG(LM_CHUNK, LL_DEBUGMORE,
      "Appending %p into %p",
      ((void*)(chunkList)),
      ((void*)(destinationChunkList)));

  assert (NULL != destinationChunkList);
  assert(CHUNK_INVALID_LEVEL != getChunkInfo(destinationChunkList)->level);
  assert(CHUNK_INVALID_LEVEL != getChunkInfo(chunkList)->level);

  if (NULL == chunkList) {
    /* nothing to append */
    return;
  }

  /* append list */
  void* lastDestinationChunk = HM_getChunkListLastChunk(destinationChunkList);
  assert(NULL == getChunkInfo(lastDestinationChunk)->nextChunk);
  getChunkInfo(lastDestinationChunk)->nextChunk = chunkList;

  /* update level head chunk */
  void* lastChunk = HM_getChunkListLastChunk(chunkList);
  getChunkInfo(destinationChunkList)->split.levelHead.lastChunk = lastChunk;
  getChunkInfo(destinationChunkList)->split.levelHead.size +=
      getChunkInfo(chunkList)->split.levelHead.size;

  /* demote chunkList's level head chunk */
#if ASSERT
  getChunkInfo(chunkList)->split.levelHead.nextHead =
      ((void*)(sentinel));
  getChunkInfo(chunkList)->split.levelHead.lastChunk =
      ((void*)(sentinel));
  getChunkInfo(chunkList)->split.levelHead.containingHH =
      ((struct HM_HierarchicalHeap*)(sentinel));
  getChunkInfo(chunkList)->split.levelHead.toChunkList =
      ((void*)(sentinel));
#endif

  getChunkInfo(chunkList)->level = CHUNK_INVALID_LEVEL;
  getChunkInfo(chunkList)->split.normal.levelHead = destinationChunkList;

  HM_assertChunkListInvariants(destinationChunkList,
                               getChunkInfo(destinationChunkList)->
                               split.levelHead.containingHH);
}

#if ASSERT
void HM_assertChunkInvariants(const void* chunk,
                              const struct HM_HierarchicalHeap* hh,
                              const void* levelHeadChunk) {
  const struct HM_ChunkInfo* chunkInfo = getChunkInfoConst(chunk);

  assert(ChunkPool_find(((char*)(chunkInfo->frontier)) - 1) == chunk);

  if (chunk == levelHeadChunk) {
    /* this is the level head chunk */
    assert(CHUNK_INVALID_LEVEL != chunkInfo->level);
    assert(hh == chunkInfo->split.levelHead.containingHH);
  } else {
    /* this is a normal chunk */
    assert(CHUNK_INVALID_LEVEL == chunkInfo->level);
  }

  assert(levelHeadChunk == getLevelHeadChunk(chunk));
}

void HM_assertChunkListInvariants(const void* chunkList,
                                  const struct HM_HierarchicalHeap* hh) {
  Word64 size = 0;
  for (const void* chunk = chunkList;
       NULL != chunk;
       chunk = getChunkInfoConst(chunk)->nextChunk) {
    HM_assertChunkInvariants(chunk, hh, chunkList);
    size += HM_getChunkSize(chunk);
  }

  assert(getChunkInfoConst(chunkList)->split.levelHead.size == size);
}
#else
void HM_assertChunkListInvariants(const void* chunkList,
                                  const struct HM_HierarchicalHeap* hh) {
  ((void)(chunkList));
  ((void)(hh));
}
#endif /* ASSERT */

void* HM_freeLevelListIterator(void* arg) {
  struct FreeLevelListIteratorArgs* state =
      ((struct FreeLevelListIteratorArgs*)(arg));

  if (NULL == state->chunkList) {
    /* get chunk list from level list */
    state->chunkList = *(state->levelList);

    if ((NULL == state->chunkList) ||
        (getChunkInfo(state->chunkList)->level < state->minLevel)) {
      /* all done */
      return NULL;
    }

    /* we should be at a levelHead */
    assert(getChunkInfo(state->chunkList)->level != CHUNK_INVALID_LEVEL);

    /* this chunk list will be freed, so unlink and advance level list */
    *(state->levelList) =
        getChunkInfo(state->chunkList)->split.levelHead.nextHead;

    LOG(LM_CHUNK, LL_DEBUG,
        "Freeing chunk list at level %u %u",
        getChunkInfo(state->chunkList)->level,
        state->minLevel);
  }

  void* chunk = state->chunkList;

  /* advance chunkList */
  state->chunkList = getChunkInfo(state->chunkList)->nextChunk;

#if ASSERT
  /* clear out memory to quickly catch some memory safety errors */
  void* start = HM_getChunkStart(chunk);
  size_t length = ((size_t)(getChunkInfo(chunk)->limit)) - ((size_t)(start));
  memset(start, 0xBF, length);
#endif

  return chunk;
}

struct HM_ChunkInfo* getChunkInfo(void* chunk) {
  return ((struct HM_ChunkInfo*)(chunk));
}

const struct HM_ChunkInfo* getChunkInfoConst(const void* chunk) {
  return ((const struct HM_ChunkInfo*)(chunk));
}

#if ASSERT
const void* getLevelHeadChunk(const void* chunk) {
  const void* cursor;
  for (cursor = chunk;
       (NULL != cursor) &&
                CHUNK_INVALID_LEVEL == getChunkInfoConst(cursor)->level;
       cursor = getChunkInfoConst(cursor)->split.normal.levelHead) {
  }
  assert(NULL != cursor);

  return cursor;
}
#endif

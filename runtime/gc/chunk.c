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

  struct HM_ChunkInfo* chunkInfo = HM_getChunkInfo(chunk);
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
  if (NULL == HM_getChunkInfo(levelHeadChunk)->nextChunk) {
    /* empty list */
    HM_getChunkInfo(levelHeadChunk)->nextChunk = chunk;
    HM_getChunkInfo(levelHeadChunk)->split.levelHead.lastChunk = chunk;
  } else {
    void* lastChunk =
      HM_getChunkInfo(levelHeadChunk)->split.levelHead.lastChunk;
    HM_getChunkInfo(lastChunk)->nextChunk = chunk;
    HM_getChunkInfo(levelHeadChunk)->split.levelHead.lastChunk = chunk;
  }
  HM_getChunkInfo(levelHeadChunk)->split.levelHead.size += totalSize;

  LOG(LM_CHUNK, LL_DEBUG,
      "Allocate chunk %p at level %u",
      ((void*)(chunk)),
      HM_getChunkInfo(levelHeadChunk)->level);

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
  struct HM_ChunkInfo* chunkInfo = HM_getChunkInfo(chunk);
  chunkInfo->frontier = HM_getChunkStart(chunk);
  chunkInfo->limit = ((void*)(((char*)(chunk)) + ChunkPool_chunkSize(chunk)));
  chunkInfo->nextChunk = NULL;
  chunkInfo->level = level;
  chunkInfo->split.levelHead.nextHead = NULL;
  chunkInfo->split.levelHead.lastChunk = chunk;
  chunkInfo->split.levelHead.containingHH = hh;
  chunkInfo->split.levelHead.toChunkList = NULL;
  chunkInfo->split.levelHead.size = totalSize;
  chunkInfo->split.levelHead.isInToSpace = (hh == COPY_OBJECT_HH_VALUE);

  /* assert that it is initialized correctly */
  assert(chunkInfo->limit != chunk);
  assert(totalSize == HM_getChunkSize(chunk));

  /* insert into level list */
  HM_mergeLevelList(levelList,
                    chunk,
                    hh,
                    false);

  LOG(LM_CHUNK, LL_DEBUG,
      "Allocate chunk %p at level %u",
      ((void*)(chunk)),
      level);

  return chunk;
}

void HM_forwardHHObjptrsInChunkList(
    GC_state s,
    void *start,
    ObjptrPredicateFunction predicate,
    void* predicateArgs,
    struct ForwardHHObjptrArgs* forwardHHObjptrArgs) {
  void *chunk = ChunkPool_find(start);
  pointer p = start;

  if (chunk == NULL) {
      DIE("could not find chunk of %p", chunk);
  }

  for (;
       NULL != chunk;
       chunk = HM_getChunkInfo(chunk)->nextChunk, p = HM_getChunkStart(chunk)) {
      /* Can I use foreachObjptrInRange() for this? */
    while (p != HM_getChunkInfo(chunk)->frontier) {
      p = advanceToObjectData(s, p);

      p = foreachObjptrInObject(s,
                                p,
                                FALSE,
                                predicate,
                                predicateArgs,
                                forwardHHObjptr,
                                forwardHHObjptrArgs);
    }
    Trace3(EVENT_COPY,
           (EventInt)forwardHHObjptrArgs->bytesCopied,
           (EventInt)forwardHHObjptrArgs->objectsCopied,
           (EventInt)forwardHHObjptrArgs->stacksCopied);
  }
}

void HM_forwardHHObjptrsInLevelList(
    GC_state s,
    void** levelList,
    ObjptrPredicateFunction predicate,
    void* predicateArgs,
    struct ForwardHHObjptrArgs* forwardHHObjptrArgs,
    bool expectEntanglement) {
  Word32 savedMaxLevel = forwardHHObjptrArgs->maxLevel;
  forwardHHObjptrArgs->maxLevel = 0;

  for (void* levelHead = *levelList;
       NULL != levelHead;
       levelHead = HM_getChunkInfo(levelHead)->split.levelHead.nextHead) {
    LOCAL_USED_FOR_ASSERT void* savedLevelList = *levelList;

    LOG(LM_HH_COLLECTION, LL_DEBUG,
        "Sweeping level %u in %p",
        HM_getChunkInfo(levelHead)->level,
        (void *)levelList);

    /* RAM_NOTE: Changing of maxLevel here is redundant sometimes */
    if (expectEntanglement) {
        forwardHHObjptrArgs->maxLevel = savedMaxLevel;
    } else {
        forwardHHObjptrArgs->maxLevel = HM_getChunkInfo(levelHead)->level;
    }

    HM_forwardHHObjptrsInChunkList(s,
                                   HM_getChunkStart(levelHead),
                                   predicate,
                                   predicateArgs,
                                   forwardHHObjptrArgs);

    /* asserts that no new lower level has been created */
    assert(savedLevelList == *levelList);
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
  return HM_getChunkInfo(chunk)->frontier;
}

void* HM_getChunkLimit(void* chunk) {
  return HM_getChunkInfo(chunk)->limit;
}

Word64 HM_getChunkSize(const void* chunk) {
  return (((Word64)(HM_getChunkInfoConst(chunk)->limit)) - ((Word64)(chunk)));
}

void* HM_getChunkStart(void* chunk) {
  return ((void*)(((char*)(chunk)) + sizeof(struct HM_ChunkInfo)));
}

Word32 HM_getChunkListLevel(void* chunkList) {
  assert(CHUNK_INVALID_LEVEL != HM_getChunkInfo(chunkList)->level);
  return ((Word32)(HM_getChunkInfo(chunkList)->level));
}

void* HM_getChunkListLastChunk(void* chunkList) {
  if (NULL == chunkList) {
    return NULL;
  }

  assert(CHUNK_INVALID_LEVEL != HM_getChunkInfo(chunkList)->level);
  return HM_getChunkInfo(chunkList)->split.levelHead.lastChunk;
}

void* HM_getChunkListToChunkList(void* chunkList) {
  assert(NULL != chunkList);
  assert(CHUNK_INVALID_LEVEL != HM_getChunkInfo(chunkList)->level);

  return HM_getChunkInfo(chunkList)->split.levelHead.toChunkList;
}

Word64 HM_getLevelSize(void* levelList, Word32 level) {
  void* cursor;
  for (cursor = levelList;
       (cursor != NULL) && (HM_getChunkInfo(cursor)->level > level);
       cursor = HM_getChunkInfo(cursor)->split.levelHead.nextHead) { }

  if ((NULL == cursor) || (HM_getChunkInfo(cursor)->level != level)) {
    return 0;
  }

  return HM_getChunkInfo(cursor)->split.levelHead.size;
}

void HM_setChunkListToChunkList(void* chunkList, void* toChunkList) {
  assert(NULL != chunkList);
  assert(CHUNK_INVALID_LEVEL != HM_getChunkInfo(chunkList)->level);

  HM_getChunkInfo(chunkList)->split.levelHead.toChunkList = toChunkList;
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
                  (CHUNK_INVALID_LEVEL == HM_getChunkInfo(chunkList)->level);
      chunkList = HM_getChunkInfo(chunkList)->split.normal.levelHead) { }

  if (NULL == chunkList) {
    DIE("Couldn't get objptrinfo for %p",
        ((void*)(object)));
  }

  /* now that I have the chunkList, path compress */
  void* parentChunk = NULL;
  while (chunkList != chunk) {
    assert(CHUNK_INVALID_LEVEL == HM_getChunkInfo(chunk)->level);

    parentChunk = HM_getChunkInfo(chunk)->split.normal.levelHead;
    HM_getChunkInfo(chunk)->split.normal.levelHead = chunkList;
    chunk = parentChunk;
  }

  info->hh = HM_getChunkInfo(chunkList)->split.levelHead.containingHH;
  info->chunkList = chunkList;
  info->level = HM_getChunkInfo(chunkList)->level;
}

Word32 HM_getHighestLevel(const void* levelList) {
  if (NULL == levelList) {
    return CHUNK_INVALID_LEVEL;
  }

  ASSERTPRINT(CHUNK_INVALID_LEVEL != HM_getChunkInfoConst(levelList)->level,
              "Chunk %p is not a level head chunk!",
              ((const void*)(levelList)));
  return (HM_getChunkInfoConst(levelList)->level);
}

void HM_mergeLevelList(void** destinationLevelList,
                       void* levelList,
                       struct HM_HierarchicalHeap * const hh,
                       bool resetToFromSpace) {
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
      size_t level1 = HM_getChunkInfo(cursor1)->level;
      size_t level2 = HM_getChunkInfo(cursor2)->level;
      assert(CHUNK_INVALID_LEVEL != level1);
      assert(CHUNK_INVALID_LEVEL != level2);

      if (level1 > level2) {
        /* append the first list */
        *previousChunkList = cursor1;

        /* advance cursor1 */
        cursor1 = HM_getChunkInfo(cursor1)->split.levelHead.nextHead;
      } else if (level1 < level2) {
        /* append the second list */
        *previousChunkList = cursor2;

        /* advance cursor2 */
        cursor2 = HM_getChunkInfo(cursor2)->split.levelHead.nextHead;
      } else {
        /* level1 == level2 */
        /* advance cursor 2 early since appendChunkList will unlink it */
        void* savedCursor2 = cursor2;
        cursor2 = HM_getChunkInfo(cursor2)->split.levelHead.nextHead;

        /* merge second list into first before inserting */
        appendChunkList(cursor1, savedCursor2, 0xcafed00dbaadf00d);

        /* append the first list */
        *previousChunkList = cursor1;

        /* advance cursor1 */
        cursor1 = HM_getChunkInfo(cursor1)->split.levelHead.nextHead;
      }

      /* set HH of this chunk list */
      HM_getChunkInfo(*previousChunkList)->split.levelHead.containingHH = hh;

      /* advance previousChunkList */
      previousChunkList =
          &(HM_getChunkInfo(*previousChunkList)->split.levelHead.nextHead);
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
         chunkList = HM_getChunkInfo(chunkList)->split.levelHead.nextHead) {
      HM_getChunkInfo(chunkList)->split.levelHead.containingHH = hh;
    }
  }

  /* mark every chunk as in from-space since they have been merged */
  if (resetToFromSpace) {
    for (void *chunk = newLevelList;
         chunk != NULL;
         chunk = HM_getChunkInfo(chunk)->split.levelHead.nextHead) {
      HM_getChunkInfo(chunk)->split.levelHead.isInToSpace = false;
    }
  }

#if ASSERT
  if (newLevelList) {
    bool toSpace = HM_getChunkInfo(newLevelList)->split.levelHead.containingHH
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

void HM_promoteChunks(void** levelList, size_t level) {
  LOG(LM_CHUNK, LL_DEBUG,
      "Promoting level %zu in level list %p",
      level,
      ((void*)(*levelList)));

  const struct HM_HierarchicalHeap* hh =
      HM_getChunkInfoConst(*levelList)->split.levelHead.containingHH;

  HM_assertLevelListInvariants(*levelList, hh, HM_HH_INVALID_LEVEL, false);

  /* find the pointer to level list of level 'level' */
  void** cursor;
  for (cursor = levelList;

#if ASSERT
       (NULL != *cursor) &&
#endif
                (HM_getChunkInfo(*cursor)->level > level);

       cursor = &(HM_getChunkInfo(*cursor)->split.levelHead.nextHead)) {
    assert(CHUNK_INVALID_LEVEL != HM_getChunkInfo(*cursor)->level);
  }
  assert(CHUNK_INVALID_LEVEL != HM_getChunkInfo(*cursor)->level);

  assert(NULL != *cursor);
  if (HM_getChunkInfo(*cursor)->level < level) {
    /* no chunks to promote */
    HM_assertLevelListInvariants(*levelList, hh, HM_HH_INVALID_LEVEL, false);
    return;
  }

  void* chunkList = *cursor;
  /* unlink level list */
  *cursor = HM_getChunkInfo(chunkList)->split.levelHead.nextHead;

  if ((NULL != *cursor) && (level - 1 == HM_getChunkInfo(*cursor)->level)) {
    /* need to merge into cursor */
    appendChunkList(*cursor, chunkList, 0xcafed00dbaadd00d);
  } else {
    /* need to reassign levelList to level - 1 */
    assert((NULL == *cursor) || (level - 1 > HM_getChunkInfo(*cursor)->level));
    HM_getChunkInfo(chunkList)->level = level - 1;

    /* insert chunkList where *cursor is */
    HM_getChunkInfo(chunkList)->split.levelHead.nextHead = *cursor;
    *cursor = chunkList;
  }

  HM_assertLevelListInvariants(*levelList, hh, HM_HH_INVALID_LEVEL, false);
}

#if ASSERT
void HM_assertChunkInLevelList(const void* levelList, const void* chunk) {
  for (const void* chunkList = levelList;
       NULL != chunkList;
       chunkList = HM_getChunkInfoConst(chunkList)->split.levelHead.nextHead) {
    for (const void* cursor = chunkList;
         NULL != cursor;
         cursor = HM_getChunkInfoConst(cursor)->nextChunk) {
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
                                  Word32 stealLevel,
                                  bool inToSpace) {
  Word32 previousLevel = ~((Word32)(0));
  for (const void* chunkList = levelList;
       NULL != chunkList;
       chunkList = HM_getChunkInfoConst(chunkList)->split.levelHead.nextHead) {
    Word32 level = HM_getChunkInfoConst(chunkList)->level;
    struct HM_HierarchicalHeap* levelListHH =
        HM_getChunkInfoConst(chunkList)->split.levelHead.containingHH;

    assert(HM_getChunkInfoConst(chunkList)->split.levelHead.isInToSpace
           == inToSpace);

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
                                  Word32 stealLevel,
                                  bool inToSpace) {
  ((void)(levelList));
  ((void)(hh));
  ((void)(stealLevel));
  ((void)(inToSpace));
}
#endif /* ASSERT */

void HM_updateChunkValues(void* chunk, void* frontier) {
  assert(ChunkPool_find(((char*)(frontier)) - 1) == chunk);
  HM_getChunkInfo(chunk)->frontier = frontier;
}

void HM_updateLevelListPointers(void* levelList,
                                struct HM_HierarchicalHeap* hh) {
  for (void* cursor = levelList;
       NULL != cursor;
       cursor = HM_getChunkInfo(cursor)->split.levelHead.nextHead) {
    HM_getChunkInfo(cursor)->split.levelHead.containingHH = hh;
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
  assert(CHUNK_INVALID_LEVEL != HM_getChunkInfo(destinationChunkList)->level);
  assert(CHUNK_INVALID_LEVEL != HM_getChunkInfo(chunkList)->level);

  if (NULL == chunkList) {
    /* nothing to append */
    return;
  }

  /* append list */
  void* lastDestinationChunk = HM_getChunkListLastChunk(destinationChunkList);
  assert(NULL == HM_getChunkInfo(lastDestinationChunk)->nextChunk);
  HM_getChunkInfo(lastDestinationChunk)->nextChunk = chunkList;

  /* update level head chunk */
  void* lastChunk = HM_getChunkListLastChunk(chunkList);
  HM_getChunkInfo(destinationChunkList)->split.levelHead.lastChunk = lastChunk;
  HM_getChunkInfo(destinationChunkList)->split.levelHead.size +=
      HM_getChunkInfo(chunkList)->split.levelHead.size;

  /* demote chunkList's level head chunk */
#if ASSERT
  HM_getChunkInfo(chunkList)->split.levelHead.nextHead =
      ((void*)(sentinel));
  HM_getChunkInfo(chunkList)->split.levelHead.lastChunk =
      ((void*)(sentinel));
  HM_getChunkInfo(chunkList)->split.levelHead.containingHH =
      ((struct HM_HierarchicalHeap*)(sentinel));
  HM_getChunkInfo(chunkList)->split.levelHead.toChunkList =
      ((void*)(sentinel));
#endif

  HM_getChunkInfo(chunkList)->level = CHUNK_INVALID_LEVEL;
  HM_getChunkInfo(chunkList)->split.normal.levelHead = destinationChunkList;

  HM_assertChunkListInvariants(destinationChunkList,
                               HM_getChunkInfo(destinationChunkList)->
                               split.levelHead.containingHH);
}

#if ASSERT
void HM_assertChunkInvariants(const void* chunk,
                              const struct HM_HierarchicalHeap* hh,
                              const void* levelHeadChunk) {
  const struct HM_ChunkInfo* chunkInfo = HM_getChunkInfoConst(chunk);

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
       chunk = HM_getChunkInfoConst(chunk)->nextChunk) {
    HM_assertChunkInvariants(chunk, hh, chunkList);
    size += HM_getChunkSize(chunk);
  }

  assert(HM_getChunkInfoConst(chunkList)->split.levelHead.size == size);
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
        (HM_getChunkInfo(state->chunkList)->level < state->minLevel)) {
      /* all done */
      return NULL;
    }

    /* we should be at a levelHead */
    assert(HM_getChunkInfo(state->chunkList)->level != CHUNK_INVALID_LEVEL);

    /* this chunk list will be freed, so unlink and advance level list */
    *(state->levelList) =
        HM_getChunkInfo(state->chunkList)->split.levelHead.nextHead;

    LOG(LM_CHUNK, LL_DEBUG,
        "Freeing chunk list at level %u %u",
        HM_getChunkInfo(state->chunkList)->level,
        state->minLevel);
  }

  void* chunk = state->chunkList;

  /* advance chunkList */
  state->chunkList = HM_getChunkInfo(state->chunkList)->nextChunk;

#if ASSERT
  /* clear out memory to quickly catch some memory safety errors */
  void* start = HM_getChunkStart(chunk);
  size_t length = ((size_t)(HM_getChunkInfo(chunk)->limit)) - ((size_t)(start));
  memset(start, 0xBF, length);
#endif

  return chunk;
}

struct HM_ChunkInfo* HM_getChunkInfo(void* chunk) {
  return ((struct HM_ChunkInfo*)(chunk));
}

const struct HM_ChunkInfo* HM_getChunkInfoConst(const void* chunk) {
  return ((const struct HM_ChunkInfo*)(chunk));
}

struct HM_ChunkInfo *HM_getChunkHeadChunk(struct HM_ChunkInfo *ci) {
    assert (ci);

    if (ci->level == CHUNK_INVALID_LEVEL) {
        ci = HM_getChunkInfo(ci->split.normal.levelHead);
    }
    assert(ci->level != CHUNK_INVALID_LEVEL);

    return ci;
}

struct HM_ChunkInfo *HM_getObjptrLevelHeadChunk(GC_state s, objptr object) {
    struct HM_ObjptrInfo objInfo;
    HM_getObjptrInfo(s, object, &objInfo);
    return HM_getChunkHeadChunk(HM_getChunkInfo(objInfo.chunkList));
}

struct HM_HierarchicalHeap *HM_getObjptrHH(GC_state s, objptr object) {
  struct HM_ObjptrInfo objInfo;
  HM_getObjptrInfo(s, object, &objInfo);
  return objInfo.hh;
}

rwlock_t *HM_getObjptrHHLock(GC_state s, objptr object) {
  return &HM_getObjptrHH(s, object)->lock;
}

bool HM_isObjptrInToSpace(GC_state s, objptr object) {
  /* return HM_getObjptrLevelHeadChunk(s, object)->split.levelHead.isInToSpace; */
  return HM_getObjptrLevelHeadChunk(s, object)->split.levelHead.containingHH
    == COPY_OBJECT_HH_VALUE;
}

#if ASSERT
const void* getLevelHeadChunk(const void* chunk) {
  const void* cursor;
  for (cursor = chunk;
       (NULL != cursor) &&
                CHUNK_INVALID_LEVEL == HM_getChunkInfoConst(cursor)->level;
       cursor = HM_getChunkInfoConst(cursor)->split.normal.levelHead) {
  }
  assert(NULL != cursor);

  return cursor;
}
#endif

/* Copyright (C) 2018 Sam Westrick
 * Copyright (C) 2015 Ram Raghunathan.
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
  HM_chunk* levelList;
  HM_chunk chunkList;
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
static void appendChunkList(HM_chunk destinationChunkList,
                            HM_chunk chunkList,
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
                                     const struct HM_HierarchicalHeap* hh,
                                     HM_chunk levelHeadChunk);
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
static void HM_assertChunkListInvariants(HM_chunk chunkList,
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
static HM_chunk getLevelHeadChunk(HM_chunk chunk);
#endif

static inline HM_chunk chunkOf(pointer p) {
  HM_chunk chunk = (HM_chunk)blockOf(p);
  assert(chunk->magic == CHUNK_MAGIC);
  return chunk;
}

/************************/
/* Function Definitions */
/************************/
#if (defined (MLTON_GC_INTERNAL_FUNCS))
HM_chunk HM_allocateChunk(HM_chunk levelHeadChunk, size_t bytesRequested) {
  size_t totalSize = bytesRequested + sizeof(struct HM_chunk);
  HM_chunk chunk = (HM_chunk)GC_getBlocks(pthread_getspecific(gcstate_key), &totalSize);

  if (NULL == chunk) {
    return NULL;
  }

#if ASSERT
  /* clear out memory to quickly catch some memory safety errors */
  pointer start = HM_getChunkStart(chunk);
  size_t length = ((size_t)chunk) + totalSize - ((size_t)start);
  memset(start, 0xAE, length);
#endif

#if ASSERT
  chunk->split.levelHead.nextHead = ((HM_chunk)(0xcafebabedeadbeef));
  chunk->split.levelHead.lastChunk = ((HM_chunk)(0xcafebabedeadbeef));
  chunk->split.levelHead.containingHH = ((struct HM_HierarchicalHeap*)(0xcafebabedeadbeef));
  chunk->split.levelHead.toChunkList = ((HM_chunk)(0xcafebabedeadbeef));
#endif

  chunk->magic = CHUNK_MAGIC;
  chunk->frontier = HM_getChunkStart(chunk);
  chunk->limit = (pointer)chunk + totalSize;
  chunk->level = CHUNK_INVALID_LEVEL;
  chunk->split.normal.levelHead = levelHeadChunk;

  /* assert that it is initialized correctly */
  assert(chunk->limit != (pointer)chunk);
  assert(totalSize == HM_getChunkSize(chunk));

  /* insert into list and update levelHeadChunk */
  chunk->nextChunk = NULL;
  if (NULL == levelHeadChunk->nextChunk) {
    /* empty list */
    levelHeadChunk->nextChunk = chunk;
    levelHeadChunk->split.levelHead.lastChunk = chunk;
  } else {
    levelHeadChunk->split.levelHead.lastChunk->nextChunk = chunk;
    levelHeadChunk->split.levelHead.lastChunk = chunk;
  }
  levelHeadChunk->split.levelHead.size += totalSize;

  LOG(LM_CHUNK, LL_DEBUG,
      "Allocate chunk %p at level %u",
      (void*)chunk,
      levelHeadChunk->level);

  return chunk;
}

HM_chunk HM_allocateLevelHeadChunk(HM_chunk* levelList,
                                   size_t allocableSize,
                                   Word32 level,
                                   struct HM_HierarchicalHeap* hh) {
  size_t totalSize = allocableSize + sizeof(struct HM_chunk);
  HM_chunk chunk = (HM_chunk)GC_getBlocks(pthread_getspecific(gcstate_key), &totalSize);

  if (NULL == chunk) {
    return NULL;
  }

#if ASSERT
  /* clear out memory to quickly catch some memory safety errors */
  pointer start = HM_getChunkStart(chunk);
  size_t length = ((size_t)chunk) + totalSize - ((size_t)start);
  memset(start, 0xAE, length);
#endif

  /* setup chunk info */
  chunk->magic = CHUNK_MAGIC;
  chunk->frontier = HM_getChunkStart(chunk);
  chunk->limit = (pointer)chunk + totalSize;
  chunk->nextChunk = NULL;
  chunk->level = level;
  chunk->split.levelHead.nextHead = NULL;
  chunk->split.levelHead.lastChunk = chunk;
  chunk->split.levelHead.containingHH = hh;
  chunk->split.levelHead.toChunkList = NULL;
  chunk->split.levelHead.size = totalSize;
  chunk->split.levelHead.isInToSpace = (hh == COPY_OBJECT_HH_VALUE);

  /* assert that it is initialized correctly */
  assert(chunk->limit != (pointer)chunk);
  assert(totalSize == HM_getChunkSize(chunk));

  /* insert into level list */
  HM_mergeLevelList(levelList,
                    chunk,
                    hh,
                    false);

  LOG(LM_CHUNK, LL_DEBUG,
      "Allocate chunk %p at level %u",
      (void*)chunk,
      level);

  return chunk;
}

void HM_forwardHHObjptrsInChunkList(
  GC_state s,
  pointer start,
  ObjptrPredicateFunction predicate,
  void* predicateArgs,
  struct ForwardHHObjptrArgs* forwardHHObjptrArgs)
{
  HM_chunk chunk = chunkOf(start);

  pointer p = start;
  size_t i = 0;

  if (chunk == NULL) {
    DIE("could not find chunk of %p", (void*)chunk);
  }

  while (NULL != chunk) {
  // for (;
  //      NULL != chunk;
  //      chunk = chunk->nextChunk, p = HM_getChunkStart(chunk)) {

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
    p = HM_getChunkStart(chunk); // just a pointer addition; safe even if chunk == NULL
  }
}

void HM_forwardHHObjptrsInLevelList(
  GC_state s,
  HM_chunk* levelList,
  ObjptrPredicateFunction predicate,
  void* predicateArgs,
  struct ForwardHHObjptrArgs* forwardHHObjptrArgs,
  bool expectEntanglement)
{
  Word32 savedMaxLevel = forwardHHObjptrArgs->maxLevel;
  forwardHHObjptrArgs->maxLevel = 0;

  for (HM_chunk levelHead = *levelList;
       NULL != levelHead;
       levelHead = levelHead->split.levelHead.nextHead) {
    LOCAL_USED_FOR_ASSERT void* savedLevelList = *levelList;

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
      HM_getChunkStart(levelHead),
      predicate,
      predicateArgs,
      forwardHHObjptrArgs);

    /* asserts that no new lower level has been created */
    assert(savedLevelList == *levelList);
  }

  forwardHHObjptrArgs->maxLevel = savedMaxLevel;
}

void HM_freeChunks(HM_chunk* levelList, Word32 minLevel) {
  struct FreeLevelListIteratorArgs iteratorArgs = {
    .levelList = levelList,
    .chunkList = NULL,
    .minLevel = minLevel
  };
  LOG(LM_CHUNK, LL_DEBUGMORE,
      "START FreeChunks levelList = %p, minLevel = %u",
      ((void*)(iteratorArgs.levelList)),
      iteratorArgs.minLevel);
  for (HM_chunk chunk = HM_freeLevelListIterator(&iteratorArgs);
       NULL != chunk;
       chunk = HM_freeLevelListIterator(&iteratorArgs));
  LOG(LM_CHUNK, LL_DEBUGMORE,
      "END FreeChunks levelList = %p, minLevel = %u",
      (void*)iteratorArgs.levelList,
      iteratorArgs.minLevel);
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

Word32 HM_getChunkListLevel(HM_chunk levelHead) {
  assert(CHUNK_INVALID_LEVEL != levelHead->level);
  return levelHead->level;
}

HM_chunk HM_getChunkListLastChunk(HM_chunk levelHead) {
  if (NULL == levelHead) {
    return NULL;
  }

  assert(CHUNK_INVALID_LEVEL != levelHead->level);
  return levelHead->split.levelHead.lastChunk;
}

HM_chunk HM_getChunkListToChunkList(HM_chunk levelHead) {
  assert(NULL != levelHead);
  assert(CHUNK_INVALID_LEVEL != levelHead->level);

  return levelHead->split.levelHead.toChunkList;
}

/* SAM_NOTE: TODO: presumably looking up the level list with a linear search is
 * not very expensive since the number of levels in well-behaved parallel
 * programs is small. That being said... can't we store them in a dynamically-
 * sized array? */
Word64 HM_getLevelSize(HM_chunk levelList, Word32 level) {
  // for (cursor = levelList;
  //      (cursor != NULL) && (HM_getChunkInfo(cursor)->level > level);
  //      cursor = HM_getChunkInfo(cursor)->split.levelHead.nextHead) { }
  HM_chunk cursor = levelList;
  while (cursor != NULL && cursor->level > level) {
    cursor = cursor->split.levelHead.nextHead;
  }

  if ((NULL == cursor) || cursor->level != level) {
    return 0;
  }

  return cursor->split.levelHead.size;
}

void HM_setChunkListToChunkList(HM_chunk levelHead, HM_chunk toChunkList) {
  assert(NULL != levelHead);
  assert(CHUNK_INVALID_LEVEL != levelHead->level);

  levelHead->split.levelHead.toChunkList = toChunkList;
  LOG(LM_CHUNK, LL_DEBUGMORE,
      "Set toChunkList of chunk %p to %p",
      (void*)levelHead,
      (void*)toChunkList);
}

void HM_getObjptrInfo(GC_state s,
                      objptr object,
                      struct HM_ObjptrInfo* info) {
  assert(HM_HH_objptrInHierarchicalHeap(s, object));

  HM_chunk chunk = chunkOf(objptrToPointer(object, s->heap->start));
  assert(NULL != chunk);

  HM_chunk chunkList = chunk;
  while (chunkList != NULL && chunkList->level == CHUNK_INVALID_LEVEL) {
    chunkList = chunkList->split.normal.levelHead;
  }
  // for(chunkList = chunk;
  //     (NULL != chunkList) &&
  //                 (CHUNK_INVALID_LEVEL == HM_getChunkInfo(chunkList)->level);
  //     chunkList = HM_getChunkInfo(chunkList)->split.normal.levelHead) { }

  if (NULL == chunkList) {
    DIE("Couldn't get objptrinfo for %p",
        ((void*)(object)));
  }

  /* now that I have the chunkList, path compress */
  void* parentChunk = NULL;
  while (chunkList != chunk) {
    assert(CHUNK_INVALID_LEVEL == chunk->level);

    parentChunk = chunk->split.normal.levelHead;
    chunk->split.normal.levelHead = chunkList;
    chunk = parentChunk;
  }

  info->hh = chunkList->split.levelHead.containingHH;
  info->chunkList = chunkList;
  info->level = chunkList->level;
}

Word32 HM_getHighestLevel(HM_chunk levelHead) {
  if (NULL == levelHead) {
    return CHUNK_INVALID_LEVEL;
  }

  ASSERTPRINT(CHUNK_INVALID_LEVEL != levelHead->level,
              "Chunk %p is not a level head chunk!",
              (void*)levelHead);
  return levelHead->level;
}

void HM_mergeLevelList(
  HM_chunk* destinationLevelList,
  HM_chunk levelList,
  struct HM_HierarchicalHeap * const hh,
  bool resetToFromSpace)
{
  LOG(LM_CHUNK, LL_DEBUG,
      "Merging %p into %p",
      ((void*)(levelList)),
      ((void*)(*destinationLevelList)));

  HM_chunk newLevelList = NULL;

  /* construct newLevelList */
  {
    HM_chunk* previousChunkList = &newLevelList;
    HM_chunk cursor1 = *destinationLevelList;
    HM_chunk cursor2 = levelList;
    while ((NULL != cursor1) && (NULL != cursor2)) {
      size_t level1 = cursor1->level;
      size_t level2 = cursor2->level;
      assert(CHUNK_INVALID_LEVEL != level1);
      assert(CHUNK_INVALID_LEVEL != level2);

      if (level1 > level2) {
        /* append the first list */
        *previousChunkList = cursor1;

        /* advance cursor1 */
        cursor1 = cursor1->split.levelHead.nextHead;
      } else if (level1 < level2) {
        /* append the second list */
        *previousChunkList = cursor2;

        /* advance cursor2 */
        cursor2 = cursor2->split.levelHead.nextHead;
      } else {
        /* level1 == level2 */
        /* advance cursor 2 early since appendChunkList will unlink it */
        void* savedCursor2 = cursor2;
        cursor2 = cursor2->split.levelHead.nextHead;

        /* merge second list into first before inserting */
        appendChunkList(cursor1, savedCursor2, 0xcafed00dbaadf00d);

        /* append the first list */
        *previousChunkList = cursor1;

        /* advance cursor1 */
        cursor1 = cursor1->split.levelHead.nextHead;
      }

      /* set HH of this chunk list */
      (*previousChunkList)->split.levelHead.containingHH = hh;

      /* advance previousChunkList */
      previousChunkList =
          &((*previousChunkList)->split.levelHead.nextHead);
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
    for (HM_chunk chunkList = *previousChunkList;
         NULL != chunkList;
         chunkList = chunkList->split.levelHead.nextHead) {
      chunkList->split.levelHead.containingHH = hh;
    }
  }

  /* mark every chunk as in from-space since they have been merged */
  if (resetToFromSpace) {
    for (HM_chunk chunk = newLevelList;
         chunk != NULL;
         chunk = chunk->split.levelHead.nextHead) {
      chunk->split.levelHead.isInToSpace = false;
    }
  }

#if ASSERT
  if (newLevelList) {
    bool toSpace = newLevelList->split.levelHead.containingHH
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

void HM_promoteChunks(HM_chunk* levelList, size_t level) {
  LOG(LM_CHUNK, LL_DEBUG,
      "Promoting level %zu in level list %p",
      level,
      ((void*)(*levelList)));

  const struct HM_HierarchicalHeap* hh =
    (*levelList)->split.levelHead.containingHH;

  HM_assertLevelListInvariants(*levelList, hh, HM_HH_INVALID_LEVEL, false);

  /* find the pointer to level list of level 'level' */
  HM_chunk* cursor;
  for (cursor = levelList;

#if ASSERT
       (NULL != *cursor) &&
#endif
                ((*cursor)->level > level);

       cursor = &((*cursor)->split.levelHead.nextHead)) {
    assert(CHUNK_INVALID_LEVEL != (*cursor)->level);
  }
  assert(CHUNK_INVALID_LEVEL != (*cursor)->level);

  assert(NULL != *cursor);
  if ((*cursor)->level < level) {
    /* no chunks to promote */
    HM_assertLevelListInvariants(*levelList, hh, HM_HH_INVALID_LEVEL, false);
    return;
  }

  HM_chunk chunkList = *cursor;
  /* unlink level list */
  *cursor = chunkList->split.levelHead.nextHead;

  if ((NULL != *cursor) && (level - 1 == (*cursor)->level)) {
    /* need to merge into cursor */
    appendChunkList(*cursor, chunkList, 0xcafed00dbaadd00d);
  } else {
    /* need to reassign levelList to level - 1 */
    assert((NULL == *cursor) || (level - 1 > (*cursor)->level));
    chunkList->level = level - 1;

    /* insert chunkList where *cursor is */
    chunkList->split.levelHead.nextHead = *cursor;
    *cursor = chunkList;
  }

  HM_assertLevelListInvariants(*levelList, hh, HM_HH_INVALID_LEVEL, false);
}

#if ASSERT
void HM_assertChunkInLevelList(HM_chunk levelList, HM_chunk chunk) {
  for (HM_chunk chunkList = levelList;
       NULL != chunkList;
       chunkList = chunkList->split.levelHead.nextHead) {
    for (HM_chunk cursor = chunkList;
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

void HM_assertLevelListInvariants(HM_chunk levelList,
                                  const struct HM_HierarchicalHeap* hh,
                                  Word32 stealLevel,
                                  bool inToSpace) {
  Word32 previousLevel = ~((Word32)(0));
  for (HM_chunk chunkList = levelList;
       NULL != chunkList;
       chunkList = chunkList->split.levelHead.nextHead) {
    Word32 level = chunkList->level;
    struct HM_HierarchicalHeap* levelListHH =
      chunkList->split.levelHead.containingHH;

    assert(chunkList->split.levelHead.isInToSpace == inToSpace);

    assert(CHUNK_INVALID_LEVEL != level);
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
void HM_assertChunkInLevelList(HM_chunk levelList, HM_chunk chunk) {
  ((void)(levelList));
  ((void)(chunk));
}

void HM_assertLevelListInvariants(HM_chunk levelList,
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

void HM_updateLevelListPointers(HM_chunk levelList,
                                struct HM_HierarchicalHeap* hh) {
  for (HM_chunk cursor = levelList;
       NULL != cursor;
       cursor = cursor->split.levelHead.nextHead) {
    cursor->split.levelHead.containingHH = hh;
  }
}
#endif /* MLTON_GC_INTERNAL_FUNCS */

void appendChunkList(HM_chunk destinationChunkList,
                     HM_chunk chunkList,
                     ARG_USED_FOR_ASSERT size_t sentinel) {
  LOG(LM_CHUNK, LL_DEBUGMORE,
      "Appending %p into %p",
      ((void*)(chunkList)),
      ((void*)(destinationChunkList)));

  assert (NULL != destinationChunkList);
  assert(CHUNK_INVALID_LEVEL != destinationChunkList->level);
  assert(CHUNK_INVALID_LEVEL != chunkList->level);

  if (NULL == chunkList) {
    /* nothing to append */
    return;
  }

  /* append list */
  HM_chunk lastDestinationChunk = HM_getChunkListLastChunk(destinationChunkList);
  assert(NULL == lastDestinationChunk->nextChunk);
  lastDestinationChunk->nextChunk = chunkList;

  /* update level head chunk */
  HM_chunk lastChunk = HM_getChunkListLastChunk(chunkList);
  destinationChunkList->split.levelHead.lastChunk = lastChunk;
  destinationChunkList->split.levelHead.size +=
    chunkList->split.levelHead.size;

  /* demote chunkList's level head chunk */
#if ASSERT
  chunkList->split.levelHead.nextHead = ((void*)(sentinel));
  chunkList->split.levelHead.lastChunk = ((void*)(sentinel));
  chunkList->split.levelHead.containingHH = ((struct HM_HierarchicalHeap*)(sentinel));
  chunkList->split.levelHead.toChunkList = ((void*)(sentinel));
#endif

  chunkList->level = CHUNK_INVALID_LEVEL;
  chunkList->split.normal.levelHead = destinationChunkList;

  HM_assertChunkListInvariants(destinationChunkList,
                               destinationChunkList->split.levelHead.containingHH);
}

#if ASSERT
void HM_assertChunkInvariants(HM_chunk chunk,
                              const struct HM_HierarchicalHeap* hh,
                              HM_chunk levelHeadChunk) {
  assert(HM_getChunkStart(chunk) <= chunk->frontier && chunk->frontier <= chunk->limit);

  if (chunk == levelHeadChunk) {
    /* this is the level head chunk */
    assert(CHUNK_INVALID_LEVEL != chunk->level);
    assert(hh == chunk->split.levelHead.containingHH);
  } else {
    /* this is a normal chunk */
    assert(CHUNK_INVALID_LEVEL == chunk->level);
  }

  assert(levelHeadChunk == getLevelHeadChunk(chunk));
}

void HM_assertChunkListInvariants(HM_chunk chunkList,
                                  const struct HM_HierarchicalHeap* hh) {
  Word64 size = 0;
  for (HM_chunk chunk = chunkList;
       NULL != chunk;
       chunk = chunk->nextChunk) {
    HM_assertChunkInvariants(chunk, hh, chunkList);
    size += HM_getChunkSize(chunk);
  }

  assert(chunkList->split.levelHead.size == size);
}
#else
void HM_assertChunkListInvariants(HM_chunk chunkList,
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
        (state->chunkList->level < state->minLevel)) {
      /* all done */
      return NULL;
    }

    /* we should be at a levelHead */
    assert(state->chunkList->level != CHUNK_INVALID_LEVEL);

    /* this chunk list will be freed, so unlink and advance level list */
    *(state->levelList) = state->chunkList->split.levelHead.nextHead;

    LOG(LM_CHUNK, LL_DEBUG,
        "Freeing chunk list at level %u %u",
        state->chunkList->level,
        state->minLevel);
  }

  HM_chunk chunk = state->chunkList;

  /* advance chunkList */
  state->chunkList = state->chunkList->nextChunk;

#if ASSERT
  /* clear out memory to quickly catch some memory safety errors */
  void* start = HM_getChunkStart(chunk);
  size_t length = ((size_t)(chunk->limit)) - ((size_t)(start));
  memset(start, 0xBF, length);
#endif

  return chunk;
}

// HM_chunk HM_getChunkInfo(void* chunk) {
//   return ((HM_chunk)(chunk));
// }

// const HM_chunk HM_getChunkInfoConst(const void* chunk) {
//   return ((const HM_chunk)(chunk));
// }

HM_chunk HM_getChunkHeadChunk(HM_chunk ci) {
  assert (ci);

  if (ci->level == CHUNK_INVALID_LEVEL) {
    ci = ci->split.normal.levelHead;
  }
  assert(ci->level != CHUNK_INVALID_LEVEL);

  return ci;
}

HM_chunk HM_getObjptrLevelHeadChunk(GC_state s, objptr object) {
  struct HM_ObjptrInfo objInfo;
  HM_getObjptrInfo(s, object, &objInfo);
  return HM_getChunkHeadChunk(objInfo.chunkList);
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
HM_chunk getLevelHeadChunk(HM_chunk chunk) {
  HM_chunk cursor;
  for (cursor = chunk;
       (NULL != cursor) &&
                CHUNK_INVALID_LEVEL == cursor->level;
       cursor = cursor->split.normal.levelHead) {
  }
  assert(NULL != cursor);

  return cursor;
}
#endif

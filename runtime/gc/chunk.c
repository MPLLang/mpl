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
 * <emph>not</emph> in a level list
 */
static void appendChunkList(void* destinationChunkList, void* chunkList);

#if ASSERT
/**
 * This function asserts the chunk invariants
 *
 * @attention
 * If an assertion fails, this function aborts the program, as per the assert()
 * macro.
 *
 * @param chunk The chunk to assert invariants for.
 * @param levelHeadChunk The head chunk of the level 'chunk' belongs to
 */
static void HM_assertChunkInvariants(const void* chunk,
                                     const void* levelHeadChunk);

/**
 * This function asserts the chunk list invariants
 *
 * @attention
 * If an assertion fails, this function aborts the program, as per the assert()
 * macro.
 *
 * @param chunkList The chunk list to assert invariants for.
 */
static void HM_assertChunkListInvariants(const void* chunkList);
#endif /* ASSERT */

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

/**
 * Gets the level's head chunk for a given chunk.
 *
 * @param chunk The chunk to get the level head chunk for
 *
 * @return the head chunk of the level 'chunk' belongs to
 */
static void* getLevelHeadChunk(void* chunk);

/**
 * Same as getLevelHeadChunk() except for const-correctness. See
 * getLevelHeadChunk() for more details.
 */
static const void* getLevelHeadChunkConst(const void* chunk);

/************************/
/* Function Definitions */
/************************/
#if (defined (MLTON_GC_INTERNAL_FUNCS))
void* HM_allocateChunk(size_t allocableSize, void* levelHeadChunk) {
  size_t totalSize = allocableSize + sizeof(struct HM_ChunkInfo);
  void* chunk = ChunkPool_allocate(&totalSize);

  if (NULL == chunk) {
    return NULL;
  }

  struct HM_ChunkInfo* chunkInfo = getChunkInfo(chunk);
  chunkInfo->chunkEnd = ((void*)(((char*)(chunk)) + totalSize));
  chunkInfo->level = CHUNK_INVALID_LEVEL;
  chunkInfo->levelInfo.headChunk = levelHeadChunk;

  /* insert into list */
  chunkInfo->nextChunk = getChunkInfo(levelHeadChunk)->nextChunk;
  getChunkInfo(levelHeadChunk)->nextChunk = chunk;

  return chunk;
}

void* HM_allocateLevelHeadChunk(size_t allocableSize,
                                size_t level,
                                void** levelList) {
  size_t totalSize = allocableSize + sizeof(struct HM_ChunkInfo);
  void* chunk = ChunkPool_allocate(&totalSize);

  if (NULL == chunk) {
    return NULL;
  }

  /* setup chunk info */
  struct HM_ChunkInfo* chunkInfo = getChunkInfo(chunk);
  chunkInfo->chunkEnd = ((void*)(((char*)(chunk)) + totalSize));
  chunkInfo->nextChunk = NULL;
  chunkInfo->level = level;

  /* insert into level list */
  chunkInfo->levelInfo.nextLevelHeadChunk = *levelList;
  *levelList = chunk;

  return chunk;
}

void* HM_getChunkEnd(void* chunk) {
  return getChunkInfo(chunk)->chunkEnd;
}

void* HM_getChunkStart(void* chunk) {
  return ((void*)(((char*)(chunk)) + sizeof(struct HM_ChunkInfo)));
}

void* HM_getChunkListLastChunk(void* chunkList) {
  if (NULL == chunkList) {
    return NULL;
  }

  void* chunk;
  for (chunk = chunkList;
       NULL != getChunkInfo(chunk)->nextChunk;
       chunk = getChunkInfo(chunk)->nextChunk) {
  }

  return chunk;
}

size_t HM_getHighestLevel(const void* levelList) {
  if (NULL == levelList) {
    return CHUNK_INVALID_LEVEL;
  }

  assert(CHUNK_INVALID_LEVEL != getChunkInfoConst(levelList)->level);
  return (getChunkInfoConst(levelList)->level);
}

void HM_mergeLevelList(void** destinationLevelList, void* levelList) {
  void* newLevelList = NULL;

  /* construct newLevelList */
  {
    void** previousChunkList = &newLevelList;
    void* cursor1 = *destinationLevelList;
    void* cursor2 = levelList;
    for (;(NULL != cursor1) && (NULL != cursor2);) {
      size_t level1 = getChunkInfo(cursor1)->level;
      size_t level2 = getChunkInfo(cursor2)->level;
      assert(CHUNK_INVALID_LEVEL != level1);
      assert(CHUNK_INVALID_LEVEL != level2);

      if (level1 > level2) {
        /* append the first list */
        *previousChunkList = cursor1;

        /* advance cursor1 */
        cursor1 = getChunkInfo(cursor1)->levelInfo.nextLevelHeadChunk;
      } else if (level1 < level2) {
        /* append the second list */
        *previousChunkList = cursor2;

        /* advance cursor2 */
        cursor2 = getChunkInfo(cursor2)->levelInfo.nextLevelHeadChunk;
      } else {
        /* level1 == level2 */
        /* advance cursor 2 early since appendChunkList will unlink it */
        void* savedCursor2 = cursor2;
        cursor2 = getChunkInfo(cursor2)->levelInfo.nextLevelHeadChunk;

        /* merge second list into first before inserting */
        appendChunkList(cursor1, savedCursor2);

        /* append the first list */
        *previousChunkList = cursor1;

        /* advance cursor1 */
        cursor1 = getChunkInfo(cursor1)->levelInfo.nextLevelHeadChunk;
      }

      /* advance previousChunkList */
      previousChunkList =
          &(getChunkInfo(*previousChunkList)->levelInfo.nextLevelHeadChunk);
    }

    if (NULL != cursor1) {
      assert(NULL == cursor2);

      /* append the remainder of cursor1 */
      *previousChunkList = cursor1;
    } else if (NULL != cursor2) {
      assert(NULL == cursor2);

      /* append the remainder of cursor2 */
      *previousChunkList = cursor2;
    }
  }

  /* update destinationChunkList */
  *destinationLevelList = newLevelList;
}

void HM_promoteChunks(void** levelList, size_t level) {
  /* find the level + 1 list */
  void** cursor;
  for (cursor = levelList;

#if ASSERT
       (NULL != *cursor) &&
#endif
                (getChunkInfo(*cursor)->level > level);

       cursor = &(getChunkInfo(*cursor)->levelInfo.nextLevelHeadChunk)) {
    assert(CHUNK_INVALID_LEVEL != getChunkInfo(*cursor)->level);
  }

  assert(NULL != *cursor);
  if (getChunkInfo(*cursor)->level < level) {
    /* no chunks to promote */
    return;
  }

  void* chunkList = *cursor;
  /* unlink level list */
  *cursor = getChunkInfo(chunkList)->levelInfo.nextLevelHeadChunk;

  if ((NULL != *cursor) && (level - 1 == getChunkInfo(*cursor)->level)) {
    /* need to merge into cursor */
    appendChunkList(*cursor, chunkList);
  } else {
    /* need to reassign levelList to level - 1 */
    assert((NULL == *cursor) || (level - 1 > getChunkInfo(*cursor)->level));
    getChunkInfo(chunkList)->level = level - 1;

    /* insert chunkList where *cursor is */
    getChunkInfo(chunkList)->levelInfo.nextLevelHeadChunk = *cursor;
    *cursor = chunkList;
  }
}

#if ASSERT
void HM_assertLevelListInvariants(const void* levelList) {
  size_t lastLevel = ~((size_t)(0LL));
  for (const void* chunkList = levelList;
       NULL != chunkList;
       chunkList = getChunkInfoConst(chunkList)->levelInfo.nextLevelHeadChunk) {
    size_t level = getChunkInfoConst(chunkList)->level;
    assert(CHUNK_INVALID_LEVEL != level);
    assert(level < lastLevel);
    lastLevel = level;

    HM_assertChunkListInvariants(chunkList);
  }
}
#else
void HM_assertLevelListInvariants(const void* levelList) {
  ((void)(levelList));
}
#endif /* ASSERT */
#endif /* MLTON_GC_INTERNAL_FUNCS */

void appendChunkList(void* destinationChunkList, void* chunkList) {
  assert (NULL != destinationChunkList);

  if (NULL == chunkList) {
    /* nothing to append */
    return;
  }

  assert(CHUNK_INVALID_LEVEL != getChunkInfo(destinationChunkList)->level);

  /* change level */
  getChunkInfo(chunkList)->level = CHUNK_INVALID_LEVEL;
  getChunkInfo(chunkList)->levelInfo.headChunk = destinationChunkList;

  /* append list */
  void* lastChunk = HM_getChunkListLastChunk(chunkList);
  getChunkInfo(lastChunk)->nextChunk =
      getChunkInfo(destinationChunkList)->nextChunk;
  getChunkInfo(destinationChunkList)->nextChunk = chunkList;
}

#if ASSERT
void HM_assertChunkInvariants(const void* chunk, const void* levelHeadChunk) {
  const struct HM_ChunkInfo* chunkInfo = getChunkInfoConst(chunk);
  assert(levelHeadChunk == getLevelHeadChunkConst(chunk));
  assert(ChunkPool_find(((char*)(chunkInfo->chunkEnd)) - 1) == chunk);
}

void HM_assertChunkListInvariants(const void* chunkList) {
  for (const void* chunk = chunkList;
       NULL != chunk;
       chunk = getChunkInfoConst(chunk)->nextChunk) {
    HM_assertChunkInvariants(chunk, chunkList);
  }
}
#endif /* ASSERT */

struct HM_ChunkInfo* getChunkInfo(void* chunk) {
  return ((struct HM_ChunkInfo*)(chunk));
}

const struct HM_ChunkInfo* getChunkInfoConst(const void* chunk) {
  return ((const struct HM_ChunkInfo*)(chunk));
}

void* getLevelHeadChunk(void* chunk) {
  void* cursor;
  for (cursor = chunk;
#if ASSERT
       (NULL != cursor) &&
#endif
                CHUNK_INVALID_LEVEL == getChunkInfo(chunk)->level;
       chunk = getChunkInfo(chunk)->levelInfo.headChunk) {
  }
  assert(NULL != cursor);

  return cursor;
}

const void* getLevelHeadChunkConst(const void* chunk) {
  const void* cursor;
  for (cursor = chunk;
#if ASSERT
       (NULL != cursor) &&
#endif
                CHUNK_INVALID_LEVEL == getChunkInfoConst(cursor)->level;
       cursor = getChunkInfoConst(cursor)->levelInfo.headChunk) {
  }
  assert(NULL != cursor);

  return cursor;
}

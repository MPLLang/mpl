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
 * This function retrieves the ChunkInfo object from a chunk
 *
 * @param chunk The chunk to retrieve the object from
 *
 * @return the ChunkInfo struct pointer
 */
static struct HM_ChunkInfo* getChunkInfo(void* chunk);

#if ASSERT
/**
 * Same as getChunkInfo() except for const-correctness. See getChunkInfo() for
 * more details
 */
static const struct HM_ChunkInfo* getConstChunkInfo(const void* chunk);
#endif /* ASSERT */

/************************/
/* Function Definitions */
/************************/
#if (defined (MLTON_GC_INTERNAL_FUNCS))
void* HM_allocateChunk(size_t allocableSize) {
  size_t totalSize = allocableSize + sizeof(struct HM_ChunkInfo);
  void* chunk = ChunkPool_allocate(&totalSize);

  if (NULL == chunk) {
    return NULL;
  }

  struct HM_ChunkInfo* chunkInfo = getChunkInfo(chunk);
  chunkInfo->chunkEnd = ((void*)(((char*)(chunk)) + totalSize));
  chunkInfo->nextChunk = NULL;

  return chunk;
}

void HM_appendChunkList(void** destinationChunkList,
                        void* chunkList,
                        void* lastChunk) {
  if (NULL == chunkList) {
    /* nothing to append */
    return;
  } else if (NULL == *destinationChunkList) {
    /*
     * nothing to append to, so just rename. While the code in else branch will
     * still work, this is a shortcut to prevent having to find lastChunk
     */
    *destinationChunkList = chunkList;
    return;
  }

  /* else, I have two lists to append */

  assert(NULL != lastChunk);
  assert(NULL == getChunkInfo(lastChunk)->nextChunk);

  /* link last chunk to the first chunk in the destination list */
  getChunkInfo(lastChunk)->nextChunk = *destinationChunkList;

  /* set 'chunkList' as the new start of the destination chunk list */
  *destinationChunkList = chunkList;
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
#endif /* MLTON_GC_INTERNAL_FUNCS */

#if ASSERT
void HM_assertChunkInvariants(const void* chunk) {
  const struct HM_ChunkInfo* chunkInfo = getConstChunkInfo(chunk);
  assert(ChunkPool_find(((char*)(chunkInfo->chunkEnd)) - 1) == chunk);
}

void HM_assertChunkListInvariants(const void* chunkList) {
  for (const void* chunk = chunkList;
       NULL != chunk;
       chunk = getConstChunkInfo(chunk)->nextChunk) {
    HM_assertChunkInvariants(chunk);
  }
}
#endif /* ASSERT */

struct HM_ChunkInfo* getChunkInfo(void* chunk) {
  return ((struct HM_ChunkInfo*)(chunk));
}

#if ASSERT
const struct HM_ChunkInfo* getConstChunkInfo(const void* chunk) {
  return ((const struct HM_ChunkInfo*)(chunk));
}
#endif /* ASSERT */

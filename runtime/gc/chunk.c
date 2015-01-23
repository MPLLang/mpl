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

/************************/
/* Function Definitions */
/************************/
#if (defined (MLTON_GC_INTERNAL_FUNCS))
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

void* HM_getLastChunk(void* chunkList) {
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
  struct HM_ChunkInfo* chunkInfo = getChunkInfo(chunk);
  assert(ChunkPool_find(((char*)(chunkInfo->chunkEnd)) - 1) == chunk);
}

void HM_assertChunkListInvariants(const void* chunkList) {
  for (void* chunk = chunkList;
       NULL != chunk;
       chunk = getChunkInfo(chunk)->nextChunk) {
    HM_assertChunkInvariants(chunk);
  }
}
#endif /* ASSERT */

struct HM_ChunkInfo* getChunkInfo(void* chunk) {
  return ((struct HM_ChunkInfo*)(chunk));
}

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
static struct ChunkInfo* getChunkInfo (void* chunk);

/************************/
/* Function Definitions */
/************************/
void HM_appendChunkList (void** destinationChunkList, void* chunkList) {
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

  /* find the last chunk in 'chunkList' */
  void* lastChunk;
  for (lastChunk = chunkList;
       NULL != getChunkInfo (lastChunk)->nextChunk;
       lastChunk = getChunkInfo (lastChunk)->nextChunk) {
  }

  assert (NULL == getChunkInfo (lastChunk)->nextChunk);

  /* link last chunk to the first chunk in the destination list */
  getChunkInfo (lastChunk)->nextChunk = *destinationChunkList;

  /* set 'chunkList' as the new start of the destination chunk list */
  *destinationChunkList = chunkList;
}

#if ASSERT
void HM_assertChunkInvariants(const void* chunk) {
  struct ChunkInfo* chunkInfo = getChunkInfo(chunk);
  assert(ChunkPool_find(((char*)(chunkInfo->chunkEnd)) - 1) == chunk);
}

void HM_assertChunkListInvariants(const void* chunkList) {
  for (void* chunk = chunkList;
       NULL != chunk;
       chunk = getChunkInfo(chunk)->nextChunk) {
    HM_assertChunkInvariants(chunk);
  }
}
#endif

struct ChunkInfo* getChunkInfo (void* chunk) {
  return ((struct ChunkInfo*)(chunk));
}

/* Copyright (C) 2015 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file chunk.h
 *
 * @author Ram Raghunathan
 *
 * @brief
 * Definition of the ChunkInfo object and management interface
 */

#ifndef CHUNK_H_
#define CHUNK_H_

/**
 * @brief
 * Represents the information about the chunk
 *
 * This packed object is placed at the beginning of a chunk for easy access
 */
struct ChunkInfo {
  void* chunkEnd; /**< The end of this chunk */
  void* nextChunk; /**< The next chunk in the heap's chunk list */
} __attribute__((packed));

/**
 * This function appends 'chunkList' to 'destinationChunkList'
 *
 * @param destinationChunkList The head of the chunk list to append to
 * @param chunkList the chunk list to append
 */
void HM_appendChunkList (void** destinationChunkList, void* chunkList);

#endif /* CHUNK_H_ */

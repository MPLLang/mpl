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
 * @param lastChunk the last chunk in the chunkList
 */
void HM_appendChunkList(void** destinationChunkList,
                        void* chunkList,
                        void* lastChunk);

/**
 * This function gets the last chunk in a list
 *
 * @param chunkList The list to get the last chunk of
 *
 * @return the last chunk, or NULL if the list is empty
 */
void* HM_getLastChunk(void* chunkList);

#if ASSERT
/**
 * This function asserts the chunk invariants
 *
 * @attention
 * If an assertion fails, this function aborts the program, as per the assert()
 * macro.
 *
 * @param chunk The chunk to assert invariants for.
 */
void HM_assertChunkInvariants(const void* chunk);

/**
 * This function asserts the chunk list invariants
 *
 * @attention
 * If an assertion fails, this function aborts the program, as per the assert()
 * macro.
 *
 * @param chunkList The chunk list to assert invariants for.
 */
void HM_assertChunkListInvariants(const void* chunkList);
#endif /* ASSERT */

#endif /* CHUNK_H_ */

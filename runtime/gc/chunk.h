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

#if (defined (MLTON_GC_INTERNAL_TYPES))
/**
 * @brief
 * Represents the information about the chunk
 *
 * This packed object is placed at the beginning of a chunk for easy access
 */
struct HM_ChunkInfo {
  void* chunkEnd; /**< The end of this chunk */
  void* nextChunk; /**< The next chunk in the heap's chunk list */
} __attribute__((packed));
#else
struct HM_ChunkInfo;
#endif /* MLTON_GC_INTERNAL_TYPES */

#if (defined (MLTON_GC_INTERNAL_FUNCS))
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
 * This function returns the end of the chunk, usually used as the heap limit
 *
 * @param chunk The chunk to use
 *
 * @return The end of the chunk
 */
void* HM_getChunkEnd(void* chunk);

/**
 * This function gets the last chunk in a list
 *
 * @param chunkList The list to get the last chunk of
 *
 * @return the last chunk, or NULL if the list is empty
 */
void* HM_getChunkListLastChunk(void* chunkList);
#endif /* MLTON_GC_INTERNAL_FUNCS */

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

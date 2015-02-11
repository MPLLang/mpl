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

/* RAM_NOTE: Remove when know unnecessary */
#if 0
/**
 * @brief
 * This pointer is used as an "invalid" chunk for when no chunk is set along
 * with its start and end.
 *
 * While NULL can be set, this leads to limit being set to NULL, A.K.A. 0, which
 * may interfere with the signal handler limit checks. Instead, we use
 * CHUNK_INVALID_POINTER for chunk, start, and end which allows for correct
 * limit checks.
 */
#define CHUNK_INVALID_POINTER ((void*)(0x1))
#endif

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
 * This function allocates and initializes a chunk of at least allocableSize
 * allocable bytes.
 *
 * @param allocableSize The minimum number of allocable bytes in the chunk
 *
 * @return NULL if no chunk could be allocated, or chunk pointer otherwise.
 */
void* HM_allocateChunk(size_t allocableSize);

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
 * This function returns the end of the chunk (pointer to byte after last
 * allocable byte), usually used as the heap limit
 *
 * @param chunk The chunk to use
 *
 * @return end of the chunk
 */
void* HM_getChunkEnd(void* chunk);

/**
 * This function returns the start of allocable area of the chunk (pointer to
 * the first byte that can be allocated), usually used as the initial heap
 * frontier.
 *
 * @param chunk The chunk to use
 *
 * @return start of the chunk
 */
void* HM_getChunkStart(void* chunk);

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

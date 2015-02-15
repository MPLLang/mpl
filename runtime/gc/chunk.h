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
 * CHUNK_INVALID_LEVEL denotes an invalid level value for HM_ChunkInfo::level.
 */
#define CHUNK_INVALID_LEVEL (~((size_t)(0LL)))

/**
 * @brief
 * Represents the information about the chunk
 *
 * This packed object is placed at the beginning of a chunk for easy access
 */
struct HM_ChunkInfo {
  void* chunkEnd; /**< The end of this chunk */

  void* nextChunk; /**< The next chunk in the heap's chunk list */

  size_t level; /**< The level of this chunk. If set to CHUNK_INVALID_LEVEL,
                 * this chunk is not a level head and the level can be found by
                 * following levelInfo::headChunk fields. */

  union {
    void* headChunk; /**< Linked list of chunks ending in the head chunk of the
                      * level this chunk belongs to. This field is set if level
                      * is set to CHUNK_INVALID_LEVEL */

    void* nextLevelHeadChunk; /**< The head chunk of the next level. This field
                               * is set if level is <emph>not</emph>
                               * CHUNK_INVALID_LEVEL */
  } levelInfo; /**< The union containing headChunk and nextLevelHeadChunk to
                * save space */
} __attribute__((packed));

COMPILE_TIME_ASSERT(HM_ChunkInfo__packed,
                    sizeof(struct HM_ChunkInfo) ==
                    sizeof(void*) +
                    sizeof(void*) +
                    sizeof(size_t) +
                    sizeof(void*));

COMPILE_TIME_ASSERT(HM_ChunkInfo__aligned,
                    (sizeof(struct HM_ChunkInfo) % 8) == 0);
#else
struct HM_ChunkInfo;
#endif /* MLTON_GC_INTERNAL_TYPES */

#if (defined (MLTON_GC_INTERNAL_FUNCS))
/**
 * This function allocates and initializes a chunk of at least allocableSize
 * allocable bytes.
 *
 * @param allocableSize The minimum number of allocable bytes in the chunk
 * @param levelHeadChunk The head chunk of the level to insert this new chunk
 * into.
 *
 * @return NULL if no chunk could be allocated, or chunk pointer otherwise.
 */
void* HM_allocateChunk(size_t allocableSize,
                       void* levelHeadChunk);

/**
 * This function allocates and initializes a level head chunk of at least
 * allocableSize allocable bytes.
 *
 * @param allocableSize The minimum number of allocable bytes in the chunk
 * @param level The level to assign to this head chunk
 * @param levelList The list to insert this new chunk into.
 *
 * @return NULL if no chunk could be allocated, or chunk pointer otherwise.
 */
void* HM_allocateLevelHeadChunk(size_t allocableSize,
                                size_t level,
                                void** levelList);

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

/**
 * This functions returns the highest level in a level list
 *
 * @param levelList The levelList to operate on
 *
 * @return CHUNK_INVALID_LEVEL if levelList is empty, the highest level
 * otherwise
 */
size_t HM_getHighestLevel(const void* levelList);

/**
 * Merges 'levelList' into 'destinationLevelList'.
 *
 * @note
 * All level head chunks in 'levelList' are demoted to regular chunks.
 *
 * @param destinationLevelList The destination of the merge
 * @param levelList The list to merge
 */
void HM_mergeLevelList(void** destinationLevelList, void* levelList);

/**
 * This function promotes the chunks from level 'level' to the level 'level -
 * 1'.
 *
 * @param levelList the level list to operate on
 * @param level The level to promotechunks*
 */
void HM_promoteChunks(void** levelList, size_t level);

/**
 * This function asserts the level list invariants
 *
 * @attention
 * If an assertion fails, this function aborts the program, as per the assert()
 * macro.
 *
 * @param levelList The level list to assert invariants for.
 */
void HM_assertLevelListInvariants(const void* levelList);
#endif /* MLTON_GC_INTERNAL_FUNCS */

#endif /* CHUNK_H_ */

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
#define CHUNK_INVALID_LEVEL (~((Word32)(0)))

/**
 * @brief
 * Represents the information about the chunk
 *
 * This packed object is placed at the beginning of a chunk for easy access
 */
struct HM_ChunkInfo {
  /* common between all chunks */
  void* frontier; /**< The end of the allocations in this chunk */

  void* limit; /**< The limit of the chunk. */

  void* nextChunk; /**< The next chunk in the heap's chunk list */

  Word32 level; /**< The level of this chunk. If set to CHUNK_INVALID_LEVEL,
                 * this chunk is not a level head and the level can be found by
                 * following split::normal::levelHead fields. */

  uint32_t padding; /**< Unused padding bytes to keep alignment */

  union {
    struct {
      void* nextHead; /**< The head chunk of the next level. This field
                       * is set if level is <em>not</em>
                       * CHUNK_INVALID_LEVEL */

      void* lastChunk; /**< The last chunk in this level's list of chunks */

      struct HM_HierarchicalHeap* containingHH; /**< The hierarchical heap
                                                 * containing this chunk */

      void* toChunkList; /**< The corresponding Chunk List in the to-LevelList
                          * during GC */

      Word64 size; /**< The size in number of bytes of this level, both
                      allocated and unallocated. */
    } levelHead; /**< The struct containing information for level head chunks */

    struct {
      void* levelHead; /**< Linked list of chunks ending in the head chunk of the
                        * level this chunk belongs to. This field is set if level
                        * is set to CHUNK_INVALID_LEVEL */
    } normal; /**< The struct containing information for normal chunks */
  } split;
} __attribute__((packed));

COMPILE_TIME_ASSERT(HM_ChunkInfo__packed,
                    sizeof(struct HM_ChunkInfo) ==
                    sizeof(void*) +
                    sizeof(void*) +
                    sizeof(void*) +
                    sizeof(Word32) +
                    sizeof(uint32_t) +
                    sizeof(void*) +
                    sizeof(void*) +
                    sizeof(struct HM_HierarchicalHeap*) +
                    sizeof(void*) +
                    sizeof(Word64));

COMPILE_TIME_ASSERT(HM_ChunkInfo__aligned,
                    (sizeof(struct HM_ChunkInfo) % 8) == 0);

struct HM_ObjptrInfo {
  struct HM_HierarchicalHeap* hh;
  void* chunkList;
  Word32 level;
};

#else
struct HM_ChunkInfo;
#endif /* MLTON_GC_INTERNAL_TYPES */

#if (defined (MLTON_GC_INTERNAL_FUNCS))
/**
 * This function allocates and initializes a chunk of at least allocableSize
 * allocable bytes.
 *
 * @param levelHeadChunk The head chunk of the level to insert this new chunk
 * into.
 * @param allocableSize The minimum number of allocable bytes in the chunk
 *
 * @return NULL if no chunk could be allocated, with chunkEnd undefined, or
 * chunk pointer otherwise with chunkEnd set to the end of the returned chunk.
 */
void* HM_allocateChunk(void* levelHeadChunk, size_t allocableSize);

/**
 * This function allocates and initializes a level head chunk of at least
 * allocableSize allocable bytes.
 *
 * @param levelList The list to insert this new chunk into.
 * @param allocableSize The minimum number of allocable bytes in the chunk
 * @param level The level to assign to this head chunk
 * @param hh The hierarchical heap this chunk belongs to
 *
 * @return NULL if no chunk could be allocated, with chunkEnd undefined, or
 * chunk pointer otherwise with chunkEnd set to the end of the returned chunk.
 */
void* HM_allocateLevelHeadChunk(void** levelList,
                                size_t allocableSize,
                                Word32 level,
                                struct HM_HierarchicalHeap* hh);

/**
 * Calls foreachHHObjptrInObject() on every object in 'destinationLevelList',
 * until no more objects exist.
 *
 * @attention
 * 'f' should not modify 'destinationLevelList', i.e., it should not add any
 * higher levels than those already there.
 *
 * @param s The GC_state to use
 * @param levelList The level list to iterate over
 * @param predicate The predicate function to apply to foreachObjptrInObject()
 * @param the arguments to the predicate function.
 * @param forwardHHObjptrArgs the args to use for forwardHHObjptr()
 */
void HM_forwardHHObjptrsInLevelList(
    GC_state s,
    void** levelList,
    ObjptrPredicateFunction predicate,
    void* predicateArgs,
    struct ForwardHHObjptrArgs* forwardHHObjptrArgs);

/**
 * Frees chunks in the level list up to the level specified, inclusive
 *
 * @param levelList The level list to free chunks from
 * @param minLevel The minimum level to free up to, inclusive
 */
void HM_freeChunks(void** levelList, Word32 minLevel);

/**
 * This function returns the frontier of the chunk
 *
 * @param chunk The chunk to operate on
 *
 * @return The frontier of the chunk
 */
void* HM_getChunkFrontier(void* chunk);

/**
 * This function returns the limit of the chunk (i.e. the end of the chunk)
 *
 * @param chunk The chunk to use
 *
 * @return The chunk's limit
 */
void* HM_getChunkLimit(void* chunk);

/**
 * This function returns the size of the chunk in bytes.
 *
 * @param chunk The chunk to return the size of.
 *
 * @return the size of the chunk in bytes.
 */
Word64 HM_getChunkSize(const void* chunk);

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
 * Returns the level of a chunk list
 *
 * @param chunkList The chunk list to operate on
 *
 * @return The level of the chunkList
 */
Word32 HM_getChunkListLevel(void* chunk);

/**
 * This function gets the last chunk in a list
 *
 * @param chunkList The list to get the last chunk of
 *
 * @return the last chunk, or NULL if the list is empty
 */
void* HM_getChunkListLastChunk(void* chunkList);

/**
 * This function returns the to-ChunkList corresponding to the specified
 * ChunkList during garbage collection.
 *
 * @param chunkList The ChunkList to get the to-ChunkList for
 *
 * @return NULL if no to-ChunkList has been set, the to-ChunkList otherwise.
 */
void* HM_getChunkListToChunkList(void* chunkList);

/**
 * This function returns the size in bytes of the specified level in a level
 * list.
 *
 * @param levelList The level list to search through.
 * @param level The level to get the size of.
 *
 * @return 0 if the level does not exist in the level list, or the size of the
 * level in bytes otherwise.
 */
Word64 HM_getLevelSize(void* levelList, Word32 level);

/**
 * This function sets the to-ChunkList corresponding to the specified ChunkList
 * during garbage collection.
 *
 * @param chunkList The ChunkList to set the to-ChunkList for.
 * @param toChunkList The ChunkList set the to-ChunkList to.
 */
void HM_setChunkListToChunkList(void* chunkList, void* toChunkList);

/**
 * Gets the info for the given objptr
 *
 * @attention
 * object <em>must</em> be within the allocated hierarchical heap!
 *
 * @param s The GC_state to use
 * @param object The objptr to get the Hierarchical Heap for
 * @param retVal Pointer to the struct to populate with 'object''s info.
 */
void HM_getObjptrInfo(GC_state s, objptr object, struct HM_ObjptrInfo* info);

/**
 * This functions returns the highest level in a level list
 *
 * @param levelList The levelList to operate on
 *
 * @return CHUNK_INVALID_LEVEL if levelList is empty, the highest level
 * otherwise
 */
Word32 HM_getHighestLevel(const void* levelList);

/**
 * Merges 'levelList' into 'destinationLevelList'.
 *
 * All level head chunks in 'levelList' that are merged into existing lists are
 * demoted to regular chunks.
 *
 * @attention
 * Note that pointers in 'levelList', such as 'containingHH' are <em>not</em>
 * updated.
 *
 * @param destinationLevelList The destination of the merge
 * @param levelList The list to merge
 * @param hh The HH containing the destination level list.
 */
void HM_mergeLevelList(void** destinationLevelList,
                       void* levelList,
                       struct HM_HierarchicalHeap * const hh);

/**
 * This function promotes the chunks from level 'level' to the level 'level -
 * 1'.
 *
 * @param levelList the level list to operate on
 * @param level The level to promotechunks*
 */
void HM_promoteChunks(void** levelList, size_t level);

/**
 * This function asserts that a chunk resides in a level list
 *
 * @attention
 * If an assertion fails, this function aborts the program, as per the assert()
 * macro.
 *
 * @param levelList the levelList to check residency within.
 * @param chunk the chunk to search for.
 */
void HM_assertChunkInLevelList(const void* levelList, const void* chunk);

/**
 * This function asserts the level list invariants
 *
 * @attention
 * If an assertion fails, this function aborts the program, as per the assert()
 * macro.
 *
 * @param levelList The level list to assert invariants for.
 * @param hh The hierarchical heap this level list belongs to.
 * @param stealLevel The level this level list was stolen from.
 */
void HM_assertLevelListInvariants(const void* levelList,
                                  const struct HM_HierarchicalHeap* hh,
                                  Word32 stealLevel);

/**
 * Updates the chunk's values to reflect mutator
 *
 * @param chunk The chunk to update
 * @param frontier The end of the allocations
 */
void HM_updateChunkValues(void* chunk, void* frontier);

/**
 * Update pointers in the level list to the hierarchical heap passed in. This
 * should be called upon moving a hierarchical heap to ensure that all pointers
 * are forwarded.
 *
 * @param levelList The level list to update
 * @param hh The hierarchical heap to update to
 */
void HM_updateLevelListPointers(void* levelList,
                                struct HM_HierarchicalHeap* hh);
#endif /* MLTON_GC_INTERNAL_FUNCS */

#endif /* CHUNK_H_ */

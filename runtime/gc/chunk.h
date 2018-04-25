/* Copyright (C) 2018 Sam Westrick
 * Copyright (C) 2015 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef CHUNK_H_
#define CHUNK_H_

#if (defined (MLTON_GC_INTERNAL_TYPES))

#define CHUNK_INVALID_LEVEL (~((Word32)(0)))

#define CHUNK_MAGIC 0xcafedabbedfacade

/* Chunks are contiguous regions of memory for containing heap-allocated user
 * (ML) objects. Metadata which is common across all objects within a chunk
 * are stored in this struct, at the front of the chunk. Each chunk metadata
 * struct is followed by a certain number
 * of reserved bytes, ending at .limit.
 * The reserved space up to .frontier contains user
 * data which is traceable by the GC.
 * Chunks are built from one or more contiguous blocks. The block size is
 * fixed at runtime with the `block-size` runtime argument.
 *
 * A CRUCIAL INVARIANT of chunks is that all constituent objects begin within
 * the FIRST BLOCK of the chunk. This permits looking up
 * the chunk metadata of any arbitrary objptr by masking the pointer.
 *
 * The heap hierarchy is built from chunks, which are organized into level
 * lists using the .nextChunk field. Each level list has a "levelhead" chunk,
 * which is the representative of that level, storing the depth of the level
 * as well as pointers to other levelheads. Other than storing this data,
 * levelheads are identical to normal chunks (e.g. they still store user
 * objects in the reserved space up to .frontier). */
struct GC_chunk {

  // common data for all chunks:
  Word64 magic;       // for sanity checks; should always be equal to CHUNK_MAGIC
  pointer frontier;   // end of allocations within this chunk
  pointer limit;      // the end of this chunk
  GC_chunk nextChunk; // linked list of chunks at the same level

  /* if != CHUNK_INVALID_LEVEL, then this chunk is a levelhead.
   * otherwise, a normal chunk. */
  Word32 level;

  // Unused padding bytes to keep alignment
  uint32_t padding;

  union {

    // Data for level-head chunks:
    struct {
      GC_chunk nextHead; // the head of the parent list (at depth level-1)
      GC_chunk lastChunk; /**< The last chunk in this level's list of chunks */
      struct HM_HierarchicalHeap* containingHH; /**< The hierarchical heap
                                                 * containing this chunk */
      void* toChunkList; /**< The corresponding Chunk List in the to-LevelList
                          * during GC */
      Word64 size; /**< The size in number of bytes of this level, both
                      allocated and unallocated. */
      bool isInToSpace; /**< False if the corresponding Chunk List is in
                         * from-space, true if it is in to-space. */
    } levelHead;

    /* Data for normal chunks is just a pointer towards the levelHead.
     * This pointer is used like a parent pointer in a path-compressing tree,
     * thus it might be necessary to follow the levelHead pointer multiple
     * times to find the actual level-head chunk. */
    struct {
      GC_chunk levelHead;
    } normal;

  } split;

} __attribute__((aligned(8)));

typedef struct GC_chunk* GC_chunk;

COMPILE_TIME_ASSERT(GC_chunk__aligned,
                    (sizeof(struct GC_chunk) % 8) == 0);

struct HM_ObjptrInfo {
  struct HM_HierarchicalHeap* hh;
  void* chunkList;
  Word32 level;
};

#else

struct GC_chunk;
typedef struct GC_chunk* GC_chunk;

#endif /* MLTON_GC_INTERNAL_TYPES */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static void* chunkOf(void* p);

/* Allocate and return a pointer to a new chunk in the list of the given
 * levelHead, with the requirement that
 *   chunk->limit - chunk->frontier <= bytesRequested
 * Returns NULL if unable to find space for such a chunk. */
GC_chunk HM_allocateChunk(GC_chunk levelHeadChunk, size_t bytesRequested);

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
 * Calls foreachHHObjptrInObject() on every object starting at 'start', which
 * should be inside a chunk.
 *
 * @param s The GC_state to use
 * @param start The pointer at which the scanning starts. Should point to the
 * beginning of the metadata of the object.
 * @param predicate The predicate function to apply to foreachObjptrInObject()
 * @param the arguments to the predicate function.
 * @param forwardHHObjptrArgs the args to use for forwardHHObjptr()
 */
void HM_forwardHHObjptrsInChunkList(
    GC_state s,
    void *start,
    ObjptrPredicateFunction predicate,
    void* predicateArgs,
    struct ForwardHHObjptrArgs* forwardHHObjptrArgs);

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
 * @param expectEntanglement Whether or not we expect entanglement during the
 *                           scan.
 */
void HM_forwardHHObjptrsInLevelList(
    GC_state s,
    void** levelList,
    ObjptrPredicateFunction predicate,
    void* predicateArgs,
    struct ForwardHHObjptrArgs* forwardHHObjptrArgs,
    bool expectEntanglement);

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
 * @param resetToFromSpace if true, reset the whole level list to to-space
 */
void HM_mergeLevelList(void** destinationLevelList,
                       void* levelList,
                       struct HM_HierarchicalHeap * const hh,
                       bool resetToFromSpace);

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
 * @param inToSpace If true, check that all the chunks are in to-space,
                    otherwise they should all be from-space.
 */
void HM_assertLevelListInvariants(const void* levelList,
                                  const struct HM_HierarchicalHeap* hh,
                                  Word32 stealLevel,
                                  bool inToSpace);

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

/**
 * This function retrieves the ChunkInfo object from a chunk
 *
 * @param chunk The chunk to retrieve the object from
 *
 * @return the ChunkInfo struct pointer
 */
GC_chunk HM_getChunkInfo(void* chunk);

/**
 * Same as HM_getChunkInfo() except for const-correctness. See HM_getChunkInfo()
 * for more details
 */
const GC_chunk HM_getChunkInfoConst(const void* chunk);

/**
 * Gets the head chunk for the given chunk info
 *
 * @param ci The chunk info to use
 * @param retVal Pointer to the head chunk of this chunk
 */
GC_chunk HM_getChunkHeadChunk(GC_chunk ci);

/**
 * Gets the level head chunk for the given objptr
 *
 * @attention
 * object <em>must</em> be within the allocated hierarchical heap!
 *
 * @param s The GC_state to use
 * @param object The objptr to get the Hierarchical Heap for
 * @param retVal Pointer to the head chunk of this object's level.
 */
GC_chunk HM_getObjptrLevelHeadChunk(GC_state s, objptr object);

/**
 * Gets the HH for the given objptr
 *
 * @attention
 * object <em>must</em> be within the allocated hierarchical heap!
 *
 * @param s The GC_state to use
 * @param object The objptr to get the Hierarchical Heap for
 * @param retVal Pointer to the HH of the object
 */
struct HM_HierarchicalHeap *HM_getObjptrHH(GC_state s, objptr object);

/**
 * Gets the HH lock for the given objptr
 *
 * @attention
 * object <em>must</em> be within the allocated hierarchical heap!
 *
 * @param s The GC_state to use
 * @param object The objptr to get the Hierarchical Heap lock for
 * @param retVal Pointer to the rwlock of this object's HH.
 */
rwlock_t *HM_getObjptrHHLock(GC_state s, objptr object);

/**
 * Check whether the given objptr is in to-space
 *
 * @attention
 * object <em>must</em> be within the allocated hierarchical heap!
 *
 * @param s The GC_state to use
 * @param object The objptr to get the Hierarchical Heap for
 * @param retVal True if the objptr is in to-space, false otherwise
 */
bool HM_isObjptrInToSpace(GC_state s, objptr object);

#endif /* MLTON_GC_INTERNAL_FUNCS */

#endif /* CHUNK_H_ */

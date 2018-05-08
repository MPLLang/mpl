/* Copyright (C) 2018 Sam Westrick
 * Copyright (C) 2015 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */


#ifndef CHUNK_H_
#define CHUNK_H_

#if (defined (MLTON_GC_INTERNAL_TYPES))

/* SAM_NOTE: Why do I need to declare here? Shouldn't the forwarding functions
 * be in hierarchical-heap-collection.{c,h}? */
struct ForwardHHObjptrArgs;

#define CHUNK_INVALID_LEVEL (~((Word32)(0)))

#define CHUNK_MAGIC 0xcafedabbedfacade

/* Chunks are contiguous regions of memory for containing heap-allocated user
 * (ML) objects. Metadata which is common across all objects within a chunk
 * are stored in this struct, at the front of the chunk. Each chunk metadata
 * struct is followed by a certain number of reserved bytes, ending at .limit.
 * The reserved space up to .frontier contains user data traceable by the GC.
 * Chunks are built from one or more contiguous blocks. The block size is
 * fixed at runtime with the `block-size` runtime argument.
 *
 * A CRUCIAL INVARIANT of chunks is that all constituent objects begin within
 * the FIRST BLOCK of the chunk. This permits looking up the chunk metadata of
 * any arbitrary objptr by masking the pointer.
 *
 * The heap hierarchy is built from chunks, which are organized into level
 * lists using the .nextChunk field. Each level list has a "levelhead" chunk,
 * which is the representative of that level, storing the depth of the level
 * as well as pointers to other levelheads. Other than storing this data,
 * levelheads are identical to normal chunks (e.g. they still store user
 * objects in the reserved space up to .frontier). */
struct HM_chunk {

  // common data for all chunks:
  Word64 magic;     // for sanity checks; should always be CHUNK_MAGIC
  pointer frontier; // end of allocations within this chunk
  pointer limit;    // the end of this chunk

  /* doubly linked list of chunks at this level.
   * when prevChunk == NULL, this chunk is a levelHead */
  struct HM_chunk *nextChunk;
  struct HM_chunk *prevChunk;

  // Word32 level;  /* the level of this chunk (if a levelhead), or
  //                 * CHUNK_INVALID_LEVEL for normal chunks */

  bool mightContainMultipleObjects;

  // Unused padding bytes to keep alignment
  uint32_t padding;

  union {

    // Data for level-head chunks:
    struct {
      struct HM_chunk *nextHead; // the head of the parent list (at depth level-1)
      struct HM_chunk *lastChunk; // The last chunk in this level
      struct HM_HierarchicalHeap *containingHH;
      struct HM_chunk *toChunkList; // the corresponding chunklist in the to-space during a GC
      Word64 size; // size (bytes) of this level, both allocated and unallocated
      bool isInToSpace;
      Word32 level;
    } levelHead;

    /* Data for normal chunks is just a pointer towards the levelHead.
     * This pointer is a parent pointer in a path-compressing tree,
     * thus it might be necessary to follow the levelHead pointer multiple
     * times to find the actual level-head chunk. */
    struct {
      struct HM_chunk *levelHead;
    } normal;

  } split;

} __attribute__((aligned(8)));

typedef struct HM_chunk* HM_chunk;

COMPILE_TIME_ASSERT(HM_chunk__aligned,
                    (sizeof(struct HM_chunk) % 8) == 0);

struct HM_ObjptrInfo {
  struct HM_HierarchicalHeap* hh;
  HM_chunk chunkList;
  Word32 level;
};

#else

struct HM_chunk;
typedef struct HM_chunk* HM_chunk;

#endif /* MLTON_GC_INTERNAL_TYPES */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

/* Find the associated chunk metadata of a pointer which is known to point
 * into the first block of a chunk. */
static HM_chunk HM_getChunkOf(pointer p);

/* Allocate and return a pointer to a new chunk in the list of the given
 * levelHead, with the requirement that
 *   chunk->limit - chunk->frontier <= bytesRequested
 * Returns NULL if unable to find space for such a chunk. */
HM_chunk HM_allocateChunk(HM_chunk levelHeadChunk, size_t bytesRequested);

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
HM_chunk HM_allocateLevelHeadChunk(HM_chunk* levelList,
                                   size_t allocableSize,
                                   Word32 level,
                                   struct HM_HierarchicalHeap* hh);

HM_chunk HM_splitChunk(GC_state s, HM_chunk chunk, size_t bytesRequested);

void HM_unlinkChunk(HM_chunk chunk);

bool HM_isLevelHeadChunk(HM_chunk chunk);

HM_chunk HM_getLevelHeadPathCompress(HM_chunk chunk);

/* append freeList onto the end of *parentFreeList, and update *parentFreeList
 * accordingly. */
void HM_mergeFreeList(HM_chunk *parentFreeList, HM_chunk freeList);

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
  pointer start,
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
  HM_chunk* levelList,
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
void HM_freeChunks(HM_chunk *levelList, HM_chunk *freeList, Word32 minLevel);

/**
 * This function returns the frontier of the chunk
 *
 * @param chunk The chunk to operate on
 *
 * @return The frontier of the chunk
 */
pointer HM_getChunkFrontier(HM_chunk chunk);

/**
 * This function returns the limit of the chunk (i.e. the end of the chunk)
 *
 * @param chunk The chunk to use
 *
 * @return The chunk's limit
 */
pointer HM_getChunkLimit(HM_chunk chunk);

/**
 * This function returns the size of the chunk in bytes.
 *
 * @param chunk The chunk to return the size of.
 *
 * @return the size of the chunk in bytes.
 */
Word64 HM_getChunkSize(HM_chunk chunk);

/**
 * This function returns the start of allocable area of the chunk (pointer to
 * the first byte that can be allocated), usually used as the initial heap
 * frontier.
 *
 * @param chunk The chunk to use
 *
 * @return start of the chunk
 */
pointer HM_getChunkStart(HM_chunk chunk);

/**
 * Returns the level of a chunk list
 *
 * @param chunkList The chunk list to operate on
 *
 * @return The level of the chunkList
 */
Word32 HM_getChunkListLevel(HM_chunk chunk);

/**
 * This function gets the last chunk in a list
 *
 * @param chunkList The list to get the last chunk of
 *
 * @return the last chunk, or NULL if the list is empty
 */
HM_chunk HM_getChunkListLastChunk(HM_chunk chunkList);

/**
 * This function returns the to-ChunkList corresponding to the specified
 * ChunkList during garbage collection.
 *
 * @param chunkList The ChunkList to get the to-ChunkList for
 *
 * @return NULL if no to-ChunkList has been set, the to-ChunkList otherwise.
 */
HM_chunk HM_getChunkListToChunkList(HM_chunk chunkList);

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
Word64 HM_getLevelSize(HM_chunk levelList, Word32 level);

/**
 * This function sets the to-ChunkList corresponding to the specified ChunkList
 * during garbage collection.
 *
 * @param chunkList The ChunkList to set the to-ChunkList for.
 * @param toChunkList The ChunkList set the to-ChunkList to.
 */
void HM_setChunkListToChunkList(HM_chunk chunkList, HM_chunk toChunkList);

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
Word32 HM_getHighestLevel(HM_chunk levelList);

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
void HM_mergeLevelList(HM_chunk* destinationLevelList,
                       HM_chunk levelList,
                       struct HM_HierarchicalHeap * const hh,
                       bool resetToFromSpace);

/**
 * This function promotes the chunks from level 'level' to the level 'level -
 * 1'.
 *
 * @param levelList the level list to operate on
 * @param level The level to promotechunks*
 */
void HM_promoteChunks(HM_chunk* levelList, size_t level);

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
void HM_assertChunkInLevelList(HM_chunk levelList, HM_chunk chunk);

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
void HM_assertLevelListInvariants(HM_chunk levelList,
                                  const struct HM_HierarchicalHeap* hh,
                                  Word32 stealLevel,
                                  bool inToSpace);

/**
 * Updates the chunk's values to reflect mutator
 *
 * @param chunk The chunk to update
 * @param frontier The end of the allocations
 */
void HM_updateChunkValues(HM_chunk chunk, pointer frontier);

/**
 * Update pointers in the level list to the hierarchical heap passed in. This
 * should be called upon moving a hierarchical heap to ensure that all pointers
 * are forwarded.
 *
 * @param levelList The level list to update
 * @param hh The hierarchical heap to update to
 */
void HM_updateLevelListPointers(HM_chunk levelList,
                                struct HM_HierarchicalHeap* hh);

/**
 * This function retrieves the ChunkInfo object from a chunk
 *
 * @param chunk The chunk to retrieve the object from
 *
 * @return the ChunkInfo struct pointer
 */
// HM_chunk HM_getChunkInfo(void* chunk);

/**
 * Same as HM_getChunkInfo() except for const-correctness. See HM_getChunkInfo()
 * for more details
 */
// const HM_chunk HM_getChunkInfoConst(const void* chunk);

/**
 * Gets the head chunk for the given chunk info
 *
 * @param ci The chunk info to use
 * @param retVal Pointer to the head chunk of this chunk
 */
HM_chunk HM_getChunkHeadChunk(HM_chunk ci);

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
HM_chunk HM_getObjptrLevelHeadChunk(GC_state s, objptr object);

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

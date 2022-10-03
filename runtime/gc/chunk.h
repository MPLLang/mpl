/* Copyright (C) 2018-2019 Sam Westrick
 * Copyright (C) 2015 Ram Raghunathan.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */


#ifndef CHUNK_H_
#define CHUNK_H_

struct HM_chunk;
struct HM_chunkList;
typedef struct HM_chunk * HM_chunk;
typedef struct HM_chunkList * HM_chunkList;

#if (defined (MLTON_GC_INTERNAL_TYPES))

/* SAM_NOTE: Why do I need to declare here? Shouldn't the forwarding functions
 * be in hierarchical-heap-collection.{c,h}? */
struct ForwardHHObjptrArgs;

#define CHUNK_INVALID_DEPTH (~((uint32_t)(0)))

#define CHUNK_MAGIC 0xcafeface

/* Chunks are contiguous regions of memory for containing heap-allocated user
 * (ML) objects. Metadata which is common across all objects within a chunk
 * are stored in this struct, at the front of the chunk. Each chunk metadata
 * struct is followed by a certain number of reserved bytes, ending at .limit.
 * The reserved space up to .frontier contains user data traceable by the GC.
 * Chunks are built from one or more contiguous blocks. The block size is
 * fixed at runtime with the `min-chunk` runtime argument.
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
  struct HM_UnionFindNode *levelHead;

  pointer frontier; // end of allocations within this chunk
  pointer limit;    // the end of this chunk

  HM_chunk nextChunk;
  HM_chunk prevChunk;

  // HM_chunk nextAdjacent;
  // HM_chunk prevAdjacent;

  /* some chunks may be used to store other non-ML allocated objects, like
   * heap records; if so, these will be stored at the front of the chunk, and
   * the startGap will indicate the amount of space used.
   * GC-traceable objects begin at
   *   (pointer)chunk + sizeof(struct HM_chunk) + startGap
   */
  uint8_t startGap;

  /* this bool is only used during local collections, to mark which chunks
   * have pinned objects. outside of a collection, it is always false. */
  bool pinnedDuringCollection;

  /** set during entanglement when in "safe" mode, to help temporarily disable
    * local GCs while the entanglement persists.
    */
  bool retireChunk;

  bool mightContainMultipleObjects;
  void* tmpHeap;

  SuperBlock container;
  size_t numBlocks;

  decheck_tid_t decheckState;

  // for padding and sanity checks
  uint32_t magic;

} __attribute__((aligned(8)));


struct HM_chunkList {
  HM_chunk firstChunk;
  HM_chunk lastChunk;

  /** usedSize = sum of used (frontier-start) space of chunks in this list.
    * size = total size of chunks in this list.
    *
    * So, for example, the fraction of "wasted" space in the list is
    *   (size-usedSize)/size
    */
  size_t usedSize;
  size_t size;

} __attribute__((aligned(8)));

COMPILE_TIME_ASSERT(HM_chunk__aligned,
                    (sizeof(struct HM_chunk) % 8) == 0);

#endif /* MLTON_GC_INTERNAL_TYPES */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

// GLOBALS ===================================================================

// Cache values from s->controls for convenience. These are overwritten once
// by HM_configChunks at program start.
extern size_t HM_BLOCK_SIZE;
extern size_t HM_ALLOC_SIZE;

// INLINE FUNCTIONS ==========================================================

static inline pointer blockOf(pointer p) {
  return (pointer)(uintptr_t)alignDown((size_t)p, HM_BLOCK_SIZE);
}

static inline bool inSameBlock(pointer p, pointer q) {
  return blockOf(p) == blockOf(q);
}

static inline bool inFirstBlockOfChunk(HM_chunk chunk, pointer p) {
  return p < (pointer)chunk + HM_BLOCK_SIZE;
}

/* Find the associated chunk metadata of a pointer which is known to point
 * into the first block of a chunk. */
static inline HM_chunk HM_getChunkOf(pointer p) {
  HM_chunk chunk = (HM_chunk)blockOf(p);
  assert(chunk->magic == CHUNK_MAGIC); // sanity check
  assert((pointer)chunk + sizeof(struct HM_chunk) <= p);

  /* Must be strictly less than the limit; a pointer at the limit would be
   * considered a pointer into the next physically adjacent chunk */
  assert(p < chunk->limit);

  return chunk;
}

// DECLARATIONS ==============================================================

// Sets the block size and alloc size; called once at program startup.
void HM_configChunks(GC_state s);

HM_chunk HM_initializeChunk(pointer start, pointer end);

HM_chunk HM_getFreeChunk(GC_state s, size_t bytesRequested);

/* Allocate and return a pointer to a new chunk in the list
 * Requires
 *   chunk->limit - chunk->frontier <= bytesRequested
 * Returns NULL if unable to find space for such a chunk. */
HM_chunk HM_allocateChunk(HM_chunkList list, size_t bytesRequested);
HM_chunk HM_allocateChunkWithPurpose(HM_chunkList list, size_t bytesRequested, enum BlockPurpose purpose);

void HM_initChunkList(HM_chunkList list);

void HM_freeChunk(GC_state s, HM_chunk chunk);
void HM_freeChunksInList(GC_state s, HM_chunkList list);

void HM_freeChunkWithInfo(GC_state s, HM_chunk chunk, writeFreedBlockInfoFnClosure f, enum BlockPurpose purpose);
void HM_freeChunksInListWithInfo(GC_state s, HM_chunkList list, writeFreedBlockInfoFnClosure f, enum BlockPurpose purpose);

// void HM_deleteChunks(GC_state s, HM_chunkList deleteList);
void HM_appendChunkList(HM_chunkList destinationChunkList, HM_chunkList chunkList);

void HM_appendChunk(HM_chunkList list, HM_chunk chunk);
void HM_prependChunk(HM_chunkList list, HM_chunk chunk);

/* Remove chunk from a list. Requires that the list actually contains the
 * chunk (this is not checked).
 *
 * The first form sets chunk->levelHead to NULL; the second leaves it
 * untouched.
 */
void HM_unlinkChunk(HM_chunkList list, HM_chunk chunk);
void HM_unlinkChunkPreserveLevelHead(HM_chunkList list, HM_chunk chunk);

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
static inline void HM_forwardHHObjptrsInChunkList(
  GC_state s,
  HM_chunk chunk,
  pointer start,
  GC_objptrPredicateFun predicate,
  void *predicateArgs,
  GC_foreachObjptrFun forwardHHObjptrFunc,
  struct ForwardHHObjptrArgs* forwardHHObjptrArgs);

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

typedef void (*HM_foreachObjFun)(GC_state s, pointer p, void* args);

typedef struct HM_foreachObjClosure {
  HM_foreachObjFun fun;
  void *env;
} *HM_foreachObjClosure;

void HM_foreachObjInChunk(GC_state s, HM_chunk chunk, HM_foreachObjClosure f);
void HM_foreachObjInChunkList(GC_state s, HM_chunkList list, HM_foreachObjClosure f);

void traverseEachObjInChunkList(GC_state s, HM_chunkList list);


/**
 * This function returns the size of the chunk in bytes.
 *
 * @param chunk The chunk to return the size of.
 *
 * @return the size of the chunk in bytes.
 */
size_t HM_getChunkSize(HM_chunk chunk);

/** frontier-start */
size_t HM_getChunkUsedSize(HM_chunk chunk);

/** limit-frontier */
size_t HM_getChunkSizePastFrontier(HM_chunk chunk);

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

/* shift the start of the chunk, to store e.g. a heap record.
 * returns a pointer to the gap, or NULL if no more space is available. */
pointer HM_shiftChunkStart(HM_chunk chunk, size_t bytes);

/* return the pointer to the start gap, or NULL if it doesn't have a gap. */
pointer HM_getChunkStartGap(HM_chunk chunk);

/* store the object pointed by p at the end of list and return the address */
pointer HM_storeInChunkList(HM_chunkList chunkList, void* p, size_t objSize);
pointer HM_storeInchunkListWithPurpose(HM_chunkList chunkList, void* p, size_t objSize, enum BlockPurpose purpose);


/**
 * This function gets the last chunk in a list
 *
 * @param chunkList The list to get the last chunk of
 *
 * @return the last chunk, or NULL if the list is empty
 */
HM_chunk HM_getChunkListLastChunk(HM_chunkList chunkList);
HM_chunk HM_getChunkListFirstChunk(HM_chunkList chunkList);

size_t HM_getChunkListSize(HM_chunkList list);
size_t HM_getChunkListUsedSize(HM_chunkList list);

/** These functions update the frontier of a chunk. This is used typically
  * to reflect bumps made by the mutator.
  *
  * Updating the frontier also needs to be reflected in the `usedSize` of
  * the list that holds the chunk. This is the purpose of the `list` argument
  * for `HM_updateChunkFrontierInList`. For updating the frontier of a chunk
  * that is not currently in any list, `HM_updateChunkFrontier` can be used
  * instead.
  */
void HM_updateChunkFrontierInList(
  HM_chunkList list,
  HM_chunk chunk,
  pointer frontier);
void HM_updateChunkFrontier(HM_chunk chunk, pointer frontier);

/** These query the union-find tree, to either look up the depth of
  * an object, or look up the heap that contains the object.
  */
uint32_t HM_getObjptrDepth(objptr op);
uint32_t HM_getObjptrDepthPathCompress(objptr op);
struct HM_HierarchicalHeap* HM_getLevelHead(HM_chunk chunk);
struct HM_HierarchicalHeap* HM_getLevelHeadPathCompress(HM_chunk chunk);

bool listContainsChunk(HM_chunkList list, HM_chunk theChunk);

void HM_assertChunkListInvariants(HM_chunkList list);

#endif /* MLTON_GC_INTERNAL_FUNCS */

#endif /* CHUNK_H_ */

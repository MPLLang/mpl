/* Copyright (C) 2018 Sam Westrick
 * Copyright (C) 2015 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
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

#define CHUNK_INVALID_LEVEL (~((Word32)(0)))

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

  pointer frontier; // end of allocations within this chunk
  pointer limit;    // the end of this chunk

  HM_chunk nextChunk;
  HM_chunk prevChunk;

  HM_chunk nextAdjacent;
  HM_chunk prevAdjacent;

  HM_chunkList levelHead;

  bool mightContainMultipleObjects;

  // for padding and sanity checks
  uint32_t magic;

} __attribute__((aligned(8)));

struct HM_chunkList {
  HM_chunkList parent;
  Word32 level;
  HM_chunkList rememberedSet;

  HM_chunk firstChunk;
  HM_chunk lastChunk;

  struct HM_HierarchicalHeap * containingHH;
  Word64 size; // size (bytes) of this level, both allocated and unallocated
  bool isInToSpace;
};

COMPILE_TIME_ASSERT(HM_chunk__aligned,
                    (sizeof(struct HM_chunk) % 8) == 0);

struct HM_ObjptrInfo {
  struct HM_HierarchicalHeap* hh;
  HM_chunkList chunkList;
  Word32 level;
};

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

/* Find the associated chunk metadata of a pointer which is known to point
 * into the first block of a chunk. */
static inline HM_chunk HM_getChunkOf(pointer p) {
  HM_chunk chunk = (HM_chunk)blockOf(p);
  assert(chunk->magic == CHUNK_MAGIC); // sanity check
  assert((pointer)chunk + sizeof(struct HM_chunk) <= p);
  assert(p <= chunk->limit);
  return chunk;
}

static inline bool HM_isUnlinked(HM_chunk chunk) {
  assert(chunk != NULL);
  return chunk->prevChunk == NULL &&
         chunk->nextChunk == NULL &&
         chunk->levelHead == NULL;
}

static inline bool HM_isLevelHead(HM_chunkList list) {
  return list->parent == list;
}

// DECLARATIONS ==============================================================

// Sets the block size and alloc size; called once at program startup.
void HM_configChunks(GC_state s);

/* Allocate and return a pointer to a new chunk in the list of the given
 * levelHead, with the requirement that
 *   chunk->limit - chunk->frontier <= bytesRequested
 * Returns NULL if unable to find space for such a chunk. */
HM_chunk HM_allocateChunk(HM_chunkList levelHeadChunk, size_t bytesRequested);

HM_chunkList HM_newChunkList(struct HM_HierarchicalHeap* hh, Word32 level);

void HM_appendChunkList(HM_chunkList destinationChunkList, HM_chunkList chunkList);

void HM_prependChunk(HM_chunkList levelHead, HM_chunk chunk);
void HM_appendChunk(HM_chunkList levelHead, HM_chunk chunk);
void HM_unlinkChunk(HM_chunk chunk);

/* Requires: chunk->limit - chunk->frontier >= bytesRequested
 * Requires: chunk is in a valid chunk list
 *
 * Attempts to split chunk into two, so that the result chunk contains at
 * least the requested number of bytes
 *   BEFORE: ... <-> chunk <-> ...
 *   AFTER:  ... <-> chunk <-> result <-> ...
 * if chunk cannot be split as such, returns NULL. */
HM_chunk HM_splitChunk(HM_chunk chunk, size_t bytesRequested);

void HM_coalesceChunks(HM_chunk left, HM_chunk right);

HM_chunkList HM_getLevelHeadPathCompress(HM_chunk chunk);

/* Lookup the levelhead for this chunk, but don't path compress. This is useful
 * for looking up the levelhead of a chunk which might not be locally owned */
HM_chunkList HM_getLevelHead(HM_chunk chunk);

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
Word32 HM_getChunkListLevel(HM_chunkList chunk);

/**
 * This function gets the last chunk in a list
 *
 * @param chunkList The list to get the last chunk of
 *
 * @return the last chunk, or NULL if the list is empty
 */
HM_chunk HM_getChunkListLastChunk(HM_chunkList chunkList);
HM_chunk HM_getChunkListFirstChunk(HM_chunkList chunkList);

Word64 HM_getChunkListSize(HM_chunkList levelHead);

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

void HM_assertLevelListInvariants(const struct HM_HierarchicalHeap* hh,
                                  Word32 stealLevel,
                                  bool inToSpace);

/**
 * Updates the chunk's values to reflect mutator
 *
 * @param chunk The chunk to update
 * @param frontier The end of the allocations
 */
void HM_updateChunkValues(HM_chunk chunk, pointer frontier);

HM_chunkList HM_getChunkList(HM_chunk chunk);


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

Word32 HM_getObjptrLevel(objptr op);

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

void assertObjptrInHH(objptr op);

#endif /* MLTON_GC_INTERNAL_FUNCS */

#endif /* CHUNK_H_ */

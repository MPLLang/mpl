/* Copyright (C) 2014-2017 Ram Raghunathan, Sam Westrick.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file chunk-allocator.h
 *
 * @author Ram Raghunathan, Sam Westrick
 *
 * @brief
 * The Chunk Allocator defines the black box from which the Hierarchical Heap (HH)
 * allocates and frees chunks. It also defines a mechanism to get the containing
 * chunk of an arbitrary object, if it exists.
 */

#ifndef CHUNK_ALLOCATOR_H_
#define CHUNK_ALLOCATOR_H_

/* Must be a power of 2 */
#define CHUNK_FIXED_SIZE (4 * 1024ULL)

#if (defined (MLTON_GC_INTERNAL_TYPES))

typedef void* (*ChunkAlloc_BatchFreeFunction) (void* args);
#endif /* MLTON_GC_INTERNAL_TYPES */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

/* Allocates a new variable-sized chunk with at least *bytesRequested bytes
 * of space. Returns a pointer to the front of the allocated space, or NULL if
 * the allocation was unsuccessful.
 *
 * SAM_NOTE: does this need to be 8-byte aligned? How is that guaranteed?
 */
void* ChunkAlloc_allocateVariableSize (size_t bytesRequested);

/* Allocate a new chunk of size CHUNK_FIXED_SIZE, guaranteed to be aligned
 * at multiples of CHUNK_FIXED_SIZE. Returns NULL if the allocation was
 * unsuccessful. */
void* ChunkAlloc_allocateFixedSize(void);

/**
* @brief Frees a batch of chunks via an iterator function
*
* @attention
* If a free fails, this function terminates and returns false
*
* @param f The iterator function
* @param fArgs The arguments to f
*
* @return TRUE if all frees succeeded, FALSE otherwise
*/
bool ChunkAlloc_iteratedFree (ChunkAlloc_BatchFreeFunction f, void* fArgs);

/* Free a chunk of memory. The argument must be a pointer returned respectively
 * by a call to ChunkAlloc_allocateVariableSize() or
 * ChunkAlloc_allocateFixedSize(). It is illegal to free a chunk twice. */
bool ChunkAlloc_freeVariableSize (void* chunk);
bool ChunkAlloc_freeFixedSize (void* chunk);

/* Return a pointer to the front of the chunk containing *object. Must be
 * in a fixed-size chunk. */
void* ChunkAlloc_findFixedFront (void* object);

/* Returns the total size of all currently allocated chunks. */
size_t ChunkAlloc_allocated (void);

/* Returns the high water mark of memory allocated by this interface (i.e.
 * the maximum of ChunkAlloc_allocated() across all timesteps) */
size_t ChunkAlloc_maxAllocated(void);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#endif /* CHUNK_ALLOCATOR_H_ */

/* Copyright (C) 2014 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file chunk-pool.h
 *
 * @author Ram Raghunathan
 *
 * @brief
 * The Chunk Pool defines black box from which the Hierarchical Heap (HH)
 * allocates and frees chunks. It also defines a mechanism to get the containing
 * chunk of an arbitrary object, if it exists.
 */

#ifndef CHUNK_POOL_H_
#define CHUNK_POOL_H_

#if (defined (MLTON_GC_INTERNAL_TYPES))
typedef void* (*ChunkPool_BatchFreeFunction) (void* args);
#endif /* MLTON_GC_INTERNAL_TYPES */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

/**
 * @brief Initializes the ChunkPool with the given <tt>poolSize</tt>
 *
 * <tt>poolSize</tt> is the maximum size of the pool. This function
 * <em>must</em> be called before any other functions can be called.
 *
 * @param poolSize Maximum size of the pool, in bytes
 */
void ChunkPool_initialize (size_t poolSize);

/**
 * @brief Adjusts the pool size for overHalfAllocated() checks
 *
 * If the current pool size has less than half the space free, the current pool
 * size is at most doubled for future allocations.
 */
void ChunkPool_adjustPoolSize(void);

/**
 * @brief Allocates a new chunk from the pool
 *
 * This function allocates a new chunk from the pool with <em>at least</em>
 * <tt>*bytesRequested</tt> bytes of space. The actual number of bytes in the
 * allocated chunk is stored in <tt>bytesRequested</tt> on return. However,
 * <tt>*bytesRequested</tt> is undefined upon a NULL return, when a chunk could
m * not be allocated
 *
 * @param bytesRequested Pointer to the number of bytes requested. Note that
 * <tt>*bytesRequested</tt> is undefined on a NULL return and set to the space
 * available otherwise.
 *
 * @return NULL if a chunk could not be allocated, pointer to an allocated chunk
 * otherwise. Guaranteed to be 8-byte aligned.
 */
void* ChunkPool_allocate (size_t* bytesRequested);

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
bool ChunkPool_iteratedFree (ChunkPool_BatchFreeFunction f, void* fArgs);

/**
 * @brief Frees a chunk back to the pool
 *
 * @param chunk The chunk to free. This <em>must</em> have been a value returned
 * by a call to <tt>ChunkPool_allocate()</tt>. Multiple frees of the same chunk
 * is illegal.
 *
 * @return true on success and false otherwise.
 */
bool ChunkPool_free (void* chunk);

/**
 * @brief Finds the containing chunk for the given pointer.
 *
 * This function finds the containing chunk for the given <tt>pointer</tt>. If
 * found, it returns a pointer to the beginning of the chunk which is the same
 * value returned by <tt>ChunkPool_allocate()</tt> upon allocation of the
 * chunk.
 *
 * @attention
 * This function does no error checking or locking so 'object' <em>must</em>
 * reside in an allocated chunk for the duration of this call.
 *
 * @param object Pointer to find containing chunk for.
 *
 * @return The beginning of the chunk as returned by
 * <tt>ChunkPool_allocate()</tt> on success, NULL otherwise.
 */
void* ChunkPool_find (void* object);

/**
 * This function returns if half of the chunk pool has been allocated
 *
 * @return TRUE if more than half of the chunk pool's space has been allocated,
 * FALSE otherwise
 */
bool ChunkPool_overHalfAllocated(void);

/**
 * This functions checks if 'pointer' is within the ChunkPool space
 *
 * @param pointer The pointer to check
 *
 * @return TRUE if 'pointer' is in the ChunkPool space, FALSE otherwise.
 */
bool ChunkPool_pointerInChunkPool (void* pointer);

/**
 * This function returns the size of the chunk.
 *
 * @attention
 * For performance, this function does <em>no</em> error checking or
 * locking. 'chunk' <em>must</em> be pointer to the first byte of an allocated
 * chunk for the duration of this call.
 *
 * @param chunk The chunk to return size of
 *
 * @return size of 'chunk' or 0 on error
 */
size_t ChunkPool_size(void* chunk);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */

#endif /* CHUNK_POOL_H_ */

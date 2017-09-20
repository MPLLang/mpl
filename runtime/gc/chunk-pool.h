/* Copyright (C) 2014-2016 Ram Raghunathan.
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
 * The Chunk Pool defines the black box from which the Hierarchical Heap (HH)
 * allocates and frees chunks. It also defines a mechanism to get the containing
 * chunk of an arbitrary object, if it exists.
 */

#ifndef CHUNK_POOL_H_
#define CHUNK_POOL_H_

#if (defined (MLTON_GC_INTERNAL_TYPES))
/**
 * @brief This struct contains the configuration information for the chunk pool
 */
struct ChunkPool_config {
  size_t initialSize; /**< Initial size of the chunk pool in bytes */

  size_t maxSize; /**< Maximum size of the pool, in bytes */

  double liveRatio; /**< minimum Heap:Live ratio to maintain */
};

typedef void* (*ChunkPool_BatchFreeFunction) (void* args);
#endif /* MLTON_GC_INTERNAL_TYPES */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

/**
 * @brief Initializes the ChunkPool with the given configuration.
 *
 * @note
 * 'config->initialSize' and 'config->maxSize' will be rounded down to an
 * integral multiple of the page size. 'liveRatio' must be greater than or equal
 * to 1.0. 'growRatio' must be greater than 1.0.
 *
 * @attention
 * This function <em>must</em> be called before any other functions can be
 * called. It can only be called once.
 *
 * @param config The ChunkPool configuration as defined by struct
 * ChunkPool_config.
 */
void ChunkPool_initialize (struct ChunkPool_config* config);

/**
 * @brief
 * Resizes the chunk pool if necessary according the initialization parameters.
 */
void ChunkPool_maybeResize(void);

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
 * This function returns the current number of bytes allocated from the pool.
 *
 * @return current number of bytes allocated.
 */
size_t ChunkPool_allocated(void);

/**
 * This function returns the maximum number of bytes allocated from the pool
 *
 * @return maximum number of bytes allocated.
 */
size_t ChunkPool_maxAllocated(void);

/**
 * This function returns the current size of the pool, in bytes.
 *
 * @return current size of the pool in bytes.
 */
size_t ChunkPool_size(void);

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
size_t ChunkPool_chunkSize(void* chunk);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#endif /* CHUNK_POOL_H_ */

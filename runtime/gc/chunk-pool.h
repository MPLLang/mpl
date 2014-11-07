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

#if (defined (MLTON_GC_INTERNAL_FUNCS))

/**
 * @brief Allocates a new chunk from the pool
 *
 * This function allocates a new chunk from the pool with <em>at least</em>
 * <tt>*bytesRequested</tt> bytes of space. The actual number of bytes in the
 * allocated chunk is stored in <tt>bytesRequested</tt> on return. However,
 * <tt>*bytesRequested</tt> is undefined upon a NULL return, when a chunk could
 * not be allocated
 *
 * @param bytesRequested Pointer to the number of bytes requested. Note that
 * <tt>*bytesRequested</tt> is undefined on a NULL return and set to the space
 * available otherwise.
 *
 * @return NULL if a chunk could not be allocated, pointer to an allocated chunk
 * otherwise. Guaranteed to be 8-byte aligned.
 */
PRIVATE void* ChunkPool_allocate (size_t* bytesRequested);

/**
 * @brief Frees a chunk back to the pool
 *
 * @param chunk The chunk to free. This <em>must</em> have been a value returned
 * by a call to <tt>ChunkPool_allocate()</tt>. Multiple frees of the same chunk
 * is illegal.
 *
 * @return true on success and false otherwise.
 */
PRIVATE bool ChunkPool_free (void* chunk);

/**
 * @brief Finds the containing chunk for the given pointer.
 *
 * This function finds the containing chunk for the given <tt>pointer</tt>. If
 * found, it returns a pointer to the beginning of the chunk which is the same
 * value returned by <tt>ChunkPool_allocate()</tt> upon allocation of the
 * chunk. It returns NULL if it cannot find the containing chunk or the
 * containing chunk has not been allocated.
 *
 * @param pointer Pointer to find containing chunk for.
 *
 * @return The beginning of the chunk as returned by
 * <tt>ChunkPool_allocate()</tt> on success, NULL otherwise.
 */
PRIVATE void* ChunkPool_find (void* pointer);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */

#endif /* CHUNK_POOL_H_ */

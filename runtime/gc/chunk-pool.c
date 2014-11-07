/* Copyright (C) 2014 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file chunk-pool.c
 *
 * @author Ram Raghunathan
 *
 * @brief
 * This is a naive implementation of the Chunk Pool interface with minimal
 * optimizations. It requires chunk sizes to be powers of 2 and multiples of
 * <tt>ChunkPool_MinimumChunkSize</tt>
 *
 * This implementation of the Chunk Pool interface found in chunk-pool.h is a
 * simple version that has serialized access to <tt>ChunkPool_allocate()</tt>
 * and <tt>ChunkPool_free</tt> along with a RW-lock on
 * <tt>ChunkPool_find()</tt>. It does not do any other multithreaded access or
 * locality optimizations. It is backed by a <tt>mmap()</tt>'d memory region.
 *
 * Finding a chunk is done by checking candidate chunk beginnings and verifying
 * them against a guarantor.
 The method used requires chunk sizes to be powers
 * of 2 and integral multiples of the minimum chunk size.
 */

/**
 * This specifies the compiled-in minimum chunk size. All allocation requests
 * will be satisfied with chunks that are at least this size
 */
static const size_t ChunkPool_MinimumChunkSize = 4 * 1024; /* 4KiB */

/**
 * This function is implemented and serializes against all other functions.
 */
PRIVATE void* ChunkPool_allocate (size_t* bytesRequested) {
#error Unimplemented!
}

/**
 * This function is implemented and serializes against all other functions.
 */
PRIVATE bool ChunkPool_free (void* chunk) {
#error Unimplemented!
}

/**
 * This function serializes against <tt>ChunkPool_allocate()</tt> and
 * <tt>ChunkPool_free()</tt> but is reentrant against itself. It works by
 * consecutively checking candidate chunk beginnings and verifying them against
 * a guarantor array. This continues until the chunk is found or the function
 * runs out of candidates to test. For fast performance, this method requires
 * chunk sizes to be powers of 2 and integral multiples of the minimum chunk
 * size.
 */
PRIVATE void* ChunkPool_find (void* pointer) {
#error Unimplemented!
}

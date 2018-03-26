/* Copyright (C) 2018 Sam Westrick
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

struct Blocks_batchInfo {
  void* base;
  size_t len;
};

#define BATCH_INFO_PTR(p) ((struct Blocks_batchInfo*)((size_t)p - sizeof(struct Blocks_batchInfo)))

#endif /* defined MLTON_GC_INTERNAL_TYPES */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

/* Allocate a region of size at least *bytesRequested, storing the resulting
 * usable size at *bytesRequested. The returned pointer is aligned at the
 * specified alignment.
 *
 * requests an extra `alignment` bytes in order to store a batchInfo at
 * the front of the batch (and therefore requires that the requested alignment
 * is at least as large as the size of the batchInfo) */
static pointer Blocks_allocRegion(size_t* bytesRequested, size_t alignment) {
  assert(sizeof(struct Blocks_batchInfo) <= alignment);
  size_t len = align(*bytesRequested, alignment);
  *bytesRequested = len;
  pointer base = (pointer) GC_mmapAnon(NULL, len + alignment);
  /* make sure there is space at the front for batchInfo */
  pointer p = (pointer) alignDown((size_t)base + alignment, alignment);
  BATCH_INFO_PTR(p)->base = (void*)base;
  BATCH_INFO_PTR(p)->len = len + alignment;

  assert(isAligned((size_t)p, alignment));
  return p;
}

void Blocks_freeBatch(pointer p) {
  GC_release(BATCH_INFO_PTR(p)->base, BATCH_INFO_PTR(p)->len);
}

pointer GC_getBlocks(GC_state s, size_t* bytesRequested) {
  size_t blockSize = s->controls->blocksConfig.blockSize;
  size_t batchSize = s->controls->blocksConfig.batchSize;
  size_t requested = *bytesRequested;

  /* When a large allocation is requested, just mmap a batch for it directly */
  if (requested > batchSize) {
    return Blocks_allocRegion(bytesRequested, blockSize);
  }

  if (NULL == s->freeBlocks || s->freeBlocksLength < requested) {
    s->freeBlocksLength = batchSize;
    s->freeBlocks = Blocks_allocRegion(&(s->freeBlocksLength), blockSize);
  }

  assert(s->freeBlocks != NULL);
  assert(s->freeBlocksLength >= requested);
  assert(isAligned(s->freeBlocks, blockSize));

  size_t len = align(requested, blockSize);
  *bytesRequested = len;
  pointer p = s->freeBlocks;
  s->freeBlocks += len;
  s->freeBlocksLength -= len;
  return p;
}

#endif /* defined (MLTON_GC_INTERNAL_FUNCS) */

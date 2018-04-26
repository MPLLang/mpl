/* Copyright (C) 2018 Sam Westrick
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))
#endif /* defined MLTON_GC_INTERNAL_TYPES */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static struct Block_config Block_sizes;

static void initBlocks(struct Block_config* config) {
  assert(isAligned(config->blockSize, GC_MODEL_MINALIGN));
  assert(config->blockSize >= GC_HEAP_LIMIT_SLOP);
  if (!isAligned(config->batchSize, config->blockSize)) {
    DIE("Error: batch size is not a multiple of the block size");
  }
  Block_sizes.blockSize = config->blockSize;
  Block_sizes.batchSize = config->batchSize;
}

static inline pointer blockOf(pointer p) {
  return (pointer)(uintptr_t)alignDown((size_t)p, Block_sizes.blockSize);
}

static inline bool inSameBlock(pointer p, pointer q) {
  return blockOf(p) == blockOf(q);
}

/* Allocate a region of size at least *bytesRequested, storing the resulting
 * usable size at *bytesRequested. The returned pointer is aligned at the
 * block size. */
static pointer Block_allocRegion(size_t* bytesRequested) {
  size_t bs = Block_sizes.blockSize;
  size_t len = align(*bytesRequested, bs);
  *bytesRequested = len;
  pointer base = (pointer)GC_mmapAnon(NULL, len + bs);
  pointer p = (pointer)(uintptr_t)align((size_t)base, bs);

  assert(isAligned((size_t)p, bs));
  return p;
}

pointer GC_getBlocks(GC_state s, size_t* bytesRequested) {
  size_t requested = *bytesRequested;

  /* When a large allocation is requested, just mmap a batch for it directly */
  if (requested > Block_sizes.batchSize) {
    return Block_allocRegion(bytesRequested);
  }

  if (NULL == s->freeBlocks || s->freeBlocksLength < requested) {
    s->freeBlocksLength = Block_sizes.batchSize;
    s->freeBlocks = Block_allocRegion(&(s->freeBlocksLength));
  }

  assert(s->freeBlocks != NULL);
  assert(s->freeBlocksLength >= align(requested, Block_sizes.blockSize));
  assert(isAligned((size_t)s->freeBlocks, Block_sizes.blockSize));

  size_t len = align(requested, Block_sizes.blockSize);
  *bytesRequested = len;
  pointer p = s->freeBlocks;
  s->freeBlocks += len;
  s->freeBlocksLength -= len;
  return p;
}

#endif /* defined (MLTON_GC_INTERNAL_FUNCS) */

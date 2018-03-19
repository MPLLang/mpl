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

/* requests an extra `alignment` bytes in order to store a batchInfo at
 * the front of the batch (and therefore requires that the requested alignment
 * is at least as large as the size of the batchInfo) */
pointer Blocks_allocBatch(size_t bytesRequested, size_t alignment) {
  assert(sizeof(struct Blocks_batchInfo) <= alignment);
  size_t len = align(bytesRequested, alignment) + alignment;
  pointer base = (pointer) mmapAnon(NULL, len);
  /* make sure there is space at the front for batchInfo */
  pointer p = (pointer) alignDown((size_t)base + alignment, alignment);
  BATCH_INFO_PTR(p)->base = (void*)base;
  BATCH_INFO_PTR(p)->len = len;

  assert(isAligned((size_t)p, alignment));
  return p;
}

void Blocks_freeBatch(pointer p) {
  munmap_safe(BATCH_INFO_PTR(p)->base, BATCH_INFO_PTR(p)->len);
}

#endif /* defined (MLTON_GC_INTERNAL_FUNCS) */

/* Copyright (C) 2021 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

void initFixedSizeAllocator(FixedSizeAllocator fsa, size_t fixedSize) {
  size_t minSize = sizeof(struct FixedSizeElement);
  fsa->fixedSize = align(fixedSize < minSize ? minSize : fixedSize, 8);
  HM_initChunkList(&(fsa->buffer));
  fsa->freeList = NULL;
  return;
}

void* allocateFixedSize(FixedSizeAllocator fsa) {
  // Fast path: if the freelist has a top element, just use that!
  if (NULL != fsa->freeList) {
    struct FixedSizeElement *elem = fsa->freeList;
    fsa->freeList = elem->nextFree;
    elem->nextFree = NULL;
    return (void*)elem;
  }

  HM_chunk chunk = HM_getChunkListLastChunk(&(fsa->buffer));

  /** Secondary fast path: if enough space at the frontier of the buffer,
    * then bump the pointer.
    */
  if (NULL != chunk && HM_getChunkSizePastFrontier(chunk) >= fsa->fixedSize) {
    pointer elem = chunk->frontier;
    chunk->frontier += fsa->fixedSize;
    assert(chunk->frontier <= chunk->limit);
    return elem;
  }

  /** Slow path: not enough space in the buffer, so we need to allocate a new
    * chunk. Note that this case implicitly handles the (chunk == NULL) case.
    */
  chunk = HM_allocateChunk(&(fsa->buffer), fsa->fixedSize);
  assert(NULL != chunk && HM_getChunkSizePastFrontier(chunk) >= fsa->fixedSize);
  pointer elem = chunk->frontier;
  chunk->frontier += fsa->fixedSize;
  assert(chunk->frontier <= chunk->limit);
  return elem;
}

void freeFixedSize(FixedSizeAllocator fsa, void* arg) {
  struct FixedSizeElement *elem = arg;
  elem->nextFree = fsa->freeList;
  fsa->freeList = elem;
  return;
}

#endif

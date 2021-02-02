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
  // Fast path #1: if the fast local freelist has a top element, just use that!
  if (NULL != fsa->freeList) {
    struct FixedSizeElement *elem = fsa->freeList;
    fsa->freeList = elem->nextFree;
    elem->nextFree = NULL;
    return (void*)elem;
  }

  /** Fast path #2: clear the shared list, if it's non-empty. We return the
    * top element, and all other elements from the shared list go into the
    * fast local freelist.
    *
    * SAM_NOTE: I'm pretty sure this doesn't suffer from ABA? The shared
    * freelist is essentially a Treiber stack, which (when popping) can suffer
    * from ABA. But in this scenario, pops are serialized against new
    * allocations, which would be the only source of ABA.
    */
  if (NULL != fsa->sharedFreeList) {
    struct FixedSizeElement *topElem = fsa->sharedFreeList;
    while (true) {
      if (__sync_bool_compare_and_swap(&(fsa->sharedFreeList), topElem, NULL))
        break;
      topElem = fsa->sharedFreeList;
    }
    assert(NULL != topElem);
    struct FixedSizeElement *otherElems = topElem->nextFree;
    fsa->freeList = otherElems;
    topElem->nextFree = NULL;
    return (void*)topElem;
  }

  // Fast path #3: bump space on the buffer, if we can.

  HM_chunk chunk = HM_getChunkListLastChunk(&(fsa->buffer));

  if (NULL != chunk && HM_getChunkSizePastFrontier(chunk) >= fsa->fixedSize) {
    pointer elem = chunk->frontier;
    chunk->frontier += fsa->fixedSize;
    assert(chunk->frontier <= chunk->limit);
    return elem;
  }

  /** Slow path: not enough space in the buffer, so we need to allocate a new
    * chunk. Note that this case implicitly handles the (chunk == NULL) case.
    *
    * When we allocate a new chunk, we include a pointer (in the start gap)
    * to the containing allocator. This makes it easy to "return" freed objects
    * to their original buffer, by looking up the chunk header.
    */

  chunk = HM_allocateChunk(&(fsa->buffer), fsa->fixedSize + sizeof(void*));
  pointer gap = HM_shiftChunkStart(chunk, sizeof(void*));
  *(FixedSizeAllocator *)gap = fsa;

  assert(NULL != chunk && HM_getChunkSizePastFrontier(chunk) >= fsa->fixedSize);
  pointer elem = chunk->frontier;
  chunk->frontier += fsa->fixedSize;
  assert(chunk->frontier <= chunk->limit);
  return elem;
}


void freeFixedSize(FixedSizeAllocator myfsa, void* arg) {
  HM_chunk chunk = HM_getChunkOf((pointer)arg);
  pointer gap = HM_getChunkStartGap(chunk);
  FixedSizeAllocator owner = (FixedSizeAllocator)gap;
  struct FixedSizeElement *elem = arg;

  /** Fast path! When I own the object, just use the fast free-list. */
  if (myfsa == owner) {
    elem->nextFree = myfsa->freeList;
    myfsa->freeList = elem;
    return;
  }

  /** Slow path: concurrent insertion into shared freelist. */
  while (true) {
    struct FixedSizeElement *oldVal = owner->sharedFreeList;
    elem->nextFree = oldVal;
    if (__sync_bool_compare_and_swap(&(owner->sharedFreeList), oldVal, elem))
      break;
  }
}

#endif

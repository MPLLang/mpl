#include "cc-work-list.h"

#if (defined (MLTON_GC_INTERNAL_FUNCS))


void CC_workList_init(
  __attribute__((unused)) GC_state s,
  CC_workList w)
{
  HM_chunkList c = &(w->storage);
  HM_initChunkList(c);
  // arbitrary, just need an initial chunk
  w->currentChunk = HM_allocateChunk(c, sizeof(objptr));
}


void CC_workList_push(
  __attribute__((unused)) GC_state s,
  CC_workList w,
  objptr op)
{
  HM_chunkList list = &(w->storage);
  HM_chunk chunk = w->currentChunk;
  size_t opsz = sizeof(objptr);

  if (HM_getChunkSizePastFrontier(chunk) < opsz) {
    if (chunk->nextChunk != NULL) {
      chunk = chunk->nextChunk; // this will be an empty chunk
    } else {
      chunk = HM_allocateChunk(list, opsz);
    }
    w->currentChunk = chunk;
  }

  assert(NULL != chunk);
  assert(HM_getChunkSizePastFrontier(chunk) >= opsz);
  assert(chunk == w->currentChunk);

  pointer frontier = HM_getChunkFrontier(chunk);
  HM_updateChunkFrontierInList(
    list,
    chunk,
    frontier + opsz);

  *((objptr*)frontier) = op;

  return;
}


objptr CC_workList_pop(
  GC_state s,
  CC_workList w)
{
  HM_chunkList list = &(w->storage);
  HM_chunk chunk = w->currentChunk;

  if (HM_getChunkFrontier(chunk) <= HM_getChunkStart(chunk)) {
    // chunk is empty; try to move backwards

    HM_chunk prevChunk = chunk->prevChunk;
    if (prevChunk == NULL) {
      // whole worklist is empty
      return BOGUS_OBJPTR;
    }

    /** Otherwise, there is a chunk before us. It's now safe (for cost
      * amortization) to delete the chunk after us, if there is one.
      */
    if (NULL != chunk->nextChunk) {
      HM_chunk nextChunk = chunk->nextChunk;
      HM_unlinkChunk(list, nextChunk);
      HM_freeChunk(s, nextChunk);
    }

    assert(NULL == chunk->nextChunk);
    assert(prevChunk == chunk->prevChunk);

    chunk = prevChunk;
    w->currentChunk = chunk;
  }

  assert(w->currentChunk == chunk);
  assert(HM_getChunkFrontier(chunk) >= HM_getChunkStart(chunk) + sizeof(objptr));

  pointer frontier = HM_getChunkFrontier(chunk);
  pointer newFrontier = frontier - sizeof(objptr);

  HM_updateChunkFrontierInList(
    list,
    chunk,
    newFrontier);

  objptr result = *((objptr*)newFrontier);

  return result;
}

#endif /* MLTON_GC_INTERNAL_FUNCS */

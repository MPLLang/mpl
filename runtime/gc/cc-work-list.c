#include "cc-work-list.h"

#if (defined (MLTON_GC_INTERNAL_FUNCS))


void CC_workList_init(
  __attribute__((unused)) GC_state s,
  CC_workList w)
{
  HM_chunkList c = &(w->storage);
  HM_initChunkList(c);
  // arbitrary, just need an initial chunk
  w->currentChunk = HM_allocateChunk(c, sizeof(struct CC_workList_elem));
}

void CC_workList_free(
    __attribute__((unused)) GC_state s,
    CC_workList w)
{
  HM_chunkList c = &(w->storage);
  HM_freeChunksInListWithInfo(s, c, NULL, BLOCK_FOR_UNKNOWN_PURPOSE);
  w->currentChunk = NULL;
}

bool CC_workList_isEmpty(
  __attribute__((unused)) GC_state s,
  CC_workList w)
{
  HM_chunkList list = &(w->storage);
  HM_chunk curr = w->currentChunk;

  return
    (list->firstChunk == curr)
    &&
    ((list->lastChunk == curr) || (list->lastChunk == curr->nextChunk))
    &&
    (HM_getChunkFrontier(curr) == HM_getChunkStart(curr))
    &&
    ( curr->nextChunk == NULL
      || HM_getChunkFrontier(curr->nextChunk) == HM_getChunkStart(curr->nextChunk)
    );
}


pointer findStackNonEmptyFrameTop(GC_state s, pointer bottom, pointer top) {
  assert(bottom <= top);

  while (top > bottom) {
    /* Invariant: top points just past a "return address". */
    GC_returnAddress returnAddress =
      *((GC_returnAddress*)(top - GC_RETURNADDRESS_SIZE));
    GC_frameInfo frameInfo = getFrameInfoFromReturnAddress(s, returnAddress);
    // index zero of this array is size
    GC_frameOffsets frameOffsets = frameInfo->offsets;

    if (frameOffsets[0] > 0) {
      return top;
    }

    top -= frameInfo->size;
  }

  return NULL;
}


// returns FALSE if object has no objptrs and doesn't need to be traced
bool makeInitialElem(GC_state s, objptr op, CC_workList_elem result) {
  GC_header header;
  uint16_t numObjptrs;
  GC_objectTypeTag tag;
  header = getHeader(objptrToPointer(op, NULL));
  splitHeader(s, header, &tag, NULL, NULL, &numObjptrs);

  if (NORMAL_TAG == tag) {
    if (0 == numObjptrs) return FALSE;

    result->op = op;
    result->data.normal.objptrIdx = 0;
    return TRUE;
  }

  if (SEQUENCE_TAG == tag) {
    if (0 == numObjptrs) return FALSE;
    if (0 == getSequenceLength(objptrToPointer(op, NULL))) return FALSE;

    result->op = op;
    result->data.sequence.cellIdx = 0;
    result->data.sequence.objptrIdx = 0;
    return TRUE;
  }

  if (STACK_TAG == tag) {
    // printf("makeInitialElem stack\n");

    GC_stack stack = (GC_stack)objptrToPointer(op, NULL);
    assert (stack->used <= stack->reserved);
    pointer bottom = getStackBottom(s, stack);
    pointer top = getStackTop(s, stack);
    pointer firstTop = findStackNonEmptyFrameTop(s, bottom, top);

    if (NULL == firstTop) {
      // printf("makeInitialElem: skipping stack "FMTOBJPTR"\n", op);
      return FALSE;
    }

    assert(firstTop > bottom);

    result->op = op;
    result->data.stack.topCursor = firstTop;
    result->data.stack.frameOffsetsIdx = 0;

    // printf("makeInitialElem: push stack "FMTOBJPTR"\n", op);
    return TRUE;
  }

  DIE("makeInitialElem: cannot handle tag %u", tag);
  return FALSE;
}


void CC_workList_push(
  GC_state s,
  CC_workList w,
  objptr op)
{
  struct CC_workList_elem elem;
  if (!makeInitialElem(s, op, &elem))
    return;

  HM_chunkList list = &(w->storage);
  HM_chunk chunk = w->currentChunk;
  size_t elemSize = sizeof(struct CC_workList_elem);

  if (HM_getChunkSizePastFrontier(chunk) < elemSize) {
    if (chunk->nextChunk != NULL) {
      chunk = chunk->nextChunk; // this will be an empty chunk
    } else {
      chunk = HM_allocateChunk(list, elemSize);
    }
    w->currentChunk = chunk;
  }

  assert(NULL != chunk);
  assert(HM_getChunkSizePastFrontier(chunk) >= elemSize);
  assert(chunk == w->currentChunk);

  pointer frontier = HM_getChunkFrontier(chunk);
  HM_updateChunkFrontierInList(
    list,
    chunk,
    frontier + elemSize);

  *(CC_workList_elem)frontier = elem;
  return;
}


struct advanceOneFieldResult {
  objptr* field;
  bool objectDone;
};

void advanceOneField(
  GC_state s,
  CC_workList_elem elem,
  struct advanceOneFieldResult * result)
{
  pointer p = objptrToPointer(elem->op, NULL);

  // inspect the object
  GC_header header;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
  GC_objectTypeTag tag;
  header = getHeader(p);
  splitHeader(s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);

  // ======================== NORMAL OBJECTS ========================

  if (NORMAL_TAG == tag) {
    uint16_t objptrIdx = elem->data.normal.objptrIdx;
    assert(objptrIdx < numObjptrs);
    result->field = (objptr*)(p + bytesNonObjptrs + (objptrIdx * OBJPTR_SIZE));
    result->objectDone = (objptrIdx+1 == numObjptrs);
    elem->data.normal.objptrIdx++;
    return;
  }

  // ======================== SEQUENCE OBJECTS ========================

  if (SEQUENCE_TAG == tag) {
    GC_sequenceLength numCells = getSequenceLength(p);
    size_t bytesPerCell = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);

    size_t cellIdx = elem->data.sequence.cellIdx;
    uint16_t objptrIdx = elem->data.sequence.objptrIdx;
    assert(cellIdx < numCells);
    assert(objptrIdx < numObjptrs);

    result->field =
      (objptr*)(
        p                            // object start
        + (cellIdx * bytesPerCell)   // cell offset
        + bytesNonObjptrs            // objptrs offset
        + (objptrIdx * OBJPTR_SIZE)  // current objptr offset
      );

    result->objectDone = FALSE;

    elem->data.sequence.objptrIdx++;
    if (objptrIdx+1 == numObjptrs) {
      elem->data.sequence.cellIdx++;
      elem->data.sequence.objptrIdx = 0;
      if (cellIdx+1 == numCells)
        result->objectDone = TRUE;
    }
    return;
  }

  // ======================== STACK OBJECTS ========================

  if (STACK_TAG == tag) {
    GC_stack stack = (GC_stack)p;
    pointer bottom = getStackBottom(s, stack);
    pointer top = elem->data.stack.topCursor;
    unsigned int i = elem->data.stack.frameOffsetsIdx;

    // printf(
    //   "advanceOneField: stack "FMTOBJPTR" top="FMTPTR" bottom="FMTPTR" i=%u\n",
    //   (uintptr_t)elem->op,
    //   (uintptr_t)top,
    //   (uintptr_t)bottom,
    //   i
    // );

    /* Invariant: top points just past a "return address". */
    GC_returnAddress returnAddress =
      *((GC_returnAddress*)(top - GC_RETURNADDRESS_SIZE));
    GC_frameInfo frameInfo = getFrameInfoFromReturnAddress(s, returnAddress);
    // index zero of this array is size
    GC_frameOffsets frameOffsets = frameInfo->offsets;
    pointer frameStart = top - frameInfo->size;

    assert(frameOffsets[0] > 0);
    assert(i < frameOffsets[0]);
    assert(frameStart >= bottom);

    result->field = (objptr*)(frameStart + frameOffsets[i+1]);
    elem->data.stack.frameOffsetsIdx++;

    result->objectDone = FALSE;

    if (i+1 == frameOffsets[0]) {
      /** walk backwards until we find a frame that has at least one objptr
        * or, until we find the end of the stack.
        */
      pointer newTop = findStackNonEmptyFrameTop(s, bottom, frameStart);

      if (NULL == newTop) {
        result->objectDone = TRUE;
      } else {
        elem->data.stack.topCursor = newTop;
        elem->data.stack.frameOffsetsIdx = 0;
      }
    }

    return;
  }

  // ======================== OTHERWISE... ========================

  DIE("advanceOneField: cannot handle tag %u", tag);
  return;
}


objptr* CC_workList_pop(
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
      return NULL;
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
  assert(HM_getChunkFrontier(chunk) >= HM_getChunkStart(chunk) + sizeof(struct CC_workList_elem));

  pointer frontier = HM_getChunkFrontier(chunk);
  pointer elemPtr = frontier - sizeof(struct CC_workList_elem);
  CC_workList_elem elem = (CC_workList_elem)elemPtr;

  struct advanceOneFieldResult r;
  advanceOneField(s, elem, &r);

  if (r.objectDone) {
    pointer newFrontier = elemPtr;
    HM_updateChunkFrontierInList(
      list,
      chunk,
      newFrontier);
  }

  assert(NULL != r.field);
  return r.field;
}

#endif /* MLTON_GC_INTERNAL_FUNCS */

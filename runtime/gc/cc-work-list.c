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


bool mightHaveObjptrs(GC_state s, objptr op) {
  GC_header header;
  uint16_t numObjptrs;
  GC_objectTypeTag tag;
  header = getHeader(objptrToPointer(op, NULL));
  splitHeader(s, header, &tag, NULL, NULL, &numObjptrs);

  return (STACK_TAG == tag) || (numObjptrs > 0);
}


void CC_workList_push(
  GC_state s,
  CC_workList w,
  objptr op)
{
  if (!mightHaveObjptrs(s, op))
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

  CC_workList_elem elem = (CC_workList_elem)frontier;
  elem->op = op;
  elem->pos = objptrToPointer(op, NULL);

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
  pointer pos = elem->pos;

  // inspect the object
  GC_header header;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;
  GC_objectTypeTag tag;
  header = getHeader(p);
  splitHeader(s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);

  // ======================== NORMAL OBJECTS ========================

  if (NORMAL_TAG == tag) {
    pointer end = p + bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
    pointer fieldsStart = p + bytesNonObjptrs;

    if (pos < fieldsStart) pos = fieldsStart;

    if (pos < end) {
      result->field = (objptr*)pos;
      pos = pos + OBJPTR_SIZE;
    } else {
      result->field = NULL;
    }

    elem->pos = pos;
    result->objectDone = (pos >= end);
    return;
  }

  // ======================== SEQUENCE OBJECTS ========================

  if (SEQUENCE_TAG == tag) {

    if (0 == numObjptrs) {
      /* No objptrs to process. */
      result->field = NULL;
      result->objectDone = TRUE;
      return;
    }

    GC_sequenceLength numCells = getSequenceLength(p);
    size_t bytesPerCell = bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
    size_t dataBytes = numCells * bytesPerCell;
    pointer last = p + dataBytes;

    size_t posOffset = (size_t)(pos-p);
    // pointer cellStart = p + alignDown(posOffset, bytesPerCell);
    pointer cellStart = pos - ((size_t)posOffset % bytesPerCell);

    pointer cellEnd = cellStart + bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE);
    pointer fieldsStart = cellStart + bytesNonObjptrs;

    if (pos < fieldsStart) pos = fieldsStart;

    if (pos < cellEnd) {
      result->field = (objptr*)pos;
      pos = pos + OBJPTR_SIZE;
    } else {
      result->field = NULL;
    }

    elem->pos = pos;
    result->objectDone = (pos >= last);

    return;
  }

  // ======================== STACK OBJECTS ========================

  if (STACK_TAG == tag) {
    DIE("advanceOneField: stack objects not implemented yet");
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

  /** It's possible to not succeed in finding a field in the most recent
    * object (if the object doesn't have remaining objptrs). So, if we fail,
    * try again. Eventually this will either return NULL because the worklist
    * is empty, or it will return a field pointer.
    */
  while (TRUE) {
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

    // We should only return NULL if the work list is empty.
    if (NULL != r.field)
      return r.field;
  }
}

#endif /* MLTON_GC_INTERNAL_FUNCS */

/* Copyright (C) 2014,2015 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file hierarchical-heap.c
 *
 * @author Ram Raghunathan
 *
 * This file implements the utility functions for the HierarchicalHeap object
 * described in hierarchical-heap.h.
 */

#include "hierarchical-heap.h"

/************************/
/* Function Definitions */
/************************/
void HM_displayHierarchicalHeap (
    const struct HM_HierarchicalHeap* hh,
    FILE* stream) {
  fprintf (stream,
           "\t\tlastAllocatedChunk = %p\n"
           "\t\tsavedFrontier = %p\n"
           "\t\tchunkList = %p\n"
           "\t\tparentHH = "FMTOBJPTR"\n"
           "\t\tnextChildHH = "FMTOBJPTR"\n"
           "\t\tchildHHList= "FMTOBJPTR"\n",
           hh->lastAllocatedChunk,
           hh->savedFrontier,
           hh->chunkList,
           hh->parentHH,
           hh->nextChildHH,
           hh->childHHList);
}

/* RAM_NOTE: Should be able to compute once and save result */
size_t HM_sizeofHierarchicalHeap (void) {
  GC_state s = pthread_getspecific (gcstate_key);

  size_t result = GC_NORMAL_HEADER_SIZE + sizeof (struct HM_HierarchicalHeap);
  result = align (result, s->alignment);

  if (DEBUG) {
    uint16_t bytesNonObjptrs;
    uint16_t numObjptrs;
    splitHeader (s,
                 GC_HIERARCHICAL_HEAP_HEADER,
                 NULL,
                 NULL,
                 &bytesNonObjptrs,
                 &numObjptrs);

    size_t check = GC_NORMAL_HEADER_SIZE +
                   (bytesNonObjptrs + (numObjptrs * OBJPTR_SIZE));

    if (DEBUG_DETAILED) {
      fprintf (
          stderr,
          "sizeofHierarchicalHeap: result = %"PRIuMAX"  check = %"PRIuMAX"\n",
          (uintmax_t)result,
          (uintmax_t)check);
    }

    assert (check == result);
  }
  assert (isAligned (result, s->alignment));

  return result;
}

/* RAM_NOTE: Should be able to compute once and save result */
size_t HM_offsetofHierarchicalHeap (void) {
  return ((HM_sizeofHierarchicalHeap ()) -
          GC_NORMAL_HEADER_SIZE +
          sizeof (struct HM_HierarchicalHeap));
}

void HM_appendChildHierarchicalHeap (pointer parentHHPointer,
                                     pointer childHHPointer) {
  GC_state s = pthread_getspecific (gcstate_key);

  objptr parentHHObjptr = pointerToObjptr (parentHHPointer, s->heap->start);
  struct HM_HierarchicalHeap* parentHH =
      ((struct HM_HierarchicalHeap*)(parentHHPointer +
                                     HM_offsetofHierarchicalHeap ()));

  objptr childHHObjptr = pointerToObjptr (childHHPointer, s->heap->start);
  struct HM_HierarchicalHeap* childHH =
      ((struct HM_HierarchicalHeap*)(childHHPointer +
                                     HM_offsetofHierarchicalHeap ()));

  /* childHH should be a orphan! */
  assert (BOGUS_OBJPTR == childHH->parentHH);
  assert (BOGUS_OBJPTR == childHH->nextChildHH);

  /*
   * If childHH's will be merged back in LIFO order, this sets up
   * parentHH->childHHList in that order
   */
  childHH->parentHH = parentHHObjptr;
  childHH->nextChildHH = parentHH->childHHList;
  parentHH->childHHList = childHHObjptr;
}

void HM_mergeIntoParentHierarchicalHeap (pointer hhPointer) {
  GC_state s = pthread_getspecific (gcstate_key);

  LOCAL_USED_FOR_ASSERT objptr hhObjptr =
      pointerToObjptr (hhPointer, s->heap->start);
  struct HM_HierarchicalHeap* hh =
      ((struct HM_HierarchicalHeap*)(hhPointer +
                                     HM_offsetofHierarchicalHeap ()));

  assert (BOGUS_OBJPTR != hh->parentHH);
  pointer parentHHPointer = objptrToPointer (hh->parentHH, s->heap->start);
  struct HM_HierarchicalHeap* parentHH =
      ((struct HM_HierarchicalHeap*)(parentHHPointer +
                                     HM_offsetofHierarchicalHeap ()));

  /* remove hh from parentHH->childHHList */
  /*
   * This assert assumes that all merges happen in LIFO order, as per the
   * comment in HM_appendChildHH ()
   */
  assert (parentHH->childHHList == hhObjptr);
  parentHH->childHHList = hh->nextChildHH;

  /* append hh->chunkList to parentHH->chunkList */
  HM_appendChunkList (&(parentHH->chunkList), hh->chunkList);
}

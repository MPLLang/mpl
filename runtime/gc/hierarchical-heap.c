/* Copyright (C) 2014 Ram Raghunathan.
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
           "\t\tsourceHH = "FMTOBJPTR"\n"
           "\t\tnextDerivedHH = "FMTOBJPTR"\n"
           "\t\tderivedHHList= "FMTOBJPTR"\n",
           hh->lastAllocatedChunk,
           hh->savedFrontier,
           hh->chunkList,
           hh->sourceHH,
           hh->nextDerivedHH,
           hh->derivedHHList);
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

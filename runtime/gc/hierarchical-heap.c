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
size_t HeapManagement_displayHierarchicalHeap (
    const struct HeapManagement_HierarchicalHeap* hierarchicalHeap,
    FILE* stream) {
  fprintf (stream,
           "HierarchicalHeap (%p) = {\n"
           "\t.lastAllocatedChunk = %p,\n"
           "\t.savedFrontier = %p,\n"
           "\t.chunkList = %p,\n"
           "\t.sourceHeap = %"FMTOBJPTR",\n"
           "\t.nextDerivativeHeap = %"FMTOBJPTR",\n"
           "\t.derivativeHeapList= %"FMTOBJPTR",\n"
           "}\n",
           hierarchicalHeap,
           hierarchicalHeap->lastAllocatedChunk,
           hierarchicalHeap->savedFrontier,
           hierarchicalHeap->chunkList,
           hierarchicalHeap->sourceHeap,
           hierarchicalHeap->nextDerivativeHeap,
           hierarchicalHeap->derivativeHeapList);
}

#pragma message "Should be able to compute once and save result"
size_t HeapManagement_sizeofHierarchicalHeap (void) {
  GC_state s = pthread_getspecific (gcstate_key);

  size_t result = GC_NORMAL_HEADER_SIZE +
                  sizeof (struct HeapManagement_HierarchicalHeap);
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

#pragma message "Should be able to compute once and save result"
size_t HeapManagement_offsetofHierarchicalHeap (void) {
  return ((sizeofHierarchicalHeap ()) -
          GC_NORMAL_HEADER_SIZE +
          sizeof (struct HeapManagement_HierarchicalHeap));
}

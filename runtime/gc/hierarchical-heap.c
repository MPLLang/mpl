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

/******************************/
/* Static Function Prototypes */
/******************************/
/**
 * This function converts a hierarchical heap objptr to the struct
 * HM_HierarchicalHeap
 *
 * @param hhObjptr the objptr to convert
 *
 * @return the contained struct HM_HierarchicalHeap if hhObjptr is a valid
 * objptr, NULL otherwise
 */
static struct HM_HierarchicalHeap* HHObjptrToStruct(GC_state s,
                                                    objptr hhObjptr);

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
size_t HM_sizeofHierarchicalHeap (GC_state s) {
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
size_t HM_offsetofHierarchicalHeap (GC_state s) {
  return ((HM_sizeofHierarchicalHeap (s)) -
          (GC_NORMAL_HEADER_SIZE +
           sizeof (struct HM_HierarchicalHeap)));
}

void HM_appendChildHierarchicalHeap (pointer parentHHPointer,
                                     pointer childHHPointer) {
  GC_state s = pthread_getspecific (gcstate_key);

  objptr parentHHObjptr = pointerToObjptr (parentHHPointer, s->heap->start);
  struct HM_HierarchicalHeap* parentHH = HHObjptrToStruct(s, parentHHObjptr);

  objptr childHHObjptr = pointerToObjptr (childHHPointer, s->heap->start);
  struct HM_HierarchicalHeap* childHH = HHObjptrToStruct(s, childHHObjptr);

#if ASSERT
  HM_assertHierarchicalHeapInvariants(s, parentHH);
  HM_assertHierarchicalHeapInvariants(s, childHH);
#endif /* ASSERT */

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

#if ASSERT
  HM_assertHierarchicalHeapInvariants(s, parentHH);
  HM_assertHierarchicalHeapInvariants(s, childHH);
#endif /* ASSERT */
}

void HM_mergeIntoParentHierarchicalHeap (pointer hhPointer) {
  GC_state s = pthread_getspecific (gcstate_key);

  objptr hhObjptr = pointerToObjptr (hhPointer, s->heap->start);
  struct HM_HierarchicalHeap* hh = HHObjptrToStruct(s, hhObjptr);

  assert (BOGUS_OBJPTR != hh->parentHH);
  struct HM_HierarchicalHeap* parentHH = HHObjptrToStruct(s, hh->parentHH);

#if ASSERT
  HM_assertHierarchicalHeapInvariants(s, parentHH);
  HM_assertHierarchicalHeapInvariants(s, hh);
#endif /* ASSERT */

  /* remove hh from parentHH->childHHList */
  /*
   * This assert assumes that all merges happen in LIFO order, as per the
   * comment in HM_appendChildHH ()
   */
  assert (parentHH->childHHList == hhObjptr);
  parentHH->childHHList = hh->nextChildHH;

  /* append hh->chunkList to parentHH->chunkList */
  assert(HM_getLastChunk(hh->chunkList) == hh->lastAllocatedChunk);
  HM_appendChunkList (&(parentHH->chunkList),
                      hh->chunkList,
                      hh->lastAllocatedChunk);

#if ASSERT
  HM_assertHierarchicalHeapInvariants(s, parentHH);
  /* don't assert hh here as it should be thrown away! */
#endif /* ASSERT */
}

#if ASSERT
void HM_assertHierarchicalHeapInvariants(GC_state s,
                                         const struct HM_HierarchicalHeap* hh) {
  HM_assertChunkListInvariants(hh->chunkList);
  assert(HM_getLastChunk(hh->chunkList) == hh->lastAllocatedChunk);
  if (NULL != hh->savedFrontier) {
    assert(hh->lastAllocatedChunk == ChunkPool_find(hh->savedFrontier));
  }

  struct HM_HierarchicalHeap* parentHH = HHObjptrToStruct(s, hh->parentHH);
  if (NULL != parentHH) {
    /* Make sure I am in parentHH->childHHList */
    bool foundInParentList = FALSE;
    for (struct HM_HierarchicalHeap* childHH =
             HHObjptrToStruct(s, parentHH->childHHList);
         NULL != childHH;
         childHH = HHObjptrToStruct(s, childHH->nextChildHH)) {
      if (hh == childHH) {
        foundInParentList = TRUE;
        break;
      }
    }
    assert(foundInParentList);
  }

  for (struct HM_HierarchicalHeap* childHH = HHObjptrToStruct(s,
                                                              hh->childHHList);
       NULL != childHH;
       childHH = HHObjptrToStruct(s, childHH->nextChildHH)) {
    assert(HHObjptrToStruct(s, childHH->parentHH) == hh);
  }
}
#endif /* ASSERT */

static struct HM_HierarchicalHeap* HHObjptrToStruct(GC_state s,
                                                    objptr hhObjptr) {
  if (BOGUS_OBJPTR == hhObjptr) {
    return NULL;
  }

  pointer hhPointer = objptrToPointer (hhObjptr, s->heap->start);
  return ((struct HM_HierarchicalHeap*)(hhPointer +
                                        HM_offsetofHierarchicalHeap (s)));
}

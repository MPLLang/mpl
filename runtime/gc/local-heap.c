/* Copyright (C) 2014 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file local-heap.c
 *
 * @author Ram Raghunathan
 *
 * This file implements the local heap interface defined in
 * local-heap.h.
 */

#include "local-heap.h"

#include "heap-utils.h"

/************************/
/* Function Definitions */
/************************/
void HM_enterLocalHeap (GC_state s) {
  const struct HM_HierarchicalHeap* hh = HM_getCurrentHierarchicalHeap(s);

  s->frontier = HM_getHierarchicalHeapSavedFrontier(hh);
  s->limit = HM_getChunkEnd(HM_getHierarchicalHeapLastAllocatedChunk(hh));
}

void HM_exitLocalHeap (GC_state s) {
  struct HM_HierarchicalHeap* hh = HM_getCurrentHierarchicalHeap(s);

  HM_setHierarchicalHeapSavedFrontier(hh, s->frontier);
}

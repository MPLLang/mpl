/* Copyright (C) 2018,2019 Sam Westrick
 * Copyright (C) 2014,2015 Ram Raghunathan.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef HIERARCHICAL_HEAP_H_
#define HIERARCHICAL_HEAP_H_

#include "chunk.h"

#if (defined (MLTON_GC_INTERNAL_TYPES))

typedef struct HM_HierarchicalHeap {
  struct HM_HierarchicalHeap *representative;
  uint32_t depth;

  struct HM_chunkList chunkList;

  struct HM_chunkList rememberedSet;

  /* The next non-empty ancestor heap. This may skip over "unused" levels.
   * Also, all threads have their own leaf-to-root path (essentially, path
   * copying) which is merged only at join points of the program. */
  struct HM_HierarchicalHeap *nextAncestor;

} *HM_HierarchicalHeap;

#define HM_HH_INVALID_DEPTH CHUNK_INVALID_DEPTH

#else

struct HM_HierarchicalHeap;
typedef struct HM_HierarchicalHeap *HM_HierarchicalHeap;

#endif /* MLTON_GC_INTERNAL_TYPES */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline HM_chunkList HM_HH_getChunkList(HM_HierarchicalHeap hh)
{
  return &(hh->chunkList);
}

static inline HM_chunkList HM_HH_getRemSet(HM_HierarchicalHeap hh)
{
  return &(hh->rememberedSet);
}

HM_HierarchicalHeap HM_HH_new(GC_state s, uint32_t depth);

uint32_t HM_HH_getDepth(HM_HierarchicalHeap hh);

bool HM_HH_isLevelHead(HM_HierarchicalHeap hh);

void HM_HH_merge(GC_state s, GC_thread parent, GC_thread child);
void HM_HH_promoteChunks(GC_state s, GC_thread thread);
void HM_HH_ensureNotEmpty(GC_state s, GC_thread thread);

bool HM_HH_extend(GC_state s, GC_thread thread, size_t bytesRequested);

/* zip-up hh1 and hh2, returning the new deepest leaf
 * (will be one of hh1 or hh2) */
HM_HierarchicalHeap HM_HH_zip(HM_HierarchicalHeap hh1, HM_HierarchicalHeap hh2);

/* Find the HH at the indicated depth (create one if doesn't exist) */
HM_HierarchicalHeap HM_HH_getHeapAtDepth(GC_state s, GC_thread thread, uint32_t depth);

HM_HierarchicalHeap HM_HH_getCurrent(GC_state s);
pointer HM_HH_getFrontier(GC_thread thread);
pointer HM_HH_getLimit(GC_thread thread);
void HM_HH_updateValues(GC_thread thread, pointer frontier);

size_t HM_HH_nextCollectionThreshold(GC_state s, size_t survivingSize);
size_t HM_HH_addRecentBytesAllocated(GC_thread thread, size_t bytes);

uint32_t HM_HH_desiredCollectionScope(GC_state s, GC_thread thread);

#endif /* MLTON_GC_INTERNAL_FUNCS */

#endif /* HIERARCHICAL_HEAP_H_ */

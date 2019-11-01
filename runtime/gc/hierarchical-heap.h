/* Copyright (C) 2018,2019 Sam Westrick
 * Copyright (C) 2014,2015 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef HIERARCHICAL_HEAP_H_
#define HIERARCHICAL_HEAP_H_

#include "chunk.h"

#if (defined (MLTON_GC_INTERNAL_TYPES))

#define HM_MAX_NUM_LEVELS 64

struct levelData {
  HM_chunkList chunkList;
};

typedef struct HM_HierarchicalHeap {
  struct levelData levels[HM_MAX_NUM_LEVELS];
} *HM_HierarchicalHeap;

// l/r-value for ith level
#define HM_HH_LEVEL(hh, i) ((hh)->levels[i].chunkList)

/* SAM_NOTE: These macros are nasty. But they are also nice. Sorry. */
#define FOR_LEVEL_IN_RANGE(LEVEL, IDX, HH, LO, HI, BODY) \
  do {                                                   \
    for (uint32_t IDX = (LO); IDX < (HI); IDX++) {       \
      HM_chunkList LEVEL = HM_HH_LEVEL(HH, IDX);         \
      if (LEVEL != NULL) { BODY }                        \
    }                                                    \
  } while (0)

#define FOR_LEVEL_DECREASING_IN_RANGE(LEVEL, IDX, HH, LO, HI, BODY) \
  do {                                                              \
    uint32_t IDX = (HI);                                            \
    while (IDX > (LO)) {                                            \
      IDX--;                                                        \
      HM_chunkList LEVEL = HM_HH_LEVEL(HH, IDX);                    \
      if (LEVEL != NULL) { BODY }                                   \
    }                                                               \
  } while (0)

#define HM_HH_INVALID_DEPTH CHUNK_INVALID_DEPTH

#else

struct HM_HierarchicalHeap;
typedef struct HM_HierarchicalHeap *HM_HierarchicalHeap;

#endif /* MLTON_GC_INTERNAL_TYPES */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

struct HM_HierarchicalHeap* HM_HH_new(GC_state s);

void HM_HH_merge(GC_state s, GC_thread parent, GC_thread child);
void HM_HH_promoteChunks(GC_state s, GC_thread thread);
void HM_HH_ensureNotEmpty(GC_thread thread);

static inline size_t HM_HH_levelSize(struct HM_HierarchicalHeap *hh, uint32_t depth);

bool HM_HH_extend(GC_thread thread, size_t bytesRequested);

struct HM_HierarchicalHeap* HM_HH_getCurrent(GC_state s);
pointer HM_HH_getFrontier(GC_thread thread);
pointer HM_HH_getLimit(GC_thread thread);
void HM_HH_updateValues(GC_thread thread, pointer frontier);

size_t HM_HH_size(struct HM_HierarchicalHeap* hh, uint32_t currentDepth);
size_t HM_HH_nextCollectionThreshold(GC_state s, size_t survivingSize);
size_t HM_HH_addRecentBytesAllocated(GC_thread thread, size_t bytes);

uint32_t HM_HH_desiredCollectionScope(GC_state s, GC_thread thread);

#endif /* MLTON_GC_INTERNAL_FUNCS */

#endif /* HIERARCHICAL_HEAP_H_ */

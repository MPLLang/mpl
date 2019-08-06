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
  // size_t capacity;
  // size_t outstandingBytesPromoted;
};

struct HM_HierarchicalHeap {
  struct levelData levels[HM_MAX_NUM_LEVELS];

  HM_chunk lastAllocatedChunk; /**< The last allocated chunk */

  uint32_t level; /**< The current level of the hierarchy which new chunks should
                 * belong to. */

  uint32_t shallowestLevel;

  /* when the number of bytes in this HH exceeds the threshold, we collect.
   * the threshold is adjusted after each collection. */
  size_t collectionThreshold;
};

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

#define HM_HH_INVALID_LEVEL CHUNK_INVALID_LEVEL

#else

struct HM_HierarchicalHeap;

#endif /* MLTON_GC_INTERNAL_TYPES */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

struct HM_HierarchicalHeap* HM_HH_new(GC_state s);

/* stealLevel is the level of the parent that will now become suspended due
 * to this child. */
void HM_HH_appendChild(GC_state s,
                       struct HM_HierarchicalHeap* parentHH,
                       struct HM_HierarchicalHeap* childHH,
                       uint32_t stealLevel);

uint32_t HM_HH_getLevel(GC_state s, struct HM_HierarchicalHeap* hh);
void HM_HH_merge(GC_state s, struct HM_HierarchicalHeap* parent, struct HM_HierarchicalHeap* child);
void HM_HH_promoteChunks(GC_state s, struct HM_HierarchicalHeap* hh);
void HM_HH_setLevel(GC_state s, struct HM_HierarchicalHeap* hh, uint32_t level);
void HM_HH_display(struct HM_HierarchicalHeap* hh, FILE* stream);
void HM_HH_ensureNotEmpty(struct HM_HierarchicalHeap* hh);

/**
 * This function extends the hierarchical heap with at least bytesRequested free
 * space.
 *
 * @attention
 * On successful completion, the frontier in hh->lastAllocatedChunk is updated
 * to the frontier of the extension and HM_getHierarchicalHeapLimit(hh) will
 * return the limit of the extension.
 *
 * @param hh The hierarchical heap to extend
 * @param bytesRequested The minimum size of the extension
 *
 * @return TRUE if extension succeeded, FALSE otherwise
 */
bool HM_HH_extend(struct HM_HierarchicalHeap* hh, size_t bytesRequested);

struct HM_HierarchicalHeap* HM_HH_getCurrent(GC_state s);
pointer HM_HH_getFrontier(struct HM_HierarchicalHeap* hh);
pointer HM_HH_getLimit(struct HM_HierarchicalHeap* hh);
void HM_HH_updateValues(struct HM_HierarchicalHeap* hh, pointer frontier);

size_t HM_HH_size(struct HM_HierarchicalHeap* hh);
size_t HM_HH_nextCollectionThreshold(GC_state s, size_t survivingSize);

#endif /* MLTON_GC_INTERNAL_FUNCS */

#endif /* HIERARCHICAL_HEAP_H_ */

/* Copyright (C) 2018 Sam Westrick
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

struct HM_HierarchicalHeap {
  HM_chunkList levels[HM_MAX_NUM_LEVELS];
  Word64 capacities[HM_MAX_NUM_LEVELS];

  HM_chunk lastAllocatedChunk; /**< The last allocated chunk */

  Word32 level; /**< The current level of the hierarchy which new chunks should
                 * belong to. */

  Word32 stealLevel; /**< The parent's level that I stole from */

  Word64 locallyCollectibleSize; /**< The size in bytes of the locally
                                  * collectable heap. */

  Word64 locallyCollectibleHeapSize; /** < The size in bytes of locally
                                      * collectible heap size, used for
                                      * collection decisions */


  struct HM_HierarchicalHeap* parentHH; /**< The heap this object branched off of or BOGUS_OBJPTR
                    * if it is the first heap. */

  struct HM_HierarchicalHeap* nextChildHH; /**< The next heap in the 'derivedHHList' of
                       * the 'parentHH'. This variable is the 'next'
                       * pointer for the intrusive linked list. */

  struct HM_HierarchicalHeap* childHHList; /**< The list of heaps that are derived from this
                       * heap. All heaps in this list have their 'parentHH' set
                       * to this object. In addition, it is in descending order
                       * of 'stealLevel' */
};

// l/r-value for ith level
#define HM_HH_LEVEL(hh, i) ((hh)->levels[i])
#define HM_HH_LEVEL_CAPACITY(hh, i) ((hh)->capacities[i])

/* SAM_NOTE: These macros are nasty. But they are also nice. Sorry. */
#define FOR_LEVEL_IN_RANGE(LEVEL, IDX, HH, LO, HI, BODY) \
  do {                                                   \
    for (Word32 IDX = (LO); IDX < (HI); IDX++) {         \
      HM_chunkList LEVEL = HM_HH_LEVEL(HH, IDX);         \
      if (LEVEL != NULL) { BODY }                        \
    }                                                    \
  } while (0)

#define FOR_LEVEL_DECREASING_IN_RANGE(LEVEL, IDX, HH, LO, HI, BODY) \
  do {                                                              \
    Word32 IDX = (HI);                                              \
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
                       Word32 stealLevel);

Word32 HM_HH_getLevel(GC_state s, struct HM_HierarchicalHeap* hh);
void HM_HH_mergeIntoParent(GC_state s, struct HM_HierarchicalHeap* hh);
void HM_HH_promoteChunks(GC_state s, struct HM_HierarchicalHeap* hh);
void HM_HH_setLevel(GC_state s, struct HM_HierarchicalHeap* hh, Word32 level);
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
Word32 HM_HH_getDeepestStolenLevel(GC_state s, struct HM_HierarchicalHeap* hh);
Word32 HM_HH_getShallowestPrivateLevel(GC_state s, struct HM_HierarchicalHeap* hh);
pointer HM_HH_getFrontier(struct HM_HierarchicalHeap* hh);
pointer HM_HH_getLimit(struct HM_HierarchicalHeap* hh);
double HM_HH_getLCRatio(struct HM_HierarchicalHeap* hh);
void HM_HH_maybeResizeLCHS(GC_state s, struct HM_HierarchicalHeap* hh);
void HM_HH_updateValues(struct HM_HierarchicalHeap* hh, pointer frontier);

#endif /* MLTON_GC_INTERNAL_FUNCS */

#endif /* HIERARCHICAL_HEAP_H_ */

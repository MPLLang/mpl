/* Copyright (C) 2014 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file hierarchical-heap.h
 *
 * @author Ram Raghunathan
 *
 * @brief
 * Utility functions for managing the HierarchicalHeap object
 */

#ifndef HIERARCHICAL_HEAP_H_
#define HIERARCHICAL_HEAP_H_

#if (defined (MLTON_GC_INTERNAL_TYPES))
/**
 * @brief
 * Represents a "node" of the hierarchical heap and contains various data
 * about its contents and structure
 *
 * HierarchicalHeap objects are normal objects with the following layout:
 *
 * header ::
 * padding ::
 * lastAllocatedChunk (void*) ::
 * savedFrontier (void*) ::
 * chunkList (void*) ::
 * sourceHH (objptr) ::
 * nextDerivedHH (objptr) ::
 * derivedHHList (objptr)
 *
 * There may be zero or more bytes of padding for alignment purposes.
 */
struct HM_HierarchicalHeap {
  void* lastAllocatedChunk; /**< The last allocated chunk, so that 'chunkList'
                             * does not have to be traversed to find it. */

  void* savedFrontier; /**< The saved frontier when returning to this
                        * hierarchical heap. */

  void* chunkList; /**< The list of chunks making up this heap. */

  objptr sourceHH; /**< The heap this object branched off of or BOGUS_OBJPTR
                      * if it is the first heap. */

  objptr nextDerivedHH; /**< The next heap in the 'derivedHHList' of
                         * the 'sourceHH'. This variable is the 'next'
                         * pointer for the intrusive linked list. */

  objptr derivedHHList; /**< The list of heaps that are derived from this
                         * heap. All heaps in this list should have their
                         * 'sourceHH' set to this object. */
} __attribute__((packed));

COMPILE_TIME_ASSERT(HM_HierarchicalHeap__packed,
                    sizeof (struct HM_HierarchicalHeap) ==
                    sizeof (void*) +
                    sizeof (void*) +
                    sizeof (void*) +
                    sizeof (objptr) +
                    sizeof (objptr) +
                    sizeof (objptr));

/* RAM_NOTE: should take GC_state argument once I get that back in */
static inline void HM_displayHierarchicalHeap (
    const struct HM_HierarchicalHeap* hh,
    FILE* stream);
static inline size_t HM_sizeofHierarchicalHeap (void);
static inline size_t HM_offsetofHierarchicalHeap (void);

/**
 * Appends the derived hierarchical heap to the derivedHHList of the source
 * hierarchical heap and sets relationships appropriately.
 *
 * @param sourceHH The source struct HM_HierarchicalHeap
 * @param derviedHH The struct HM_HierarchicalHeap set to be derived off of
 * 'sourceHH'
 */
PRIVATE void HM_appendDerivedHeap (struct HM_HierarchicalHeap* sourceHH,
                                   struct HM_HierarchicalHeap* derivedHH);

/**
 * Merges the specified hierarchical heap back into its source hierarchical
 * heap. Note that the specified heap must already be fully merged (i.e. its
 * derivedHHList should be empty)
 *
 * @param hh The struct HM_HierarchicalHeap* to merge back into its source.
 */
PRIVATE void HM_mergeIntoSourceHeap (struct HM_HierarchicalHeap* hh);

#else
struct HM_HierarchicalHeap;
#endif

#endif /* HIERARCHICAL_HEAP_H_ */

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
 * sourceHeap (objptr) ::
 * nextDerivativeHeap (objptr) ::
 * derivativeHeapList (objptr)
 *
 * There may be zero or more bytes of padding for alignment purposes.
 */
struct HeapManagement_HierarchicalHeap {
  void* lastAllocatedChunk; /**< The last allocated chunk, so that 'chunkList'
                             * does not have to be traversed to find it. */

  void* savedFrontier; /**< The saved frontier when returning to this
                        * hierarchical heap. */

  void* chunkList; /**< The list of chunks making up this heap. */

  objptr sourceHeap; /**< The heap this object branched off of or BOGUS_OBJPTR
                      * if it is the first heap. */

  objptr nextDerivativeHeap; /**< The next heap in the 'derivativeHeapList' of
                              * the 'sourceHeap'. This variable is the 'next'
                              * pointer for the intrusive linked list. */

  objptr derivativeHeapList; /**< The list of heaps that are derived from this
                              * heap. All heaps in this list should have their
                              * 'sourceHeap' set to this object. */
} __attribute__((packed));

COMPILE_TIME_ASSERT(HeapManagement_HierarchicalHeap__packed,
                    sizeof (struct HeapManagement_HierarchicalHeap) ==
                    sizeof (void*) +
                    sizeof (void*) +
                    sizeof (void*) +
                    sizeof (objptr) +
                    sizeof (objptr) +
                    sizeof (objptr));

/* RAM_NOTE: should take GC_state argument once I get that back in */
size_t HeapManagement_displayHierarchicalHeap (
    const struct HeapManagement_HierarchicalHeap* hierarchicalHeap,
    FILE* stream);
size_t HeapManagement_sizeofHierarchicalHeap (void);
size_t HeapManagement_offsetofHierarchicalHeap (void);

#else
struct HeapManagement_HierarchicalHeap;
#endif

#endif /* HIERARCHICAL_HEAP_H_ */

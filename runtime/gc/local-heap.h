/* Copyright (C) 2014 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file local-heap.h
 *
 * @author Ram Raghunathan
 *
 * @brief
 * The interface for managing local heaps
 */

#ifndef LOCAL_HEAP_H_
#define LOCAL_HEAP_H_

#if (defined (MLTON_GC_INTERNAL_FUNCS))
/**
 * This function enters the local heap of the currently running thread
 *
 * @param s The GC_state of the processor calling this function
 */
void HM_enterLocalHeap (GC_state s);

/**
 * This function exits the local heap of the currently running thread
 *
 * @param s The GC_state of the processor calling this function
 */
void HM_exitLocalHeap (GC_state s);

/**
 * This function ensures that the hierarchical heap has enough space
 *
 * @param s The GC_state to operate on
 * @param forceGC Whether or not to force a GC
 * @param bytesRequested The minimum number of bytes to leave free on return
 * @param ensureCurrentLevel Ensures that all future allocations (before the
 * next fork) occur on the current level.
 */
void HM_ensureHierarchicalHeapAssurances(GC_state s,
                                         bool forceGC,
                                         size_t bytesRequested,
                                         bool ensureCurrentLevel);
#endif /* MLTON_GC_INTERNAL_FUNCS */

#endif /* LOCAL_HEAP_H_ */

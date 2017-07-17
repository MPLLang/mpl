/* Copyright (C) 2014 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file global-heap.h
 *
 * @author Ram Raghunathan
 *
 * @brief
 * The Interface for interacting specifically with the global heap. These
 * functions are under the HM (HeapManagement) namespace
 */

#ifndef GLOBAL_HEAP_H_
#define GLOBAL_HEAP_H_

#if (defined (MLTON_GC_INTERNAL_BASIS))
/**
 * This function changes the runtime to use the global heap.
 */
PRIVATE void HM_enterGlobalHeap (void);

/**
 * This function changes the runtime to exit the global heap.
 */
PRIVATE void HM_exitGlobalHeap (void);

/**
 * This function unconditionally enters the global heap and exits the local heap
 *
 * @attention
 * Use with care! Designed to be used to unconditionally allocate in the local
 * heap
 *
 * @param inGlobalHeapCounter The counter to set for this enterGlobalHeap() call
 */
PRIVATE void HM_explicitEnterGlobalHeap(Word32 inGlobalHeapCounter);

/**
 * This function unconditionally exits the global heap and enters the local heap
 *
 * @attention
 * Use with care! Designed to be used to unconditionally allocate in the local
 * heap
 *
 * @return The former inGlobalHeapCounter
 */
PRIVATE Word32 HM_explicitExitGlobalHeap(void);
#endif /* MLTON_GC_INTERNAL_BASIS */

#if (defined (MLTON_GC_INTERNAL_FUNCS))
/**
 * This function tests if the current thread is in the global heap
 *
 * @param s The GC_state to test
 *
 * @return TRUE if in global heap, FALSE otherwise
 */
bool HM_inGlobalHeap (GC_state s);
#endif /* MLTON_GC_INTERNAL_FUNCS */

#endif /* GLOBAL_HEAP_H_ */

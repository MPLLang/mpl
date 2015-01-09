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

#if (defined (MLTON_GC_INTERNAL_FUNCS))

/**
 * This function changes the runtime to use the global heap.
 */
PRIVATE void HM_enterGlobalHeap (void);

/**
 * This function changes the runtime to exit the global heap.
 */
PRIVATE void HM_exitGlobalHeap (void);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

#endif /* GLOBAL_HEAP_H_ */

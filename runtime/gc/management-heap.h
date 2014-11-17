/* Copyright (C) 2014 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file management-heap.h
 *
 * @author Ram Raghunathan
 *
 * @brief
 * The Interface for interacting specifically with the management heap.
 */

#ifndef MANAGEMENT_HEAP_H_
#define MANAGEMENT_HEAP_H_

#if (defined (MLTON_GC_INTERNAL_FUNCS))

/**
 * This function changes the runtime to use the management heap.
 */
PRIVATE void GC_enterManagementHeap (void);

/**
 * This function changes the runtime to exit the management heap.
 */
PRIVATE void GC_exitManagementHeap (void);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */

#endif /* MANAGEMENT_HEAP_H_ */

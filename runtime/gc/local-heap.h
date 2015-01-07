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

struct HeapManagement_ChunkMetadata {
  void* next;
  size_t size;
} __attribute__((packed));

/**
 * This function enters the local heap of the currently running thread
 *
 * @param s The GC_state of the processor calling this function
 */
void HeapManagement_enterLocalHeap (GC_state s);

/**
 * This function exits the local heap of the currently running thread
 *
 * @param s The GC_state of the processor calling this function
 */
void HeapManagement_exitLocalHeap (GC_state s);

#endif /* LOCAL_HEAP_H_ */

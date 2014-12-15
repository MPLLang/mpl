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

struct HeapManagement_TaskHeap {
  void** head;
  void** lastAllocatedChunk;
};

struct HeapManagement_ChunkMetadata {
  void* next;
  size_t size;
} __attribute__((packed));

void HeapManagement_setLocalHeapFrontierAndLimit (
    GC_state s, struct HeapManagement_TaskHeap* taskHeap);

#endif /* LOCAL_HEAP_H_ */

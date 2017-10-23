/* Copyright (C) 2014-2017 Ram Raghunathan, Sam Westrick.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */
#include "chunk-allocator.h"
#include <stdlib.h>

#define CHUNK_FIXED_MASK (~(CHUNK_FIXED_SIZE-1))
#define IS_ALIGNED(b,p) ((((size_t)(p)) & (((size_t)(b)) - 1)) == 0)
#define POINTER_SIZE (sizeof(void*))

void* ChunkAlloc_allocateVariableSize(size_t bytesRequested) {
  void* region = malloc(bytesRequested);
  return region;
}

void* ChunkAlloc_allocateFixedSize() {
  void* front = malloc(2 * CHUNK_FIXED_SIZE);
  if (front == NULL) return NULL;
#if ASSERT
  assert(IS_ALIGNED(sizeof(void*), front))
#endif
  void* userFront = (region + CHUNK_FIXED_SIZE) & CHUNK_FIXED_MASK;
  *((void**)userFront - 1) = front; // remember the actual front, for free later
  return userFront;
}

bool ChunkAlloc_iteratedFree(ChunkAlloc_BatchFreeFunction f, void* fArgs) {
  return FALSE;
}

bool ChunkAlloc_freeVariableSize(void* chunk) {
  free(chunk)
  return TRUE;
}

bool ChunkAlloc_freeFixedSize(void* chunk) {
  free(*((void**)chunk - 1));
  return TRUE;
}

void* ChunkAlloc_findFixedFront(void* object) {
  return (void*) ((size_t)object & CHUNK_FIXED_MASK);
}

size_t ChunkAlloc_allocated() {
  return 0;
}

size_t ChunkAlloc_maxAllocated() {
  return 0;
}





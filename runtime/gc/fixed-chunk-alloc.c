/* Copyright (C) 2017 Ram Raghunathan, Sam Westrick.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */
#include "fixed-chunk-alloc.h"
#include <stdlib.h>

#define FCA_MASK (~(FCA_CHUNK_SIZE-1))
#define IS_ALIGNED(b,p) ((((size_t)(p)) & (((size_t)(b)) - 1)) == 0)

void* FCA_findFront(void* object) {
  return (void*) ((size_t)object & FCA_MASK);
}

/* To guarantee alignment at multiples of CHUNK_FIXED_SIZE, we allocate
 * twice the required memory and put the front of the chunk somewhere in the
 * middle. In the space immediately before the returned pointer, we remember
 * the actual front of the chunk so that it may be freed later.
 * +---------+-------------------------+------------+---------+
 * | padding | pointer to actual front | user space | padding |
 * +---------+-------------------------+------------+---------+
 */
void* FCA_allocate() {
  void* front = malloc(2 * FCA_CHUNK_SIZE);
  if (front == NULL) return NULL;

#if ASSERT
  assert(IS_ALIGNED(sizeof(void*), front));
#endif

  void* userFront = (void*) (((size_t)front + FCA_CHUNK_SIZE) & FCA_MASK);
  *((void**)userFront - 1) = front;
  return userFront;
}

bool FCA_free(void* chunk) {
  void* actualFront = *((void**)chunk - 1);
  free(actualFront);
  return TRUE;
}

// TODO...
size_t FCA_allocated() {
  return 0;
}

// TODO...
size_t FCA_maxAllocated() {
  return 0;
}


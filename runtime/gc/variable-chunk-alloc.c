/* Copyright (C) 2017 Ram Raghunathan, Sam Westrick.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */
#include "variable-chunk-alloc.h"
#include <stdlib.h>

void* VCA_allocate(size_t bytesRequested) {
  void* front = malloc(bytesRequested);
  return front;
}

bool VCA_free(void* chunk) {
  free(chunk);
  return TRUE;
}

// TODO...
size_t VCA_allocated(void) {
  return 0;
}

// TODO...
size_t VCA_maxAllocated(void) {
  return 0;
}

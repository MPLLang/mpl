/* Copyright (C) 2017 Ram Raghunathan, Sam Westrick.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* variable-chunk-alloc.h:
 * A black-box interface for allocating and freeing chunks of variable size,
 * intended for storing a single large object per chunk. These chunks have no
 * particular alignment and do not support a `findFront` operation, since
 * the client will only ever need a reference to the front of the chunk.
 */

#ifndef VARIABLE_CHUNK_ALLOC_H_
#define VARIABLE_CHUNK_ALLOC_H_

void* VCA_allocate(size_t bytesRequested);
bool VCA_free(void* chunk);
size_t VCA_allocated(void);
size_t VCA_maxAllocated(void);

#endif

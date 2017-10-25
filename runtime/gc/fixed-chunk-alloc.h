/* Copyright (C) 2017 Ram Raghunathan, Sam Westrick.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/* fixed-chunk-alloc.h:
 * A black-box interface for allocating and freeing chunks of fixed size,
 * intended for storing many small objects. These chunks will be aligned
 * at multiples of FCA_CHUNK_SIZE, so that looking up chunk metadata (with
 * FCA_findFront) is fast.
 */

#ifndef FIXED_CHUNK_ALLOC_H_
#define FIXED_CHUNK_ALLOC_H_

/* Must be a power of 2 */
#define FCA_CHUNK_SIZE (4 * 1024ULL)

void* FCA_allocate(void);
bool FCA_free(void* chunk);
void* FCA_findFront(void* object);
size_t FCA_allocated(void);
size_t FCA_maxAllocated(void);

#endif


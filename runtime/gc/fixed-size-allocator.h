/* Copyright (C) 2021 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _FIXED_SIZE_ARENA_H_
#define _FIXED_SIZE_ARENA_H_

#if (defined (MLTON_GC_INTERNAL_TYPES))

struct FixedSizeElement {
  struct FixedSizeElement *nextFree;
};

typedef struct FixedSizeAllocator {
  /** The size of each element.
    * Must be >= sizeof(struct FixedSizeElement), because when an object is
    * deallocated, that space is reuse to hold a freelist of free elements.
    */
  size_t fixedSize;

  /** A bit of a hack. I just want quick access to pages to store elements.
    * I'll reuse the frontier mechanism inherent to chunks to remember which
    * portions of chunks are currently in use by the allocator.
    */
  struct HM_chunkList buffer;

  struct FixedSizeElement *freeList;

} *FixedSizeAllocator;

#else

struct FixedSizeAllocator;
typedef struct FixedSizeAllocator *FixedSizeAllocator;

#endif


#if (defined (MLTON_GC_INTERNAL_FUNCS))

void initFixedSizeAllocator(FixedSizeAllocator fsa, size_t fixedSize);
void* allocateFixedSize(FixedSizeAllocator fsa);
void freeFixedSize(FixedSizeAllocator fsa, void* elem);

#endif



#endif // _FIXED_SIZE_ARENA_H_

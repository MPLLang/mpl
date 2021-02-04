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

  /** Some statistics. Can calculate e.g.
    *   numFreed = numLocalFreed + numSharedFreed
    *   numCurrentlyInUse = numAllocated - numFreed
    *   currentCapacity = totalSize(buffer) / fixedSize
    *   spaceUtilization = numCurrentlyInUse / currentCapacity
    */
  size_t numAllocated;
  size_t numLocalFreed;
  size_t numSharedFreed;

  /** A bit of a hack. I just want quick access to pages to store elements.
    * I'll reuse the frontier mechanism inherent to chunks to remember which
    * portions of chunks are currently in use by the allocator. Also, the
    * startGap mechanism is nice for adding some additional metadata...
    */
  struct HM_chunkList buffer;

  /** The fast free-list, which is not-safe-for-concurrency.
    */
  struct FixedSizeElement *freeList;

  /** The slow free-list, which is safe-for-concurrency. (When someone else
    * owns an object, we have to use this list, because the
    * owner's allocator could concurrently be in use.)
    *
    * TODO: flat-combining for these frees? I.e. use a bump-buffer of elements
    * returned, and then when a new allocation request comes in, move all of
    * these elements to the fast free list.
    */
  struct FixedSizeElement *sharedFreeList;

} *FixedSizeAllocator;

#else

struct FixedSizeAllocator;
typedef struct FixedSizeAllocator *FixedSizeAllocator;

#endif


#if (defined (MLTON_GC_INTERNAL_FUNCS))

/** Initialize [fsa] to be able to allocate objects of size [fixedSize].
  * You should never re-initialize an allocator.
  */
void initFixedSizeAllocator(FixedSizeAllocator fsa, size_t fixedSize);


/** Allocate an object of the size specified when the allocator was initialized.
  * Thread-safe, as long as the [fsa]'s given as argument are distinct.
  * (But two concurrent threads should NOT allocate from the same [fsa] at the
  * same time.)
  */
void* allocateFixedSize(FixedSizeAllocator fsa);


/** Free [elem]. Standard assumptions: [elem] must have been returned by some
  * call to [allocateFixedSize], and don't double-free!
  *
  * This function "returns" the allocated space to its original allocator. This
  * is safe for concurrency, i.e. two concurrent threads are allowed to
  * simultaneously free two different elements which where allocated by the
  * same allocator.
  *
  * The argument [myfsa] is for improved performance. If [elem] belongs to
  * [myfsa], it will be pushed onto the fast (not-safe-for-concurrency)
  * free-list. This way, if a processor frees an object that it itself
  * allocated, freeing will be fast!
  */
void freeFixedSize(FixedSizeAllocator myfsa, void* elem);


size_t numFixedSizeAllocated(FixedSizeAllocator fsa);
size_t numFixedSizeFreed(FixedSizeAllocator fsa);
size_t numFixedSizeSharedFreed(FixedSizeAllocator fsa);
size_t numFixedSizeCurrentlyInUse(FixedSizeAllocator fsa);
size_t currentFixedSizeCapacity(FixedSizeAllocator fsa);
double currentFixedSizeSpaceUtilization(FixedSizeAllocator fsa);

#endif



#endif // _FIXED_SIZE_ARENA_H_

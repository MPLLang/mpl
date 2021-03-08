/* Copyright (C) 2021 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/** Block allocator, inspired by Hoard [1]. Each block is one page.
  *
  * [1] Emery D. Berger, Kathryn S. McKinley, Robert D. Blumofe, and
  * Paul R. Wilson. Hoard: A Scalable Memory Allocator for Multithreaded
  * Applications. ASPLOS 2000.
  */

#ifndef BLOCK_ALLOCATOR_H_
#define BLOCK_ALLOCATOR_H_

#if (defined (MLTON_GC_INTERNAL_TYPES))

/** Blocks are identified with numbers 1 through SUPERBLOCK_SIZE-1.
  * (Block 0 is reserved for superblock metadata).
  * WARNING: Don't change the SUPERBLOCK_SIZE without also changing the
  * size of a BlockId and the number of size classes! See for example the
  * commented-out definitions in the following lines, for other superblock
  * sizes.
  */
#define SUPERBLOCK_SIZE 256
#define NUM_SIZE_CLASSES 8
typedef uint8_t BlockId;

// #define SUPERBLOCK_SIZE 1024
// #define NUM_SIZE_CLASSES 10
// typedef uint16_t BlockId;

// #define SUPERBLOCK_SIZE 512
// #define NUM_SIZE_CLASSES 9
// typedef uint16_t BlockId;

// #define SUPERBLOCK_SIZE 128
// #define NUM_SIZE_CLASSES 7
// typedef uint8_t BlockId;

// #define SUPERBLOCK_SIZE 64
// #define NUM_SIZE_CLASSES 6
// typedef uint8_t BlockId;


struct SuperBlockList;

typedef struct SuperBlock {

  pthread_mutex_t superBlockLock;

  struct SuperBlockList *owner;

  /** Superblocks only allocate in groups of 2^sizeClass contiguous blocks.
    * E.g. sizeClass == 2 means that blocks are allocated in groups of 4:
    *   {1 2 3 4}  {5 6 7 8}  {9 10 11 12} ...
    * In the freeFlags and freeList, the only block-ids you will see are
    * the starting block-ids (1, 5, 9, etc in the above example)
    */
  int sizeClass;

  /** Number of blocks in this superblock that are currently free. For example
    * When we perform an alloction in this superblock, we do:
    *   numBlocksFree -= 2^sizeClass;
    */
  int numBlocksFree;

  /** The freelist is LIFO, for caching/efficiency. List is terminated by
    * nextFree[x] == 0
    */
  BlockId firstFree;
  BlockId nextFree[SUPERBLOCK_SIZE];

  /** Each fullness group within a size-class is a doubly-linked list. */
  struct SuperBlock *nextSuperBlock;
  struct SuperBlock *prevSuperBlock;

} *SuperBlock;


typedef struct SuperBlockList {

  SuperBlock firstSuperBlock;
  pthread_mutex_t listLock;

} *SuperBlockList;


#define NUM_FULLNESS_GROUPS 3

enum FullnessGroup {
  NEARLY_FULL = 0,
  SOMEWHAT_FULL = 1,
  NEARLY_EMPTY = 2,
  COMPLETELY_EMPTY = 3
};


typedef struct BlockAllocator {

  /** There are 3 fullness groups in each size class:
    *   0 is nearly full, i.e. at least 1-emptinessFraction in use
    *   1 is neither nearly full nor nearly empty
    *   2 is nearly empty, i.e. less than emptinessFraction in use
    */
  struct SuperBlockList sizeClassFullnessGroup[NUM_SIZE_CLASSES][NUM_FULLNESS_GROUPS];

  /** Completely empty superblocks are special because these can be
    * reused for any size class.
    */
  struct SuperBlockList completelyEmptyGroup;

} *BlockAllocator;


#else

struct BlockAllocator;
typedef struct BlockAllocator * BlockAllocator;

#endif // MLTON_GC_INTERNAL_TYPES



#if (defined (MLTON_GC_INTERNAL_FUNCS))

BlockAllocator initGlobalBlockAllocator(GC_state s);
void initLocalBlockAllocator(GC_state s, BlockAllocator globalAllocator);

/** Get a pointer to the start of some number of free contiguous blocks. */
pointer allocateBlocks(GC_state s, size_t numBlocks);

/** Free a group of contiguous blocks. */
void freeBlocks(GC_state s, pointer blockStart, size_t numBlocks);

#endif

#endif // BLOCK_ALLOCATOR_H_

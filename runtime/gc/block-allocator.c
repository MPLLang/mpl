/* Copyright (C) 2021 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static void initBlockAllocator(BlockAllocator ball) {
  for (int i = 0; i < NUM_SIZE_CLASSES; i++) {
    for (int j = 0; j < NUM_FULLNESS_GROUPS; j++) {
      ball->sizeClassFullnessGroup[i][j].firstSuperBlock = NULL;
      pthread_mutex_init(&(ball->sizeClassFullnessGroup[i][j].listLock), NULL);
    }
  }

  ball->completelyEmptyGroup.firstSuperBlock = NULL;
  pthread_mutex_init(&(ball->completelyEmptyGroup.listLock), NULL);
}


BlockAllocator initGlobalBlockAllocator(GC_state s) {
  s->blockAllocatorGlobal = malloc(sizeof(struct BlockAllocator));
  initBlockAllocator(s->blockAllocatorGlobal);
  return s->blockAllocatorGlobal;
}


void initLocalBlockAllocator(GC_state s, BlockAllocator globalBall) {
  // s->controls->blockSize;
  s->blockAllocatorGlobal = globalBall;
  s->blockAllocatorLocal = malloc(sizeof(struct BlockAllocator));
  initBlockAllocator(s->blockAllocatorLocal);
}


static inline void lockList(SuperBlockList list) {
  pthread_mutex_lock(&(list->listLock));
}

static inline void unlockList(SuperBlockList list) {
  pthread_mutex_unlock(&(list->listLock));
}

static inline void lockSuperBlock(SuperBlock sb) {
  pthread_mutex_lock(&(sb->superBlockLock));
}

static inline void unlockSuperBlock(SuperBlock sb) {
  pthread_mutex_unlock(&(sb->superBlockLock));
}


static int sizeClass(size_t numBlocks) {
  assert(numBlocks <= SUPERBLOCK_SIZE / 2);

  int class = 0;
  while (((size_t)1 << class) < numBlocks) {
    class++;
  }

  assert(class < NUM_SIZE_CLASSES);
  return class;
}


static void unlinkSuperBlock(SuperBlock sb) {
  SuperBlockList list = sb->owner;
  sb->owner = NULL;

  if (NULL == sb->prevSuperBlock) {
    assert(list->firstSuperBlock == sb);
    list->firstSuperBlock = sb->nextSuperBlock;
  }
  else {
    assert(list->firstSuperBlock != sb);
    sb->prevSuperBlock->nextSuperBlock = sb->nextSuperBlock;
  }

  if (NULL != sb->nextSuperBlock) {
    sb->nextSuperBlock->prevSuperBlock = sb->prevSuperBlock;
  }

  sb->prevSuperBlock = NULL;
  sb->nextSuperBlock = NULL;
  sb->owner = NULL;
}


static void prependSuperBlock(SuperBlockList list, SuperBlock sb) {
  assert(sb->prevSuperBlock == NULL);
  assert(sb->nextSuperBlock == NULL);
  assert(sb->owner == NULL);

  sb->nextSuperBlock = list->firstSuperBlock;

  if (list->firstSuperBlock != NULL) {
    list->firstSuperBlock->prevSuperBlock = sb;
  }

  list->firstSuperBlock = sb;
  sb->owner = list;
}


/** Change the size class of this super-block, and put it into a clean state.
  * Only call this if you already have the sb->superBlockLock...
  */
static void setSuperBlockSizeClass(SuperBlock sb, int sizeClass) {
  /** This block should be currently unassigned to a size class and be
    * completely free.
    */
  assert(sb->nextSuperBlock == NULL);
  assert(sb->prevSuperBlock == NULL);
  assert(sb->owner == NULL);
  assert(sb->numBlocksFree == SUPERBLOCK_SIZE);

  sb->allocationSize = (1 << sizeClass);
  sb->firstFree = 1;

  BlockId cursor = 1;
  while (SUPERBLOCK_SIZE - cursor > sb->allocationSize) {
    sb->nextFree[cursor] = cursor + sb->allocationSize;
    cursor = cursor + sb->allocationSize;
  }
  sb->nextFree[cursor] = 0;
}


static inline SuperBlock findSuperBlockFront(GC_state s, pointer p) {
  return
    (SuperBlock)(uintptr_t)
    alignDown((size_t)p, s->controls->blockSize * SUPERBLOCK_SIZE);
}


static SuperBlock mmapNewSuperBlock(GC_state s, int sizeClass) {
  /** This is an annoying hack, to make sure that we have an aligned start to
    * the superblock. I wish mmap could take an alignment parameter, to
    * enforce that the result was aligned to a certain number of pages. But
    * instead what we do here is map twice the desired size, to guarantee that
    * somewhere within that space, there is a superblock-sized amount of
    * of aligned memory.
    */
  pointer start =
    GC_mmapAnon(NULL, s->controls->blockSize * SUPERBLOCK_SIZE * 2);
  if (MAP_FAILED == start) {
    DIE("I ran out of space!");
  }

  pointer alignedStart =
    (pointer)(uintptr_t)
    align((uintptr_t)start, s->controls->blockSize * SUPERBLOCK_SIZE);

  assert(alignedStart + s->controls->blockSize * SUPERBLOCK_SIZE
         <= start + s->controls->blockSize * SUPERBLOCK_SIZE * 2);
  assert((pointer)findSuperBlockFront(s, alignedStart) == alignedStart);
  assert((pointer)findSuperBlockFront(s, alignedStart + s->controls->blockSize * (SUPERBLOCK_SIZE-1)) == alignedStart);

  SuperBlock sb = (SuperBlock)alignedStart;
  pthread_mutex_init(&(sb->superBlockLock), NULL);
  sb->owner = NULL;
  sb->nextSuperBlock = NULL;
  sb->prevSuperBlock = NULL;
  sb->numBlocksFree = SUPERBLOCK_SIZE;
  setSuperBlockSizeClass(sb, sizeClass);
  return sb;
}


static pointer allocateInSuperBlock(
  GC_state s,
  SuperBlock sb,
  ARG_USED_FOR_ASSERT int sizeClass)
{
  assert(sb->allocationSize == (1 << sizeClass));
  assert(sb->numBlocksFree >= sb->allocationSize);

  BlockId resultId = sb->firstFree;
  sb->firstFree = sb->nextFree[resultId];
  sb->numBlocksFree -= sb->allocationSize;

  assert(isAligned(resultId-1, sb->allocationSize));
  pointer result = ((pointer)sb) + (resultId * s->controls->blockSize);
  return result;
}


static void deallocateInSuperBlock(
  SuperBlock sb,
  BlockId blockId,
  ARG_USED_FOR_ASSERT int sizeClass)
{
  assert(sb->allocationSize == (1 << sizeClass));
  assert(sb->numBlocksFree <= SUPERBLOCK_SIZE - sb->allocationSize);
  assert(isAligned(blockId-1, sb->allocationSize));

  sb->nextFree[blockId] = sb->firstFree;
  sb->firstFree = blockId;
  sb->numBlocksFree += sb->allocationSize;
}


static SuperBlock findGoodSuperBlockAndLockIt(SuperBlockList list) {
  SuperBlock sb = list->firstSuperBlock;

  while (NULL != sb) {
    lockSuperBlock(sb);

    if (0 != sb->firstFree) {
      return sb;
    }

    unlockSuperBlock(sb);
    sb = sb->nextSuperBlock;
  }

  return NULL;
}


static enum FullnessGroup fullness(GC_state s, SuperBlock sb) {
  if (sb->numBlocksFree == 0)
    return COMPLETELY_EMPTY;

  float currentEmptiness = (float)sb->numBlocksFree / (float)SUPERBLOCK_SIZE;

  if (currentEmptiness >= 1.0 - s->emptinessFraction)
    return NEARLY_FULL;
  if (currentEmptiness < s->emptinessFraction)
    return NEARLY_EMPTY;

  return SOMEWHAT_FULL;
}


void putSuperBlockInFullnessGroup(
  BlockAllocator ball,
  int class,
  enum FullnessGroup fg,
  SuperBlock sb)
{
  SuperBlockList list;

  if (fg == COMPLETELY_EMPTY)
    list = &(ball->completelyEmptyGroup);
  else
    list = &(ball->sizeClassFullnessGroup[class][fg]);

  lockList(list);
  prependSuperBlock(list, sb);
  unlockList(list);
}


/** Look in src, and try to allocate. If we find a good superblock to do an
  * allocation, put this superblock in dst, and return a pointer to the fresh
  * space. Otherwise, don't change anything and return NULL.
  */
static pointer tryAllocateAndAdjustSuperBlocks(
  GC_state s,
  BlockAllocator src,
  BlockAllocator dst,
  int class)
{
  /** First, try to find blocks in partially used superblocks. */
  for (enum FullnessGroup fg = 0; fg < NUM_FULLNESS_GROUPS; fg++) {
    SuperBlockList list = &(src->sizeClassFullnessGroup[class][fg]);
    lockList(list);
    SuperBlock sb = findGoodSuperBlockAndLockIt(list);

    if (NULL == sb) {
      unlockSuperBlock(sb);
      continue;
    }

    pointer result = allocateInSuperBlock(s, sb, class);
    enum FullnessGroup newfg = fullness(s, sb);

    if (newfg == fg) {
      // No need to move. We're done.
      unlockSuperBlock(sb);
      unlockList(list);
      return result;
    }

    unlinkSuperBlock(sb);
    unlockList(list);
    putSuperBlockInFullnessGroup(dst, class, newfg, sb);
    unlockSuperBlock(sb);
    return result;
  }

  /** Next, see if there's a completely empty superblock available. */
  SuperBlockList completelyEmpty = &(src->completelyEmptyGroup);
  lockList(completelyEmpty);
  if (completelyEmpty->firstSuperBlock != NULL) {
    SuperBlock sb = completelyEmpty->firstSuperBlock;
    lockSuperBlock(sb);
    unlinkSuperBlock(sb);
    unlockList(completelyEmpty);

    setSuperBlockSizeClass(sb, class);
    pointer result = allocateInSuperBlock(s, sb, class);
    putSuperBlockInFullnessGroup(dst, class, fullness(s, sb), sb);
    unlockSuperBlock(sb);
    return result;
  }

  unlockList(completelyEmpty);
  return NULL;
}


pointer allocateBlocks(GC_state s, size_t numBlocks) {
  BlockAllocator global = s->blockAllocatorGlobal;
  BlockAllocator local = s->blockAllocatorLocal;

  if (numBlocks > SUPERBLOCK_SIZE / 2) {
    pointer start = GC_mmapAnon(NULL, s->controls->blockSize * numBlocks);
    if (MAP_FAILED == start) {
      return NULL;
    }
    return start;
  }

  int class = sizeClass(numBlocks);

  pointer result;

  /** Look in local first. */
  result = tryAllocateAndAdjustSuperBlocks(s, local, local, class);
  if (result != NULL)
    return result;

  /** If that fails, try global. */
  result = tryAllocateAndAdjustSuperBlocks(s, global, local, class);
  if (result != NULL)
    return result;

  /** If both local and global fail, we need to mmap a new superchunk. */
  SuperBlock sb = mmapNewSuperBlock(s, class);
  result = allocateInSuperBlock(s, sb, class);
  putSuperBlockInFullnessGroup(local, class, fullness(s, sb), sb);
  return result;
}


void freeBlocks(GC_state s, pointer blockStart, size_t numBlocks) {
  // BlockAllocator global = s->blockAllocatorGlobal;
  // BlockAllocator local = s->blockAllocatorLocal;

  if (numBlocks > SUPERBLOCK_SIZE / 2) {
    GC_release(blockStart, s->controls->blockSize * numBlocks);
    return;
  }

  int class = sizeClass(numBlocks);

  SuperBlock sb = findSuperBlockFront(s, blockStart);
  size_t blockId =
    ((size_t)blockStart - (size_t)(pointer)sb) / s->controls->blockSize;
  assert(1 <= blockId && blockId < SUPERBLOCK_SIZE);
  lockSuperBlock(sb);
  SuperBlockList owner = sb->owner;
  lockList(owner);

  // enum FullnessGroup fg = fullness(s, sb);
  deallocateInSuperBlock(sb, blockId, class);
  unlinkSuperBlock(sb);
  // enum FullnessGroup newfg = fullness(s, sb);

  // if (fg == newfg) {
    // stays in same list... just move-to-front (heuristic) and then done.
    prependSuperBlock(owner, sb);
    unlockSuperBlock(sb);
    unlockList(owner);
    return;
  // }

  // putSuperBlockInFullnessGroup(dst, class, newfg, sb);
}

#endif

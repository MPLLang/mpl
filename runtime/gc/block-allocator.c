/* Copyright (C) 2021 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static void initBlockAllocator(BlockAllocator ball) {
  for (int i = 0; i < NUM_SIZE_CLASSES; i++) {
    for (int j = 0; j < NUM_FULLNESS_GROUPS; j++) {
      SuperBlockList list = &(ball->sizeClassFullnessGroup[i][j]);
      list->firstSuperBlock = NULL;
    }
  }

  SuperBlockList elist = &(ball->completelyEmptyGroup);
  elist->firstSuperBlock = NULL;

  ball->numBlocks = 0;
  ball->numBlocksInUse = 0;
  ball->firstFreedByOther = NULL;
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


static int sizeClass(size_t numBlocks) {
  assert(numBlocks <= (SUPERBLOCK_SIZE-1) / 2);

  int class = 0;
  while (((size_t)1 << class) < numBlocks) {
    class++;
  }

  assert(class < NUM_SIZE_CLASSES);
  return class;
}


static void unlinkSuperBlock(SuperBlockList list, SuperBlock sb) {
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
}


static void prependSuperBlock(SuperBlockList list, SuperBlock sb) {
  assert(sb->prevSuperBlock == NULL);
  assert(sb->nextSuperBlock == NULL);

  sb->nextSuperBlock = list->firstSuperBlock;

  if (list->firstSuperBlock != NULL) {
    list->firstSuperBlock->prevSuperBlock = sb;
  }

  list->firstSuperBlock = sb;
}


/** Change the size class of this super-block, and put it into a clean state.
  * Only call this if you already have the sb->superBlockLock...
  */
static void setSuperBlockSizeClass(GC_state s, SuperBlock sb, int sizeClass) {
  /** This block should be currently unassigned to a size class and be
    * completely free.
    */
  assert(sb->nextSuperBlock == NULL);
  assert(sb->prevSuperBlock == NULL);
  assert(sb->numBlocksFree == (SUPERBLOCK_SIZE-1));

  int allocationSize = 1 << sizeClass;
  sb->sizeClass = sizeClass;

  FreeBlock *cursor = &(sb->firstFree);
  pointer next = (pointer)sb + s->controls->blockSize;
  pointer limit = (pointer)sb + SUPERBLOCK_SIZE * s->controls->blockSize;
  while ( (next + s->controls->blockSize * allocationSize) <= limit) {
    FreeBlock b = (FreeBlock)next;
    b->container = sb;
    *cursor = b;
    cursor = &(b->nextFree);
    next = next + s->controls->blockSize * allocationSize;
  }
  *cursor = NULL;
}


static SuperBlock mmapNewSuperBlock(
  GC_state s,
  BlockAllocator ball,
  int sizeClass)
{
  pointer start = GC_mmapAnon(NULL, s->controls->blockSize * SUPERBLOCK_SIZE);
  if (MAP_FAILED == start) {
    DIE("I ran out of space!");
  }
  assert(isAligned((size_t)start, s->controls->blockSize));

  SuperBlock sb = (SuperBlock)start;
  sb->owner = ball;
  sb->nextSuperBlock = NULL;
  sb->prevSuperBlock = NULL;
  sb->numBlocksFree = SUPERBLOCK_SIZE-1;
  setSuperBlockSizeClass(s, sb, sizeClass);
  return sb;
}


static Blocks allocateInSuperBlock(
  ARG_USED_FOR_ASSERT GC_state s,
  SuperBlock sb,
  int sizeClass)
{
  if (sb->numBlocksFree == SUPERBLOCK_SIZE) {
    // It's completely empty! We can reuse.
    setSuperBlockSizeClass(s, sb, sizeClass);
  }

  assert(sb->sizeClass == sizeClass);
  assert(sb->numBlocksFree >= (1 << sizeClass));

  FreeBlock result = sb->firstFree;
  assert(result != NULL);
  // assert(findSuperBlockFront(s, (pointer)result) == sb);
  assert( ((size_t)((pointer)result - (pointer)sb) / s->controls->blockSize - 1) % (1 << sizeClass) == 0);

  sb->firstFree = result->nextFree;
  sb->numBlocksFree -= (1 << sb->sizeClass);

  assert(sb->owner != NULL);
  sb->owner->numBlocksInUse += (1 << sb->sizeClass);

  Blocks bs = (Blocks)result;
  bs->container = sb;
  bs->numBlocks = (1 << sb->sizeClass);

  return bs;
}


static void deallocateInSuperBlock(
  ARG_USED_FOR_ASSERT GC_state s,
  SuperBlock sb,
  FreeBlock block,
  ARG_USED_FOR_ASSERT int sizeClass)
{
  assert(sb->sizeClass == sizeClass);
  assert(sb->numBlocksFree <= SUPERBLOCK_SIZE - 1 - (1 << sb->sizeClass));
  assert( ((size_t)((pointer)block - (pointer)sb) / s->controls->blockSize - 1) % (1 << sizeClass) == 0);
  // assert(findSuperBlockFront(s, (pointer)block) == sb);

  block->nextFree = sb->firstFree;
  sb->firstFree = block;
  sb->numBlocksFree += (1 << sb->sizeClass);

  assert(sb->owner != NULL);
  assert(sb->owner->numBlocksInUse >= ((size_t)1 << sb->sizeClass));
  sb->owner->numBlocksInUse -= (1 << sb->sizeClass);
}


static enum FullnessGroup fullness(GC_state s, SuperBlock sb) {
  size_t free = sb->numBlocksFree;

  if (free < ((size_t)1 << sb->sizeClass))
    return COMPLETELY_FULL;

  if (free == SUPERBLOCK_SIZE)
    return COMPLETELY_EMPTY;

  float currentEmptiness = (float)free / (float)SUPERBLOCK_SIZE;

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

  prependSuperBlock(list, sb);
}


static Blocks tryAllocateAndAdjustSuperBlocks(
  GC_state s,
  BlockAllocator ball,
  int class)
{
  SuperBlockList targetList = NULL;

  /** First, try to find blocks in partially used superblocks. */
  for (enum FullnessGroup fg = NEARLY_FULL; fg < NUM_FULLNESS_GROUPS; fg++) {
    SuperBlockList list = &(ball->sizeClassFullnessGroup[class][fg]);

    if (list->firstSuperBlock != NULL) {
      assert(list->firstSuperBlock->firstFree != NULL);
      targetList = list;
      break;
    }
  }

  /** Next, try a completely empty */
  if (targetList == NULL && ball->completelyEmptyGroup.firstSuperBlock != NULL) {
    targetList = &(ball->completelyEmptyGroup);
  }

  /** At this point, if we haven't found a superblock, we're screwed. */
  if (targetList == NULL) {
    return NULL;
  }

  SuperBlock sb = targetList->firstSuperBlock;
  assert( sb != NULL );
  enum FullnessGroup fg = fullness(s, sb);
  Blocks result = allocateInSuperBlock(s, sb, class);
  enum FullnessGroup newfg = fullness(s, sb);

  if (fg != newfg) {
    unlinkSuperBlock(targetList, sb);
    putSuperBlockInFullnessGroup(ball, class, newfg, sb);
  }

  return result;
}


static void localFreeBlocks(GC_state s, SuperBlock sb, FreeBlock b) {
  BlockAllocator ball = sb->owner;
  assert(ball != NULL);
  assert(ball == s->blockAllocatorLocal);

  enum FullnessGroup fg = fullness(s, sb);
  unlinkSuperBlock(&(ball->sizeClassFullnessGroup[sb->sizeClass][fg]), sb);
  deallocateInSuperBlock(s, sb, b, sb->sizeClass);
  putSuperBlockInFullnessGroup(ball, sb->sizeClass, fullness(s, sb), sb);
}


static void clearOutOtherFrees(GC_state s) {
  BlockAllocator local = s->blockAllocatorLocal;
  FreeBlock topElem = local->firstFreedByOther;

  if (NULL != topElem) {
    while (TRUE) {
      if (__sync_bool_compare_and_swap(&(local->firstFreedByOther), topElem, NULL))
        break;
      topElem = local->firstFreedByOther;
    }
  }

  while (topElem != NULL) {
    FreeBlock next = topElem->nextFree;
    SuperBlock sb = topElem->container;
    assert( sb->owner == local );
    localFreeBlocks(s, sb, topElem);

    topElem = next;
  }
}


Blocks allocateBlocks(GC_state s, size_t numBlocks) {
  BlockAllocator local = s->blockAllocatorLocal;

  if (numBlocks > (SUPERBLOCK_SIZE-1) / 2) {
    pointer start = GC_mmapAnon(NULL, s->controls->blockSize * numBlocks);
    if (MAP_FAILED == start) {
      return NULL;
    }
    if (!isAligned((size_t)start, s->controls->blockSize)) {
      DIE("whoops, mmap didn't align by the block-size.");
    }

    Blocks bs = (Blocks)start;
    bs->container = NULL;
    bs->numBlocks = numBlocks;
    return bs;
  }

  clearOutOtherFrees(s);

  int class = sizeClass(numBlocks);

  /** Look in local first. */
  Blocks result = tryAllocateAndAdjustSuperBlocks(s, local, class);
  if (result != NULL) {
    return result;
  }

  /** If both local fails, we need to mmap a new superchunk. */
  SuperBlock sb = mmapNewSuperBlock(s, local, class);
  local->numBlocks += SUPERBLOCK_SIZE;
  result = allocateInSuperBlock(s, sb, class);
  putSuperBlockInFullnessGroup(local, class, fullness(s, sb), sb);
  return result;
}


void freeBlocks(GC_state s, Blocks bs) {
  // BlockAllocator global = s->blockAllocatorGlobal;
  BlockAllocator local = s->blockAllocatorLocal;

  size_t numBlocks = bs->numBlocks;
  SuperBlock sb = bs->container;
  pointer blockStart = (pointer)bs;

  if (numBlocks > (SUPERBLOCK_SIZE-1) / 2) {
    assert(sb == NULL);
    GC_release(blockStart, s->controls->blockSize * numBlocks);
    return;
  }

  FreeBlock elem = (FreeBlock)blockStart;
  elem->container = sb;
  BlockAllocator owner = sb->owner;
  assert( owner != NULL );
  assert( sb->sizeClass == sizeClass(numBlocks) );

  if (owner == local) {
    localFreeBlocks(s, sb, elem);
    return;
  }

  /** Otherwise, enqueue for the other proc to handle. */
  while (TRUE) {
    FreeBlock oldVal = owner->firstFreedByOther;
    elem->nextFree = oldVal;
    if (__sync_bool_compare_and_swap(&(owner->firstFreedByOther), oldVal, elem))
      break;
  }
}

#endif

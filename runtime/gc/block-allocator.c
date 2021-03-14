/* Copyright (C) 2021 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline size_t SUPERBLOCK_SIZE(GC_state s) {
  return (1 << s->controls->numBlockSizeClasses);
}

static enum FullnessGroup fullness(GC_state s, SuperBlock sb) {
  size_t free = sb->numBlocksFree;

  if (free < ((size_t)1 << sb->sizeClass))
    return COMPLETELY_FULL;

  if (free == SUPERBLOCK_SIZE(s))
    return COMPLETELY_EMPTY;

  float currentEmptiness = (float)free / (float)SUPERBLOCK_SIZE(s);

  if (currentEmptiness >= 1.0 - s->controls->emptinessFraction)
    return NEARLY_FULL;
  if (currentEmptiness < s->controls->emptinessFraction)
    return NEARLY_EMPTY;

  return SOMEWHAT_FULL;
}


static inline SuperBlockList getFullnessGroup(
  ARG_USED_FOR_ASSERT GC_state s,
  BlockAllocator ball,
  int class,
  enum FullnessGroup fg)
{
  assert(0 <= fg && fg <= COMPLETELY_EMPTY);
  assert(0 <= class && (size_t)class < s->controls->numBlockSizeClasses);

  if (fg == COMPLETELY_EMPTY)
    return &(ball->completelyEmptyGroup);

  return &(ball->sizeClassFullnessGroup[class * NUM_FULLNESS_GROUPS + fg]);
}


static inline bool isValidBlockStart(GC_state s, SuperBlock sb, pointer p) {
  size_t blockId = ((size_t)(p - (size_t)sb) / s->controls->blockSize) - 1;
  return (p > (pointer)sb)
         && blockId % (1 << sb->sizeClass) == 0;
}


#if ASSERT

static void assertSuperBlockOkay(
  GC_state s,
  SuperBlock sb,
  BlockAllocator ball,
  int class,
  enum FullnessGroup fg)
{
  assert(sb->magic == 0xabaddeed);
  assert(sb->owner == ball);
  assert(fullness(s, sb) == fg);

  if (fg != COMPLETELY_EMPTY) {
    assert(sb->sizeClass == class);
    assert(isValidBlockStart(s, sb, sb->frontier));
    for (FreeBlock b = sb->firstFree; b != NULL; b = b->nextFree) {
      assert(b->container == sb);
      assert(isValidBlockStart(s, sb, (pointer)b));
    }
  }
}


static void assertSuperBlockListOkay(
  GC_state s,
  BlockAllocator ball,
  int class,
  enum FullnessGroup fg)
{
  SuperBlockList list = getFullnessGroup(s, ball, class, fg);
  SuperBlock prev = NULL;
  SuperBlock curr = list->firstSuperBlock;
  while (curr != NULL) {
    assert(curr->magic == 0xabaddeed);
    assert(curr->prevSuperBlock == prev);
    assertSuperBlockOkay(s, curr, ball, class, fg);
    prev = curr;
    curr = curr->nextSuperBlock;
  }
}


static void assertBlockAllocatorOkay(GC_state s, BlockAllocator ball) {
  for (size_t class = 0; class < s->controls->numBlockSizeClasses; class++) {
    for (int fg = 0; fg < NUM_FULLNESS_GROUPS; fg++) {
      assertSuperBlockListOkay(s, ball, class, fg);
    }
  }
  assertSuperBlockListOkay(s, ball, 0, COMPLETELY_EMPTY);
}

#else

static void assertBlockAllocatorOkay(GC_state s, BlockAllocator ball) {
  (void)s;
  (void)ball;
}

#endif


static void initBlockAllocator(GC_state s, BlockAllocator ball) {
  ball->sizeClassFullnessGroup =
    malloc(s->controls->numBlockSizeClasses * NUM_FULLNESS_GROUPS * sizeof(struct SuperBlockList));
  for (size_t i = 0; i < s->controls->numBlockSizeClasses; i++) {
    for (int j = 0; j < NUM_FULLNESS_GROUPS; j++) {
      getFullnessGroup(s, ball, i, j)->firstSuperBlock = NULL;
    }
  }

  ball->completelyEmptyGroup.firstSuperBlock = NULL;

  ball->numBlocks = 0;
  ball->numBlocksInUse = 0;
  ball->firstFreedByOther = NULL;
}


BlockAllocator initGlobalBlockAllocator(GC_state s) {
  s->blockAllocatorGlobal = malloc(sizeof(struct BlockAllocator));
  initBlockAllocator(s, s->blockAllocatorGlobal);
  return s->blockAllocatorGlobal;
}


void initLocalBlockAllocator(GC_state s, BlockAllocator globalBall) {
  // s->controls->blockSize;
  s->blockAllocatorGlobal = globalBall;
  s->blockAllocatorLocal = malloc(sizeof(struct BlockAllocator));
  initBlockAllocator(s, s->blockAllocatorLocal);
}


static int sizeClass(ARG_USED_FOR_ASSERT GC_state s, size_t numBlocks) {
  assert(numBlocks <= SUPERBLOCK_SIZE(s) / 2);

  int class = 0;
  while (((size_t)1 << class) < numBlocks) {
    class++;
  }

  assert((size_t)class < s->controls->numBlockSizeClasses);
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


static void setSuperBlockSizeClass(GC_state s, SuperBlock sb, int sizeClass) {
  assert((size_t)sb->numBlocksFree == SUPERBLOCK_SIZE(s));

  sb->sizeClass = sizeClass;
  sb->firstFree = NULL;
  sb->frontier = (pointer)sb + s->controls->blockSize;
}


static SuperBlock mmapNewSuperBlock(
  GC_state s,
  BlockAllocator ball,
  int sizeClass)
{
  size_t width = s->controls->blockSize * (1 + SUPERBLOCK_SIZE(s));
  pointer start = GC_mmapAnon(NULL, width);
  if (MAP_FAILED == start) {
    DIE("I ran out of space!");
  }
  assert(isAligned((size_t)start, s->controls->blockSize));

  SuperBlock sb = (SuperBlock)start;
  sb->owner = ball;
  sb->nextSuperBlock = NULL;
  sb->prevSuperBlock = NULL;
  sb->numBlocksFree = SUPERBLOCK_SIZE(s);
  sb->magic = 0xabaddeed;
  setSuperBlockSizeClass(s, sb, sizeClass);
  return sb;
}


static Blocks allocateInSuperBlock(
  GC_state s,
  SuperBlock sb,
  int sizeClass)
{
  if ((size_t)sb->numBlocksFree == SUPERBLOCK_SIZE(s)) {
    // It's completely empty! We can reuse.
    setSuperBlockSizeClass(s, sb, sizeClass);
  }

  assert(sb->sizeClass == sizeClass);
  assert(sb->numBlocksFree >= (1 << sizeClass));

  if (sb->firstFree == NULL) {
    Blocks result = (Blocks)sb->frontier;
    sb->frontier += s->controls->blockSize * (1 << sb->sizeClass);

    sb->numBlocksFree -= (1 << sb->sizeClass);
    assert(sb->owner != NULL);
    sb->owner->numBlocksInUse += (1 << sb->sizeClass);

    result->container = sb;
    result->numBlocks = 1 << sb->sizeClass;
    return result;
  }

  FreeBlock result = sb->firstFree;
  assert(isValidBlockStart(s, sb, (pointer)result));

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
  assert((size_t)sb->numBlocksFree <= SUPERBLOCK_SIZE(s) - (1 << sb->sizeClass));
  assert(isValidBlockStart(s, sb, (pointer)block));

  block->nextFree = sb->firstFree;
  sb->firstFree = block;
  sb->numBlocksFree += (1 << sb->sizeClass);

  assert(sb->owner != NULL);
  assert(sb->owner->numBlocksInUse >= ((size_t)1 << sb->sizeClass));
  sb->owner->numBlocksInUse -= (1 << sb->sizeClass);
}


static Blocks tryAllocateAndAdjustSuperBlocks(
  GC_state s,
  BlockAllocator ball,
  int class)
{
  SuperBlockList targetList = NULL;

  /** First, try to find blocks in partially used superblocks. */
  for (enum FullnessGroup fg = NEARLY_FULL; fg < NUM_FULLNESS_GROUPS; fg++) {
    SuperBlockList list = getFullnessGroup(s, ball, class, fg);
    if (list->firstSuperBlock != NULL) {
      targetList = list;
      break;
    }
  }

  /** Next, try a completely empty */
  if (targetList == NULL &&
      getFullnessGroup(s, ball, 0, COMPLETELY_EMPTY)->firstSuperBlock != NULL)
  {
    targetList = getFullnessGroup(s, ball, 0, COMPLETELY_EMPTY);
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
    SuperBlockList new = getFullnessGroup(s, ball, class, newfg);
    prependSuperBlock(new, sb);
  }

  return result;
}


static void localFreeBlocks(GC_state s, SuperBlock sb, FreeBlock b) {
  BlockAllocator ball = sb->owner;
  assert(ball != NULL);
  assert(ball == s->blockAllocatorLocal);

  enum FullnessGroup fg = fullness(s, sb);
  SuperBlockList oldList = getFullnessGroup(s, ball, sb->sizeClass, fg);
  unlinkSuperBlock(oldList, sb);
  deallocateInSuperBlock(s, sb, b, sb->sizeClass);
  SuperBlockList newList = getFullnessGroup(s, ball, sb->sizeClass, fullness(s, sb));
  prependSuperBlock(newList, sb);
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
  assertBlockAllocatorOkay(s, local);

  if (numBlocks > SUPERBLOCK_SIZE(s) / 2) {
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
  assertBlockAllocatorOkay(s, local);

  int class = sizeClass(s, numBlocks);

  /** Look in local first. */
  Blocks result = tryAllocateAndAdjustSuperBlocks(s, local, class);
  if (result != NULL) {
    assertBlockAllocatorOkay(s, local);
    return result;
  }

  /** If both local fails, we need to mmap a new superchunk. */
  SuperBlock sb = mmapNewSuperBlock(s, local, class);
  local->numBlocks += SUPERBLOCK_SIZE(s);
  result = allocateInSuperBlock(s, sb, class);
  SuperBlockList list = getFullnessGroup(s, local, class, fullness(s, sb));
  prependSuperBlock(list, sb);
  assertBlockAllocatorOkay(s, local);
  return result;
}


void freeBlocks(GC_state s, Blocks bs) {
  // BlockAllocator global = s->blockAllocatorGlobal;
  BlockAllocator local = s->blockAllocatorLocal;

  assertBlockAllocatorOkay(s, local);

  size_t numBlocks = bs->numBlocks;
  SuperBlock sb = bs->container;
  pointer blockStart = (pointer)bs;

  if (numBlocks > SUPERBLOCK_SIZE(s) / 2) {
    assert(sb == NULL);
    GC_release(blockStart, s->controls->blockSize * numBlocks);
    return;
  }

  assert(sb->magic == 0xabaddeed);

  FreeBlock elem = (FreeBlock)blockStart;
  elem->container = sb;
  BlockAllocator owner = sb->owner;
  assert( owner != NULL );
  assert( sb->sizeClass == sizeClass(s, numBlocks) );

  if (owner == local) {
    localFreeBlocks(s, sb, elem);
    assertBlockAllocatorOkay(s, local);
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

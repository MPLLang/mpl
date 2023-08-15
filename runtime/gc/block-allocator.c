/* Copyright (C) 2021 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

#define INFO_BUFFER_LEN 256

static inline size_t SUPERBLOCK_SIZE(GC_state s) {
  return (1 << s->controls->superblockThreshold);
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
  assert(0 <= class && (size_t)class < s->controls->superblockThreshold);

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
  for (size_t class = 0; class < s->controls->superblockThreshold; class++) {
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
  size_t numMegaBlockSizeClasses =
    s->controls->megablockThreshold - s->controls->superblockThreshold;

  ball->sizeClassFullnessGroup =
    malloc(s->controls->superblockThreshold * NUM_FULLNESS_GROUPS * sizeof(struct SuperBlockList));
  ball->megaBlockSizeClass =
    malloc(numMegaBlockSizeClasses * sizeof(struct MegaBlockList));

  for (size_t i = 0; i < s->controls->superblockThreshold; i++) {
    for (int j = 0; j < NUM_FULLNESS_GROUPS; j++) {
      getFullnessGroup(s, ball, i, j)->firstSuperBlock = NULL;
    }
  }

  ball->completelyEmptyGroup.firstSuperBlock = NULL;

  ball->firstFreedByOther = NULL;
  ball->numBlocksMapped = 0;
  ball->numBlocksReleased = 0;
  for (enum BlockPurpose p = 0; p < NUM_BLOCK_PURPOSES; p++) {
    ball->numBlocksAllocated[p] = 0;
    ball->numBlocksFreed[p] = 0;
  }

  for (size_t i = 0; i < numMegaBlockSizeClasses; i++) {
    ball->megaBlockSizeClass[i].firstMegaBlock = NULL;
  }
  pthread_mutex_init(&(ball->megaBlockLock), NULL);
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


static int computeSizeClass(size_t numBlocks) {
  int class = 0;
  while (((size_t)1 << class) < numBlocks) {
    class++;
  }
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


static void mmapNewSuperBlocks(
  GC_state s,
  BlockAllocator ball)
{
  size_t oneWidth = s->controls->blockSize * (1 + SUPERBLOCK_SIZE(s));
  size_t count = 1 + (s->controls->allocBlocksMinSize-1) / oneWidth;
  assert(count * oneWidth >= s->controls->allocBlocksMinSize);
  pointer start = GC_mmapAnon(NULL, count * oneWidth);
  if (MAP_FAILED == start) {
    /** Try again, but the minimum amount of space we actually need. */
    count = 1;
    start = GC_mmapAnon(NULL, oneWidth);
    if (MAP_FAILED == start)
      DIE("ran out of space!");
  }
  assert(isAligned((size_t)start, s->controls->blockSize));

  for (size_t i = 0; i < count; i++) {
    SuperBlock sb = (SuperBlock)(start + oneWidth * i);
    sb->owner = ball;
    sb->nextSuperBlock = NULL;
    sb->prevSuperBlock = NULL;
    sb->numBlocksFree = SUPERBLOCK_SIZE(s);
    sb->magic = 0xabaddeed;
    setSuperBlockSizeClass(s, sb, 0);
    prependSuperBlock(getFullnessGroup(s, ball, 0, COMPLETELY_EMPTY), sb);
  }

  ball->numBlocksMapped += count*(SUPERBLOCK_SIZE(s));
}


static Blocks allocateInSuperBlock(
  GC_state s,
  SuperBlock sb,
  int sizeClass,
  enum BlockPurpose purpose)
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
    sb->owner->numBlocksAllocated[purpose] += (1 << sb->sizeClass);

    result->container = sb;
    result->numBlocks = 1 << sb->sizeClass;
    result->purpose = purpose;
    return result;
  }

  FreeBlock result = sb->firstFree;
  assert(isValidBlockStart(s, sb, (pointer)result));

  sb->firstFree = result->nextFree;
  sb->numBlocksFree -= (1 << sb->sizeClass);

  assert(sb->owner != NULL);
  sb->owner->numBlocksAllocated[purpose] += (1 << sb->sizeClass);

  Blocks bs = (Blocks)result;
  bs->container = sb;
  bs->numBlocks = (1 << sb->sizeClass);
  bs->purpose = purpose;

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

  enum BlockPurpose purpose = block->purpose;
  assert( purpose < NUM_BLOCK_PURPOSES );

  assert(sb->owner != NULL);
  sb->owner->numBlocksFreed[purpose] += (1 << sb->sizeClass);
}


static Blocks tryAllocateAndAdjustSuperBlocks(
  GC_state s,
  BlockAllocator ball,
  int class,
  enum BlockPurpose purpose)
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
  Blocks result = allocateInSuperBlock(s, sb, class, purpose);
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

  size_t numFreed = 0;

  while (topElem != NULL) {
    FreeBlock next = topElem->nextFree;
    SuperBlock sb = topElem->container;
    assert( sb->owner == local );
    localFreeBlocks(s, sb, topElem);

    topElem = next;
    numFreed++;
  }

  if (numFreed > 400) {
    LOG(LM_CHUNK_POOL, LL_DEBUG,
      "number of freed blocks: %zu",
      numFreed);
  }
}


static void freeMegaBlock(GC_state s, MegaBlock mb, size_t sizeClass) {
  BlockAllocator global = s->blockAllocatorGlobal;
  size_t nb = mb->numBlocks;
  enum BlockPurpose purpose = mb->purpose;

  LOG(LM_CHUNK_POOL, LL_INFO,
    "Freeing megablock of %zu blocks",
    nb);

  if (sizeClass >= s->controls->megablockThreshold) {
    GC_release((pointer)mb, s->controls->blockSize * mb->numBlocks);
    LOG(LM_CHUNK_POOL, LL_INFO,
      "Released large allocation of %zu blocks (unmap threshold: %zu)",
      nb,
      (size_t)1 << (s->controls->megablockThreshold - 1));
      
    __sync_fetch_and_add(&(global->numBlocksFreed[purpose]), nb);
    __sync_fetch_and_add(&(global->numBlocksReleased), nb);
    return;
  }

  size_t mbClass = sizeClass - s->controls->superblockThreshold;

  pthread_mutex_lock(&(global->megaBlockLock));
  mb->nextMegaBlock = global->megaBlockSizeClass[mbClass].firstMegaBlock;
  global->megaBlockSizeClass[mbClass].firstMegaBlock = mb;
  pthread_mutex_unlock(&(global->megaBlockLock));

  __sync_fetch_and_add(&(global->numBlocksFreed[purpose]), nb);
  return;
}


static MegaBlock tryFindMegaBlock(
  GC_state s,
  size_t numBlocksNeeded,
  size_t sizeClass,
  enum BlockPurpose purpose)
{
  BlockAllocator global = s->blockAllocatorGlobal;
  assert(sizeClass >= s->controls->superblockThreshold);

  if (sizeClass >= s->controls->megablockThreshold)
    return NULL;

  size_t numMbSizeClasses =
    s->controls->megablockThreshold - s->controls->superblockThreshold;

  pthread_mutex_lock(&(global->megaBlockLock));

  size_t lower = sizeClass - s->controls->superblockThreshold;
  size_t upper = min(lower+2, numMbSizeClasses);
  size_t count = 0;

  for (size_t i = lower; i < upper; i++) {
    for (MegaBlock *mbp = &(global->megaBlockSizeClass[i].firstMegaBlock);
         *mbp != NULL;
         mbp = &((*mbp)->nextMegaBlock))
    {
      MegaBlock mb = *mbp;
      count++;

      if (mb->numBlocks >= numBlocksNeeded) {
        *mbp = mb->nextMegaBlock;
        mb->nextMegaBlock = NULL;
        pthread_mutex_unlock(&(global->megaBlockLock));

        __sync_fetch_and_add(&(global->numBlocksAllocated[purpose]), mb->numBlocks);

        LOG(LM_CHUNK_POOL, LL_INFO,
          "inspected %zu, satisfied large alloc of %zu blocks using megablock of %zu",
          count,
          numBlocksNeeded,
          mb->numBlocks);

        return mb;
      }
    }
  }

  pthread_mutex_unlock(&(global->megaBlockLock));
  return NULL;
}


static MegaBlock mmapNewMegaBlock(GC_state s, size_t numBlocks, enum BlockPurpose purpose)
{
  pointer start = GC_mmapAnon(NULL, s->controls->blockSize * numBlocks);
  if (MAP_FAILED == start) {
    return NULL;
  }
  if (!isAligned((size_t)start, s->controls->blockSize)) {
    DIE("whoops, mmap didn't align by the block-size.");
  }

  BlockAllocator global = s->blockAllocatorGlobal;
  __sync_fetch_and_add(&(global->numBlocksMapped), numBlocks);
  __sync_fetch_and_add(&(global->numBlocksAllocated[purpose]), numBlocks);

  MegaBlock mb = (MegaBlock)start;
  mb->numBlocks = numBlocks;
  mb->nextMegaBlock = NULL;
  mb->purpose = purpose;

  LOG(LM_CHUNK_POOL, LL_INFO,
    "mmap'ed new megablock of size %zu",
    numBlocks);

  return mb;
}


Blocks allocateBlocksWithPurpose(GC_state s, size_t numBlocks, enum BlockPurpose purpose) {
  BlockAllocator local = s->blockAllocatorLocal;
  assertBlockAllocatorOkay(s, local);

  int class = computeSizeClass(numBlocks);

  if ((size_t)class >= s->controls->superblockThreshold) {

    /** First see if we can reuse. If not, try mmap a new one. If that all
      * fails, we're a bit screwed.
      */

    MegaBlock mb = tryFindMegaBlock(s, numBlocks, class, purpose);

    if (NULL == mb)
      mb = mmapNewMegaBlock(s, numBlocks, purpose);

    if (NULL == mb)
      DIE("ran out of space!");

    size_t actualNumBlocks = mb->numBlocks;
    assert(actualNumBlocks >= numBlocks);
    Blocks bs = (Blocks)mb;
    bs->container = NULL;
    bs->numBlocks = actualNumBlocks;
    bs->purpose = purpose;
    return bs;
  }

  clearOutOtherFrees(s);
  assertBlockAllocatorOkay(s, local);

  /** Look in local first. */
  Blocks result = tryAllocateAndAdjustSuperBlocks(s, local, class, purpose);
  if (result != NULL) {
    assertBlockAllocatorOkay(s, local);
    assert( result->purpose == purpose );
    return result;
  }

  /** If both local fails, we need to mmap new superchunks. */
  mmapNewSuperBlocks(s, local);

  result = tryAllocateAndAdjustSuperBlocks(s, local, class, purpose);
  if (result == NULL) {
    DIE("Ran out of space for new superblocks!");
  }

  assertBlockAllocatorOkay(s, local);
  assert( result->purpose == purpose );
  return result;
}


Blocks allocateBlocks(GC_state s, size_t numBlocks) {
  return allocateBlocksWithPurpose(s, numBlocks, BLOCK_FOR_UNKNOWN_PURPOSE);
}


void freeBlocks(GC_state s, Blocks bs, writeFreedBlockInfoFnClosure f) {
  BlockAllocator local = s->blockAllocatorLocal;
  assertBlockAllocatorOkay(s, local);

  size_t numBlocks = bs->numBlocks;
  SuperBlock sb = bs->container;
  enum BlockPurpose purpose = bs->purpose;
  pointer blockStart = (pointer)bs;

#if ASSERT
  if (!s->controls->debugKeepFreeBlocks) {
    /** Clear out memory to try and catch errors quickly... */
    memset((void*)bs, 0xBF, numBlocks * s->controls->blockSize);
  }
#endif

  if (s->controls->debugKeepFreeBlocks) {
    /** use the end of the first chunk as an info buffer, and populate it
      * with info from the call-site.
      */
    char* infoBuffer =
      ((char*)blockStart) + s->controls->blockSize - (INFO_BUFFER_LEN+1);
    infoBuffer[INFO_BUFFER_LEN] = '\0';

    if (NULL != f)
      f->fun(s, infoBuffer, INFO_BUFFER_LEN, f->env);
    else
      snprintf(infoBuffer, INFO_BUFFER_LEN, "No info provided.");

    for (size_t idx = 0; idx < numBlocks; idx++) {
      DebugKeptFreeBlock bb =
        (DebugKeptFreeBlock)
        (blockStart + idx * s->controls->blockSize);
      bb->magic = 0xfeedbadbed;
      bb->blockIdxInGroup = idx;
      bb->numBlocksInGroup = numBlocks;
      bb->infoBuffer = infoBuffer;
      bb->infoBufferLen = INFO_BUFFER_LEN;
    }

    return;
  }

  size_t sizeClass =
    NULL != sb ? sb->sizeClass : computeSizeClass(numBlocks);

  if (sizeClass >= s->controls->superblockThreshold) {
    assert(sb == NULL);
    MegaBlock mb = (MegaBlock)blockStart;
    mb->numBlocks = numBlocks;
    mb->nextMegaBlock = NULL;
    mb->purpose = purpose;
    freeMegaBlock(s, mb, sizeClass);
    return;
  }

  assert(sb->magic == 0xabaddeed);

  FreeBlock elem = (FreeBlock)blockStart;
  elem->container = sb;
  elem->purpose = purpose;
  BlockAllocator owner = sb->owner;
  assert(owner != NULL);
  assert(sb->sizeClass == computeSizeClass(numBlocks));

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


void queryCurrentBlockUsage(
  GC_state s,
  size_t *numBlocksMapped,
  size_t *numGlobalBlocksMapped,
  size_t *numBlocksReleased,
  size_t *numGlobalBlocksReleased,
  size_t *numBlocksAllocated,
  size_t *numBlocksFreed)
{
  *numBlocksMapped = 0;
  *numGlobalBlocksMapped = 0;
  *numBlocksReleased = 0;
  *numGlobalBlocksReleased = 0;
  for (enum BlockPurpose p = 0; p < NUM_BLOCK_PURPOSES; p++) {
    numBlocksAllocated[p] = 0;
    numBlocksFreed[p] = 0;
  }

  // query local allocators
  for (uint32_t i = 0; i < s->numberOfProcs; i++) {
    BlockAllocator ball = s->procStates[i].blockAllocatorLocal;
    *numBlocksMapped += ball->numBlocksMapped;
    *numBlocksReleased += ball->numBlocksReleased;
    for (enum BlockPurpose p = 0; p < NUM_BLOCK_PURPOSES; p++) {
      numBlocksAllocated[p] += ball->numBlocksAllocated[p];
      numBlocksFreed[p] += ball->numBlocksFreed[p];
    }
  }

  // query global allocator
  BlockAllocator global = s->blockAllocatorGlobal;
  *numBlocksMapped += global->numBlocksMapped;
  *numBlocksReleased += global->numBlocksReleased;
  for (enum BlockPurpose p = 0; p < NUM_BLOCK_PURPOSES; p++) {
    numBlocksAllocated[p] += global->numBlocksAllocated[p];
    numBlocksFreed[p] += global->numBlocksFreed[p];
  }

  *numGlobalBlocksMapped += global->numBlocksMapped;
  *numGlobalBlocksReleased += global->numBlocksReleased;
}


void logCurrentBlockUsage(
  GC_state s, 
  struct timespec *now, 
  __attribute__((unused)) void *env)
{
  size_t mapped;
  size_t globalMapped;
  size_t released;
  size_t globalReleased;
  size_t allocated[NUM_BLOCK_PURPOSES];
  size_t freed[NUM_BLOCK_PURPOSES];
  queryCurrentBlockUsage(
    s,
    &mapped,
    &globalMapped,
    &released,
    &globalReleased,
    (size_t*)allocated,
    (size_t*)freed
  );

  size_t inUse[NUM_BLOCK_PURPOSES];
  for (enum BlockPurpose p = 0; p < NUM_BLOCK_PURPOSES; p++) {
    if (freed[p] > allocated[p]) {
      inUse[p] = 0;
    }
    else {
      inUse[p] = allocated[p] - freed[p];
    }
  }

  size_t count = mapped-released;
  if (released > mapped) count = 0;

  size_t globalCount = globalMapped - globalReleased;
  if (globalReleased > globalMapped) globalCount = 0;

  LOG(LM_BLOCK_ALLOCATOR, LL_INFO,
    "block-allocator(%zu.%.9zu)\n"
    "  currently mapped           %zu (= %zu - %zu)\n"
    "  currently mapped (global)  %zu (= %zu - %zu)\n"
    "  BLOCK_FOR_HEAP_CHUNK       %zu (%zu%%) (= %zu - %zu)\n"
    "  BLOCK_FOR_REMEMBERED_SET   %zu (%zu%%) (= %zu - %zu)\n"
    "  BLOCK_FOR_FORGOTTEN_SET    %zu (%zu%%) (= %zu - %zu)\n"
    "  BLOCK_FOR_HH_ALLOCATOR     %zu (%zu%%) (= %zu - %zu)\n"
    "  BLOCK_FOR_UF_ALLOCATOR     %zu (%zu%%) (= %zu - %zu)\n"
    "  BLOCK_FOR_GC_WORKLIST      %zu (%zu%%) (= %zu - %zu)\n"
    "  BLOCK_FOR_SUSPECTS         %zu (%zu%%) (= %zu - %zu)\n"
    "  BLOCK_FOR_EBR              %zu (%zu%%) (= %zu - %zu)\n"
    "  BLOCK_FOR_UNKNOWN_PURPOSE  %zu (%zu%%) (= %zu - %zu)\n",
    now->tv_sec,
    now->tv_nsec,
    count,
    mapped,
    released,
    
    globalCount,
    globalMapped,
    globalReleased,

    inUse[BLOCK_FOR_HEAP_CHUNK],
    (size_t)(100.0 * (double)inUse[BLOCK_FOR_HEAP_CHUNK] / (double)count),
    allocated[BLOCK_FOR_HEAP_CHUNK],
    freed[BLOCK_FOR_HEAP_CHUNK],

    inUse[BLOCK_FOR_REMEMBERED_SET],
    (size_t)(100.0 * (double)inUse[BLOCK_FOR_REMEMBERED_SET] / (double)count),
    allocated[BLOCK_FOR_REMEMBERED_SET],
    freed[BLOCK_FOR_REMEMBERED_SET],

    inUse[BLOCK_FOR_FORGOTTEN_SET],
    (size_t)(100.0 * (double)inUse[BLOCK_FOR_FORGOTTEN_SET] / (double)count),
    allocated[BLOCK_FOR_FORGOTTEN_SET],
    freed[BLOCK_FOR_FORGOTTEN_SET],

    inUse[BLOCK_FOR_HH_ALLOCATOR],
    (size_t)(100.0 * (double)inUse[BLOCK_FOR_HH_ALLOCATOR] / (double)count),
    allocated[BLOCK_FOR_HH_ALLOCATOR],
    freed[BLOCK_FOR_HH_ALLOCATOR],

    inUse[BLOCK_FOR_UF_ALLOCATOR],
    (size_t)(100.0 * (double)inUse[BLOCK_FOR_UF_ALLOCATOR] / (double)count),
    allocated[BLOCK_FOR_UF_ALLOCATOR],
    freed[BLOCK_FOR_UF_ALLOCATOR],

    inUse[BLOCK_FOR_GC_WORKLIST],
    (size_t)(100.0 * (double)inUse[BLOCK_FOR_GC_WORKLIST] / (double)count),
    allocated[BLOCK_FOR_GC_WORKLIST],
    freed[BLOCK_FOR_GC_WORKLIST],

    inUse[BLOCK_FOR_SUSPECTS],
    (size_t)(100.0 * (double)inUse[BLOCK_FOR_SUSPECTS] / (double)count),
    allocated[BLOCK_FOR_SUSPECTS],
    freed[BLOCK_FOR_SUSPECTS],

    inUse[BLOCK_FOR_EBR],
    (size_t)(100.0 * (double)inUse[BLOCK_FOR_EBR] / (double)count),
    allocated[BLOCK_FOR_EBR],
    freed[BLOCK_FOR_EBR],

    inUse[BLOCK_FOR_UNKNOWN_PURPOSE],
    (size_t)(100.0 * (double)inUse[BLOCK_FOR_UNKNOWN_PURPOSE] / (double)count),
    allocated[BLOCK_FOR_UNKNOWN_PURPOSE],
    freed[BLOCK_FOR_UNKNOWN_PURPOSE]);
}

Sampler newBlockUsageSampler(GC_state s) {
  struct SamplerClosure func;
  func.fun = logCurrentBlockUsage;
  func.env = NULL;

  struct timespec desiredInterval = s->controls->blockUsageSampleInterval;
  Sampler result = malloc(sizeof(struct Sampler));
  initSampler(s, result, &func, &desiredInterval);

  return result;
}

#endif

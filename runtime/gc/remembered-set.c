/* Copyright (C) 2018-2021 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

void HM_initRemSet(HM_remSet remSet) {
  HM_initChunkList(&(remSet->private));
  CC_initConcList(&(remSet->public));
}

void HM_remember(GC_state s, HM_remSet remSet, HM_remembered remElem, bool conc) {
  if (!conc) {
    HM_storeInChunkListWithPurpose(s, &(remSet->private), (void*)remElem, sizeof(struct HM_remembered), BLOCK_FOR_REMEMBERED_SET);
  }
  else {
    CC_storeInConcListWithPurpose(s, &(remSet->public), (void *)remElem, sizeof(struct HM_remembered), BLOCK_FOR_REMEMBERED_SET);
  }
}

/* An entry of a public list is complete once its object is non-zero: the
 * writer publishes the object last, with a release store
 * (CC_storeInConcListWithPurpose), so it is read with an acquire load. */
static inline bool isPublishedEntry(pointer p) {
  return __atomic_load_n(&(((HM_remembered)p)->object), __ATOMIC_ACQUIRE) != 0;
}
/* CC_storeInConcListWithPurpose publishes an entry's last 8-byte word: that
 * must be the object. */
COMPILE_TIME_ASSERT(HM_remembered__object_is_the_last_word,
                    offsetof(struct HM_remembered, object) + sizeof(objptr)
                      == sizeof(struct HM_remembered)
                    && sizeof(objptr) == sizeof(uint64_t));

/* Like HM_foreachPrivate, for chunks just taken from a public list, which
 * a writer may still be filling. */
static void foreachInPoppedChunks(
  GC_state s,
  HM_chunkList chunkList,
  HM_foreachDownptrClosure f)
{
  HM_chunk chunk = HM_getChunkListFirstChunk(chunkList);
  while (chunk != NULL)
  {
    pointer p = HM_getChunkStart(chunk);
    pointer frontier = __atomic_load_n(&chunk->frontier, __ATOMIC_RELAXED);
    while (p < frontier && isPublishedEntry(p))
    {
      f->fun(s, (HM_remembered)p, f->env);
      p += sizeof(struct HM_remembered);
    }
    chunk = chunk->nextChunk;
  }
}

void HM_foreachPrivate(
  GC_state s,
  HM_chunkList chunkList,
  HM_foreachDownptrClosure f)
{
  HM_chunk chunk = HM_getChunkListFirstChunk(chunkList);
  while (chunk != NULL)
  {
    pointer p = HM_getChunkStart(chunk);
    pointer frontier = HM_getChunkFrontier(chunk);
    while (p < frontier && ((HM_remembered)p)->object != 0)
    {
      f->fun(s, (HM_remembered)p, f->env);
      p += sizeof(struct HM_remembered);
    }
    chunk = chunk->nextChunk;
  }
}

typedef struct FishyChunk
{
  HM_chunk chunk;
  pointer scanned;
} FishyChunk;

void makeChunkFishy(FishyChunk * fc, HM_chunk chunk, pointer frontier, int* numFishyChunks) {
  (fc[*numFishyChunks]).chunk = chunk;
  (fc[*numFishyChunks]).scanned = frontier;
  *numFishyChunks = *numFishyChunks + 1;
  return;
}

FishyChunk * resizeFishyArray (FishyChunk * fishyChunks, int * currentSize) {
  int cs =  *currentSize;
  int new_size = 2 * cs;
  FishyChunk * fc = malloc(sizeof(struct FishyChunk) * new_size);
  memcpy(fc, fishyChunks, sizeof(struct FishyChunk) * cs);
  *currentSize = new_size;
  free(fishyChunks);
  return fc;
}

void checkFishyChunks(GC_state s,
  FishyChunk * fishyChunks,
  int numFishyChunks,
  HM_foreachDownptrClosure f)
{
  if (fishyChunks == NULL) {
    return;
  }
  bool changed = true;
  while (changed) {
    int i = numFishyChunks - 1;
    changed = false;
    while (i >= 0)
    {
      HM_chunk chunk = fishyChunks[i].chunk;
      pointer p = fishyChunks[i].scanned;
      pointer frontier = __atomic_load_n(&chunk->frontier, __ATOMIC_RELAXED);
      while (TRUE)
      {
        while (p < frontier && isPublishedEntry(p))
        {
          f->fun(s, (HM_remembered)p, f->env);
          p += sizeof(struct HM_remembered);
        }
        frontier = __atomic_load_n(&chunk->frontier, __ATOMIC_RELAXED);
        if (p >= frontier) {
          break;
        }
      }
      if (p != fishyChunks[i].scanned) {
        fishyChunks[i].scanned = p;
        changed = true;
      }
      i --;
    }
  }
}

void HM_foreachPublic (
  GC_state s,
  HM_remSet remSet,
  HM_foreachDownptrClosure f,
  bool trackFishyChunks)
{

  if (__atomic_load_n(&(remSet->public).firstChunk, __ATOMIC_ACQUIRE) == NULL) {
    return;
  }

  if (!trackFishyChunks) {
    struct HM_chunkList _chunkList;
    HM_chunkList chunkList = &(_chunkList);
    CC_popAsChunkList(&(remSet->public), chunkList);
    foreachInPoppedChunks(s, chunkList, f);
    HM_appendChunkList(&(remSet->private), chunkList);
    return;
  }

  HM_chunk chunk = __atomic_load_n(&(remSet->public).firstChunk, __ATOMIC_ACQUIRE);
  HM_chunk lastChunk = CC_getLastChunk (&(remSet->public));
  int array_size = 2 * s->numberOfProcs;
  FishyChunk* fishyChunks = malloc(sizeof(struct FishyChunk) * array_size);
  int numFishyChunks = 0;
  while (chunk != NULL)
  {
    while (chunk != NULL)
    {
      pointer p = HM_getChunkStart(chunk);
      pointer frontier = __atomic_load_n(&chunk->frontier, __ATOMIC_RELAXED);
      while (p < frontier && isPublishedEntry(p))
      {
        f->fun(s, (HM_remembered)p, f->env);
        p += sizeof(struct HM_remembered);
      }
      /* nextChunk first: CC_appendConcList sets retireChunk before it
       * publishes nextChunk with a release store. A chunk with a successor is
       * checked again too if the scan did not reach its final frontier: the
       * scan stopped at an entry still being written, or writers filled the
       * chunk past the frontier read before the scan. A chunk that
       * allocateChunkInConcList gave a successor is full (every public entry
       * is 16 bytes), so its frontier no longer moves, and the frontier
       * reloaded after the acquire load of nextChunk is the final one;
       * checkFishyChunks scans on to it, waiting for entries being written.
       * (A chunk that CC_appendConcList linked is retired: fishy anyway.) */
      if (__atomic_load_n(&chunk->nextChunk, __ATOMIC_ACQUIRE) == NULL
          || __atomic_load_n(&chunk->retireChunk, __ATOMIC_RELAXED)
          || p < __atomic_load_n(&chunk->frontier, __ATOMIC_RELAXED))
      {
        if (numFishyChunks >= array_size) {
          fishyChunks = resizeFishyArray(fishyChunks, &array_size);
        }
        makeChunkFishy(fishyChunks, chunk, p, &numFishyChunks);
      }
      chunk = __atomic_load_n(&chunk->nextChunk, __ATOMIC_ACQUIRE);
    }
    checkFishyChunks(s, fishyChunks, numFishyChunks, f);
    lastChunk = CC_getLastChunk(&(remSet->public));
    if (lastChunk != fishyChunks[numFishyChunks - 1].chunk) {
      assert (chunk->nextChunk != NULL);
      chunk = chunk->nextChunk;
    }
  }
  free(fishyChunks);
  struct HM_chunkList _chunkList;
  HM_chunkList chunkList = &(_chunkList);
  CC_popAsChunkList(&(remSet->public), chunkList);
  HM_appendChunkList(&(remSet->private), chunkList);
}

void HM_foreachRemembered(
  GC_state s,
  HM_remSet remSet,
  HM_foreachDownptrClosure f,
  bool trackFishyChunks)
{
  assert(remSet != NULL);
  HM_foreachPrivate(s, &(remSet->private), f);
  HM_foreachPublic(s, remSet, f, trackFishyChunks);
}


size_t HM_numRemembered(HM_remSet remSet) {
  assert(remSet != NULL);
  size_t count = 0;
  HM_chunk chunk = HM_getChunkListFirstChunk(&(remSet->private)); // ignore public for now.
  while (chunk != NULL) {
    pointer p = HM_getChunkStart(chunk);
    pointer frontier = HM_getChunkFrontier(chunk);
    count += (size_t)((frontier - p)) / sizeof(struct HM_remembered);
    chunk = chunk->nextChunk;
  }

  return count;
}

void HM_appendRemSet(HM_remSet r1, HM_remSet r2) {
  HM_appendChunkList(&(r1->private), &(r2->private));
  CC_appendConcList(&(r1->public), &(r2->public));
}

void HM_freeRemSetWithInfo(GC_state s, HM_remSet remSet, void* info) {
  HM_freeChunksInListWithInfo(s, &(remSet->private), info, BLOCK_FOR_REMEMBERED_SET);
  CC_freeChunksInConcListWithInfo(s, &(remSet->public), info, BLOCK_FOR_REMEMBERED_SET);
}

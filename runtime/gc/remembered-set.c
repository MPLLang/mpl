/* Copyright (C) 2018-2021 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

void HM_initRemSet(HM_remSet remSet) {
  HM_initChunkList(&(remSet->private));
  CC_initConcList(&(remSet->public));
}

void HM_remember(HM_remSet remSet, HM_remembered remElem, bool conc) {
  if (!conc) {
    HM_storeInChunkListWithPurpose(&(remSet->private), (void*)remElem, sizeof(struct HM_remembered), BLOCK_FOR_REMEMBERED_SET);
  }
  else {
    CC_storeInConcListWithPurpose(&(remSet->public), (void *)remElem, sizeof(struct HM_remembered), BLOCK_FOR_REMEMBERED_SET);
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
      pointer frontier = HM_getChunkFrontier(chunk);
      while (TRUE)
      {
        while (p < frontier && ((HM_remembered)p)->object != 0)
        {
          f->fun(s, (HM_remembered)p, f->env);
          p += sizeof(struct HM_remembered);
        }
        frontier = HM_getChunkFrontier(chunk);
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

  if ((remSet->public).firstChunk == NULL) {
    return;
  }

  if (!trackFishyChunks) {
    struct HM_chunkList _chunkList;
    HM_chunkList chunkList = &(_chunkList);
    CC_popAsChunkList(&(remSet->public), chunkList);
    HM_foreachPrivate(s, chunkList, f);
    HM_appendChunkList(&(remSet->private), chunkList);
    return;
  }

  HM_chunk chunk = (remSet->public).firstChunk;
  HM_chunk lastChunk = CC_getLastChunk (&(remSet->public));
  int array_size = 2 * s->numberOfProcs;
  FishyChunk* fishyChunks = malloc(sizeof(struct FishyChunk) * array_size);
  int numFishyChunks = 0;
  while (chunk != NULL)
  {
    while (chunk != NULL)
    {
      pointer p = HM_getChunkStart(chunk);
      pointer frontier = HM_getChunkFrontier(chunk);
      while (p < frontier && ((HM_remembered)p)->object != 0)
      {
        f->fun(s, (HM_remembered)p, f->env);
        p += sizeof(struct HM_remembered);
      }
      if ((chunk->retireChunk || chunk->nextChunk == NULL))
      {
        if (numFishyChunks >= array_size) {
          fishyChunks = resizeFishyArray(fishyChunks, &array_size);
        }
        makeChunkFishy(fishyChunks, chunk, p, &numFishyChunks);
      }
      chunk = chunk->nextChunk;
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
  HM_freeChunksInListWithInfo(s, &(remSet->private), info, BLOCK_FOR_UNKNOWN_PURPOSE);
  CC_freeChunksInConcListWithInfo(s, &(remSet->public), info);
}

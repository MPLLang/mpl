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
    HM_storeInchunkList(&(remSet->private), (void*)remElem, sizeof(struct HM_remembered));
  }
  else {
    CC_storeInConcList(&(remSet->public), (void *)remElem, sizeof(struct HM_remembered));
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
    while (p < frontier)
    {
      f->fun(s, (HM_remembered)p, f->env);
      p += sizeof(struct HM_remembered);
    }
    chunk = chunk->nextChunk;
  }
}

void HM_foreachRemembered(
  GC_state s,
  HM_remSet remSet,
  HM_foreachDownptrClosure f)
{
  assert(remSet != NULL);
  HM_foreachPrivate(s, &(remSet->private), f);
  while (TRUE) {
    struct HM_chunkList _chunkList;
    HM_chunkList chunkList = &(_chunkList);
    CC_popAsChunkList(&(remSet->public), chunkList);
    if (chunkList->firstChunk == NULL) {
      break;
    }
    else {
      HM_foreachPrivate(s, chunkList, f);
      HM_appendChunkList(&(remSet->private), chunkList);
    }
  }
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
  HM_freeChunksInListWithInfo(s, &(remSet->private), info);
  CC_freeChunksInConcListWithInfo(s, &(remSet->public), info);
}

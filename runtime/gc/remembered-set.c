/* Copyright (C) 2018 Sam Westrick
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void HM_remember(HM_chunkList list, objptr dst, Int64 idx, objptr src) {
  assert(list != NULL);

  HM_chunkList rememberedSet = list->rememberedSet;
  if (NULL == rememberedSet) {
    rememberedSet = HM_newChunkList(NULL, CHUNK_INVALID_LEVEL);
    list->rememberedSet = rememberedSet;
  }

  HM_chunk chunk = HM_getChunkListLastChunk(rememberedSet);
  if (NULL == chunk || (size_t)(chunk->limit - chunk->frontier) < sizeof(struct HM_remembered)) {
    // size is arbitrary; just need a chunk.
    chunk = HM_allocateChunk(rememberedSet, sizeof(struct HM_remembered));
  }

  assert((size_t)(chunk->limit - chunk->frontier) >= sizeof(struct HM_remembered));
  struct HM_remembered* r = (struct HM_remembered*)chunk->frontier;
  r->dst = dst;
  r->idx = idx;
  r->src = src;
  chunk->frontier += sizeof(struct HM_remembered);
}

void HM_foreachRemembered(GC_state s, HM_chunkList rememberedSet, ForeachRememberedFunc f, void* fArgs) {
  HM_chunk chunk = rememberedSet->firstChunk;
  while (chunk != NULL) {
    pointer p = HM_getChunkStart(chunk);
    pointer frontier = HM_getChunkFrontier(chunk);
    while (p < frontier) {
      struct HM_remembered* r = (struct HM_remembered*)p;
      f(s, r->dst, r->idx, r->src, fArgs);
      p += sizeof(struct HM_remembered);
    }
    chunk = chunk->nextChunk;
  }
}

void HM_foreachRememberedInLevelList(GC_state s, HM_chunkList levelList, Word32 minLevel, ForeachRememberedFunc f, void* fArgs) {
  HM_chunkList levelHead = levelList;
  assert(HM_isLevelHead(levelHead));

  while (levelHead != NULL && levelHead->level >= minLevel) {
    if (levelHead->rememberedSet != NULL) {
      HM_foreachRemembered(s, levelHead->rememberedSet, f, fArgs);
    }
    levelHead = levelHead->nextHead;
  }
}

/* Copyright (C) 2018 Sam Westrick
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void HM_remember(HM_chunkList remset, HM_chunkList levelHead, objptr dst, objptr* field, objptr src) {
  HM_chunk chunk = HM_getChunkListLastChunk(remset);
  if (NULL == chunk || (size_t)(chunk->limit - chunk->frontier) < sizeof(struct HM_remembered)) {
    chunk = HM_allocateChunk(remset, sizeof(struct HM_remembered));

    if (levelHead != NULL) {
      assert(levelHead->rememberedSet == remset);
      levelHead->size += HM_getChunkSize(chunk);
      GC_state s = pthread_getspecific(gcstate_key);
      if (levelHead->containingHH != COPY_OBJECT_HH_VALUE &&
          levelHead->containingHH != NULL &&
          levelHead->level >= HM_HH_getLowestPrivateLevel(s, levelHead->containingHH)) {
        levelHead->containingHH->locallyCollectibleSize += HM_getChunkSize(chunk);
      }
    }
  }

  assert(NULL != chunk);
  assert((size_t)(chunk->limit - chunk->frontier) >= sizeof(struct HM_remembered));
  struct HM_remembered* r = (struct HM_remembered*)chunk->frontier;
  r->dst = dst;
  r->field = field;
  r->src = src;
  chunk->frontier += sizeof(struct HM_remembered);
}

void HM_rememberAtLevel(HM_chunkList levelHead, objptr dst, objptr* field, objptr src) {
  assert(levelHead != NULL);

  HM_chunkList rememberedSet = levelHead->rememberedSet;
  if (NULL == rememberedSet) {
    rememberedSet = HM_newChunkList(NULL, CHUNK_INVALID_LEVEL);
    levelHead->rememberedSet = rememberedSet;
  }

  HM_remember(rememberedSet, levelHead, dst, field, src);
}

void HM_foreachRemembered(GC_state s, HM_chunkList rememberedSet, ForeachRememberedFunc f, void* fArgs) {
  if (rememberedSet == NULL) {
    return;
  }

  HM_chunk chunk = rememberedSet->firstChunk;
  while (chunk != NULL) {
    pointer p = HM_getChunkStart(chunk);
    pointer frontier = HM_getChunkFrontier(chunk);
    while (p < frontier) {
      struct HM_remembered* r = (struct HM_remembered*)p;
      f(s, r->dst, r->field, r->src, fArgs);
      p += sizeof(struct HM_remembered);
    }
    chunk = chunk->nextChunk;
  }
}

/* Copyright (C) 2018-2019 Sam Westrick
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

void HM_remember(HM_chunkList remset, objptr dst, objptr* field, objptr src) {
  HM_chunk chunk = HM_getChunkListLastChunk(remset);
  if (NULL == chunk || (size_t)(chunk->limit - chunk->frontier) < sizeof(struct HM_remembered)) {
    chunk = HM_allocateChunk(remset, sizeof(struct HM_remembered));
  }

  assert(NULL != chunk);
  assert((size_t)(chunk->limit - chunk->frontier) >= sizeof(struct HM_remembered));
  struct HM_remembered* r = (struct HM_remembered*)chunk->frontier;
  r->dst = dst;
  r->field = field;
  r->src = src;
  chunk->frontier += sizeof(struct HM_remembered);
}

void HM_rememberAtLevel(HM_HierarchicalHeap hh, objptr dst, objptr* field, objptr src) {
  assert(hh != NULL);

  HM_chunkList rememberedSet = hh->rememberedSet;
  if (NULL == rememberedSet) {
    rememberedSet = HM_newChunkList();
    hh->rememberedSet = rememberedSet;
  }

  HM_remember(rememberedSet, dst, field, src);
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

size_t HM_numRemembered(HM_chunkList rememberedSet) {
  if (rememberedSet == NULL) return 0;

  size_t count = 0;

  HM_chunk chunk = rememberedSet->firstChunk;
  while (chunk != NULL) {
    pointer p = HM_getChunkStart(chunk);
    pointer frontier = HM_getChunkFrontier(chunk);

    count += (size_t)((frontier - p)) / sizeof(struct HM_remembered);

    chunk = chunk->nextChunk;
  }

  return count;
}

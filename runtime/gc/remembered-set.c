/* Copyright (C) 2018-2019 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

void HM_remember(HM_chunkList remSet, objptr dst, objptr* field, objptr src) {
  HM_chunk chunk = HM_getChunkListLastChunk(remSet);
  if (NULL == chunk || (size_t)(chunk->limit - chunk->frontier) < sizeof(struct HM_remembered)) {
    chunk = HM_allocateChunk(remSet, sizeof(struct HM_remembered));
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
  HM_remember(HM_HH_getRemSet(hh), dst, field, src);
}

void HM_foreachRemembered(
  GC_state s,
  HM_chunkList remSet,
  ForeachRememberedFunc f,
  void* fArgs)
{
  assert(remSet != NULL);
  HM_chunk chunk = HM_getChunkListFirstChunk(remSet);
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

size_t HM_numRemembered(HM_chunkList remSet) {
  assert(remSet != NULL);
  size_t count = 0;
  HM_chunk chunk = HM_getChunkListFirstChunk(remSet);
  while (chunk != NULL) {
    pointer p = HM_getChunkStart(chunk);
    pointer frontier = HM_getChunkFrontier(chunk);
    count += (size_t)((frontier - p)) / sizeof(struct HM_remembered);
    chunk = chunk->nextChunk;
  }

  return count;
}

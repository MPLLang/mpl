/* Copyright (C) 2022 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#include "chunk-set.h"

#if (defined (MLTON_GC_INTERNAL_FUNCS))

#define EMPTY_SLOT ((HM_chunk)0)
#define TOMBSTONE  ((HM_chunk)1)

// from numerical recipes
static inline uint64_t hash64(uint64_t u) {
  uint64_t v = u * 3935559000370003845ul + 2691343689449507681ul;
  v ^= v >> 21;
  v ^= v << 37;
  v ^= v >> 4;
  v *= 4768777513237032717ul;
  v ^= v << 20;
  v ^= v >> 41;
  v ^= v << 5;
  return v;
}


// a slightly cheaper, but possibly not as good version
// based on splitmix64
static inline uint64_t hash64_2(uint64_t x) {
  x = (x ^ (x >> 30)) * (uint64_t)0xbf58476d1ce4e5b9;
  x = (x ^ (x >> 27)) * (uint64_t)0x94d049bb133111eb;
  x = x ^ (x >> 31);
  return x;
}


static inline size_t ChunkSet_hashToIndex(HM_chunk chunkp, size_t capacity) {
  uint64_t p = (uint64_t)(uintptr_t)chunkp;
  return ((size_t)hash64_2(p)) % capacity;
}


void ChunkSet_init(GC_state s, ChunkSet set, size_t numBlocks) {
  Blocks bs = allocateBlocks(s, numBlocks);
  assert(bs->numBlocks >= numBlocks);

  set->container = bs->container;
  set->numBlocks = bs->numBlocks;
  set->capacity = (bs->numBlocks * s->controls->blockSize) / sizeof(HM_chunk);
  set->size = 0;
  set->data = (HM_chunk*)(void*)bs;

  for (size_t i = 0; i < set->capacity; i++) {
    set->data[i] = EMPTY_SLOT;
  }
}


void ChunkSet_free(GC_state s, ChunkSet set) {
  SuperBlock container = set->container;
  size_t numBlocks = set->numBlocks;
  Blocks bs = (Blocks)(void*)set->data;
  bs->container = container;
  bs->numBlocks = numBlocks;
  freeBlocks(s, bs, NULL);

  set->container = NULL;
  set->numBlocks = 0;
  set->capacity = 0;
  set->size = 0;
  set->data = NULL;
}


void ChunkSet_resize(GC_state s, ChunkSet old) {
  struct ChunkSet new_;
  ChunkSet new = &new_;

  ChunkSet_init(s, new, 2 * old->numBlocks);

  for (size_t i = 0; i < old->capacity; i++) {
    HM_chunk elem = old->data[i];
    if ( (EMPTY_SLOT != elem) && (TOMBSTONE != elem) ) {
      ChunkSet_insert(s, new, elem);
    }
  }

  ChunkSet_free(s, old);

  *old = *new;
}


bool ChunkSet_insert(GC_state s, ChunkSet set, HM_chunk chunkp) {
  assert(EMPTY_SLOT != chunkp && TOMBSTONE != chunkp);

  if ( ((double)set->size / (double)set->capacity) >= 0.75 ) {
    ChunkSet_resize(s, set);
  }
  assert(set->size < set->capacity);

  size_t i = ChunkSet_hashToIndex(chunkp, set->capacity);
  while (TRUE) {
    HM_chunk contents = set->data[i];
    if (contents == EMPTY_SLOT || contents == TOMBSTONE) {
      set->data[i] = chunkp;
      set->size++;
      return TRUE;
    }

    if (contents == chunkp) {
      return FALSE;
    }

    i = (i+1 < set->capacity ? i+1 : 0);
  }
}

bool ChunkSet_contains(
  __attribute__((unused)) GC_state s,
  ChunkSet set,
  HM_chunk chunkp)
{
  assert(EMPTY_SLOT != chunkp && TOMBSTONE != chunkp);

  size_t i = ChunkSet_hashToIndex(chunkp, set->capacity);
  while (TRUE) {
    HM_chunk contents = set->data[i];
    if (contents == EMPTY_SLOT) {
      return FALSE;
    }

    if (contents == chunkp) {
      return TRUE;
    }

    i = (i+1 < set->capacity ? i+1 : 0);
  }
}

bool ChunkSet_remove(
  __attribute__((unused)) GC_state s,
  ChunkSet set,
  HM_chunk chunkp)
{
  assert(EMPTY_SLOT != chunkp && TOMBSTONE != chunkp);

  size_t i = ChunkSet_hashToIndex(chunkp, set->capacity);
  while (TRUE) {
    HM_chunk contents = set->data[i];
    if (contents == EMPTY_SLOT) {
      return FALSE;
    }

    if (contents == chunkp) {
      set->data[i] = TOMBSTONE;
      set->size--;
      return TRUE;
    }

    i = (i+1 < set->capacity ? i+1 : 0);
  }
}

#endif

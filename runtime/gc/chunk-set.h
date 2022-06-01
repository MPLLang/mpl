/* Copyright (C) 2022 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef CHUNK_SET_H_
#define CHUNK_SET_H_

#include "block-allocator.h"

#if (defined (MLTON_GC_INTERNAL_TYPES))
// declare and define

struct ChunkSet {
  HM_chunk *data;
  size_t size;
  size_t capacity;

  // These are needed to free the data
  size_t numBlocks;
  SuperBlock container;
};

typedef struct ChunkSet * ChunkSet;

#else
// just declare

struct ChunkSet;
typedef struct ChunkSet * ChunkSet;

#endif /* defined (MLTON_GC_INTERNAL_TYPES) */



#if (defined (MLTON_GC_INTERNAL_FUNCS))

void ChunkSet_init(GC_state s, ChunkSet set, size_t numBlocks);
void ChunkSet_free(GC_state s, ChunkSet set);

bool ChunkSet_contains(GC_state s, ChunkSet set, HM_chunk chunk);

// returns TRUE if success; FALSE if already contained
bool ChunkSet_insert(GC_state s, ChunkSet set, HM_chunk chunk);

// returns TRUE if success; FALSE if not present
bool ChunkSet_remove(GC_state s, ChunkSet set, HM_chunk chunk);

#endif /* defined (MLTON_GC_INTERNAL_FUNCS) */


#endif /* CHUNK_SET_H_ */

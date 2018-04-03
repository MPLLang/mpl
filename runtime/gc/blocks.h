/* Copyright (C) 2018 Sam Westrick
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef MLTON_BLOCKS_H
#define MLTON_BLOCKS_H

#if (defined (MLTON_GC_INTERNAL_TYPES))

struct Block_config {
  size_t blockSize;
  size_t batchSize;
};

#endif /* defined (MLTON_GC_INTERNAL_TYPES) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static void initBlocks(struct Block_config* config);
static inline bool inSameBlock(pointer p, pointer q);
static inline pointer blockOf(pointer p);

pointer GC_getBlocks(GC_state s, size_t* bytesRequested);

#endif /* defined (MLTON_GC_INTERNAL_FUNCS) */

#endif /* MLTON_BLOCKS_H */

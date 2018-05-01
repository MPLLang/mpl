/* Copyright (C) 2018 Sam Westrick
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef MLTON_BLOCKS_H
#define MLTON_BLOCKS_H

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static void initBlocks(GC_state s);
static inline bool inSameBlock(pointer p, pointer q);
static inline pointer blockOf(pointer p);

// pointer GC_getBlocks(GC_state s, size_t* bytesRequested);

#endif /* defined (MLTON_GC_INTERNAL_FUNCS) */

#endif /* MLTON_BLOCKS_H */

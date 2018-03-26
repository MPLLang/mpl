/* Copyright (C) 2018 Sam Westrick
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef MLTON_BLOCKS_H
#define MLTON_BLOCKS_H

#if (defined (MLTON_GC_INTERNAL_FUNCS))

pointer GC_getBlocks(GC_state s, size_t* bytesRequested);

/* Return pointer to a contiguous region of at least `bytesRequested` bytes,
 * aligned at a multiple of `alignment`. The actual size of the allocated
 * region will be a multiple of `alignment`. */
// pointer Blocks_allocBatch(size_t bytesRequested, size_t alignment);

/* free a region returned by `Blocks_allocBatch` */
// void Blocks_freeBatch(pointer p);

#endif /* defined (MLTON_GC_INTERNAL_FUNCS) */

#endif /* MLTON_BLOCKS_H */

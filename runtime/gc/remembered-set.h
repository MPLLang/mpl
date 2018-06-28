/* Copyright (C) 2018 Sam Westrick
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef REMEMBERED_SET_H_
#define REMEMBERED_SET_H_


#if (defined (MLTON_GC_INTERNAL_TYPES))

/* entries in a chunkList->rememberedSet
 * preferably, we wouldn't have to remember src. However then at a collection
 * we wouldn't be able to check if dst[idx] still points into the local heap
 * without a dangerous race.
 */
struct HM_remembered {
  objptr dst;
  Int64 idx;
  objptr src;
};

typedef void (*ForeachRememberedFunc)(GC_state s, objptr dst, Int64 idx, objptr src, void* args);

#endif /* defined (MLTON_GC_INTERNAL_TYPES) */


#if (defined (MLTON_GC_INTERNAL_BASIS))

void HM_remember(HM_chunkList levelHead, objptr dst, Int64 index, objptr src);
void HM_foreachRemembered(GC_state s, HM_chunkList rememberedSet, ForeachRememberedFunc f, void* fArgs);

#endif /* defined (MLTON_GC_INTERNAL_BASIS) */


#endif /* REMEMBERED_SET_H_ */

/* Copyright (C) 2018,2019 Sam Westrick
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef REMEMBERED_SET_H_
#define REMEMBERED_SET_H_


#if (defined (MLTON_GC_INTERNAL_TYPES))

/* Remembering *field = src
 * field must be an internal pointer of dst */
struct HM_remembered {
  objptr dst;
  objptr* field;
  objptr src;
};

typedef void (*ForeachRememberedFunc)(GC_state s, objptr dst, objptr* field, objptr src, void* args);

#endif /* defined (MLTON_GC_INTERNAL_TYPES) */


#if (defined (MLTON_GC_INTERNAL_BASIS))

void HM_remember(HM_chunkList remSet, objptr dst, objptr* field, objptr src);
void HM_rememberAtLevel(HM_HierarchicalHeap hh, objptr dst, objptr* field, objptr src);
void HM_foreachRemembered(GC_state s, HM_chunkList remSet, ForeachRememberedFunc f, void* fArgs);
size_t HM_numRemembered(HM_chunkList remSet);

#endif /* defined (MLTON_GC_INTERNAL_BASIS) */


#endif /* REMEMBERED_SET_H_ */

/* Copyright (C) 2018 Sam Westrick
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef REMEMBERED_SET_H_
#define REMEMBERED_SET_H_


#if (defined (MLTON_GC_INTERNAL_TYPES))

/* Remembering *dst = src
 * dst must be an internal pointer of obj */
struct HM_remembered {
  objptr obj;
  objptr* dst;
  objptr src;
};

typedef void (*ForeachRememberedFunc)(GC_state s, objptr obj, objptr* dst, objptr src, void* args);

#endif /* defined (MLTON_GC_INTERNAL_TYPES) */


#if (defined (MLTON_GC_INTERNAL_BASIS))

void HM_remember(HM_chunkList rememberedSet, HM_chunkList levelHead, objptr obj, objptr* dst, objptr src);
void HM_rememberAtLevel(HM_chunkList levelHead, objptr obj, objptr* dst, objptr src);
void HM_foreachRemembered(GC_state s, HM_chunkList rememberedSet, ForeachRememberedFunc f, void* fArgs);

#endif /* defined (MLTON_GC_INTERNAL_BASIS) */


#endif /* REMEMBERED_SET_H_ */

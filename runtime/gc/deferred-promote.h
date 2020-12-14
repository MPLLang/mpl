/* Copyright (C) 2018-2019 Sam Westrick.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef DEFERRED_PROMOTE_H
#define DEFERRED_PROMOTE_H
#if (defined (MLTON_GC_INTERNAL_FUNCS))

void HM_deferredPromote(
  GC_state s,
  GC_thread thread,
  HM_chunkList globalDownPtrs,
  struct ForwardHHObjptrArgs* args);
void bucketIfValidAtList(GC_state s, objptr dst, objptr* field, objptr src, void* remSet);
#endif  /* defined (MLTON_GC_INTERNAL_FUNCS) */
#endif  /* DEFERRED_PROMOTE_H */

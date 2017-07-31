/* Copyright (C) 2017 Adrien Guatto.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef LEVEL_H
#define LEVEL_H

#if (defined (MLTON_GC_INTERNAL_BASIS))

typedef void (*hhFunction)(struct HM_HierarchicalHeap *, void *);

PRIVATE
struct HM_HierarchicalHeap *HM_foreachHHUp(GC_state s,
                                           struct HM_HierarchicalHeap *bottom,
                                           Word32 minLevel,
                                           hhFunction function,
                                           void *arg);

PRIVATE void HM_foreachHHDown(GC_state s,
                              struct HM_HierarchicalHeap *bottom,
                              Word32 minLevel,
                              hhFunction function,
                              void *arg);

PRIVATE pointer HM_followForwardPointerUntilNullOrBelowLevel(GC_state s,
                                                             pointer p,
                                                             Word32 minLevel);

PRIVATE bool HM_objptrIsAboveHH(GC_state s,
                                pointer p,
                                struct HM_HierarchicalHeap *hh);

#endif  /* MLTON_GC_INTERNAL_BASIS */

#endif  /* LEVEL_H */

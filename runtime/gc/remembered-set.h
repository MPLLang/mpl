/* Copyright (C) 2018-2020 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef REMEMBERED_SET_H_
#define REMEMBERED_SET_H_


#if (defined (MLTON_GC_INTERNAL_TYPES))
#include "gc/concurrent-list.h"

/* Remembering that there exists a downpointer to this object. The unpin
 * depth of the object will be stored in the object header. */
typedef struct HM_remembered {
  objptr from;
  objptr object;
} * HM_remembered;


/*
1. How do we do this public remSet in a hh changing away?
2. What's a simple ds that does the right thing? -> global lookup, which maps
   a position in the heap hierarchy to a remSet.
3. Each chunk keeps track of which remSet?
4.

hh->chunkList <--> ogList
toList
=============================

*/
typedef struct HM_remSet {
  struct HM_chunkList private;
  struct CC_concList public;
} * HM_remSet;

typedef void (*HM_foreachDownptrFun)(GC_state s, HM_remembered remElem, void* args);

typedef struct HM_foreachDownptrClosure {
  HM_foreachDownptrFun fun;
  void *env;
} *HM_foreachDownptrClosure;

#endif /* defined (MLTON_GC_INTERNAL_TYPES) */


#if (defined (MLTON_GC_INTERNAL_BASIS))

void HM_initRemSet(HM_remSet remSet);
void HM_freeRemSetWithInfo(GC_state s, HM_remSet remSet, void* info);
void HM_remember(HM_remSet remSet, HM_remembered remElem, bool conc);
void HM_appendRemSet(HM_remSet r1, HM_remSet r2);
void HM_foreachRemembered(GC_state s, HM_remSet remSet, HM_foreachDownptrClosure f, bool trackFishyChunks);
size_t HM_numRemembered(HM_remSet remSet);
void HM_foreachPublic(GC_state s, HM_remSet remSet, HM_foreachDownptrClosure f, bool trackFishyChunks);
void HM_foreachPrivate(GC_state s, HM_chunkList list,HM_foreachDownptrClosure f);

#endif /* defined (MLTON_GC_INTERNAL_BASIS) */


#endif /* REMEMBERED_SET_H_ */

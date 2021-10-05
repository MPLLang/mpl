/* Copyright (C) 2018-2020 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef REMEMBERED_SET_H_
#define REMEMBERED_SET_H_


#if (defined (MLTON_GC_INTERNAL_TYPES))

/* Remembering that there exists a downpointer to this object. The unpin
 * depth of the object will be stored in the object header. */
typedef struct HM_remembered {
  objptr from;
  objptr object;
} * HM_remembered;

typedef void (*HM_foreachDownptrFun)(GC_state s, HM_remembered remElem, void* args);

typedef struct HM_foreachDownptrClosure {
  HM_foreachDownptrFun fun;
  void *env;
} *HM_foreachDownptrClosure;

#endif /* defined (MLTON_GC_INTERNAL_TYPES) */


#if (defined (MLTON_GC_INTERNAL_BASIS))

void HM_remember(HM_chunkList remSet, HM_remembered remElem);
void HM_rememberAtLevel(HM_HierarchicalHeap hh, HM_remembered remElem);
void HM_foreachRemembered(GC_state s, HM_chunkList remSet, HM_foreachDownptrClosure f);
size_t HM_numRemembered(HM_chunkList remSet);

#endif /* defined (MLTON_GC_INTERNAL_BASIS) */


#endif /* REMEMBERED_SET_H_ */

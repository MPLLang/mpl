/* Copyright (C) 2021 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/** Epoch-based reclamation (EBR) of hierarchical heap records.
  */

#ifndef HIERARCHICAL_HEAP_EBR_H_
#define HIERARCHICAL_HEAP_EBR_H_

#if (defined (MLTON_GC_INTERNAL_BASIS))

void HH_EBR_init(GC_state s);
void HH_EBR_enterQuiescentState(GC_state s);
void HH_EBR_leaveQuiescentState(GC_state s);
void HH_EBR_retire(GC_state s, HM_UnionFindNode hhuf);

#endif // MLTON_GC_INTERNAL_BASIS


#endif // HIERARCHICAL_HEAP_EBR_H_

/* Copyright (C) 2021 Sam Westrick
 * Copyright (C) 2022 Jatin Arora
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

void HH_EBR_enterQuiescentState(GC_state s) {
  EBR_enterQuiescentState(s, s->hhEBR);
}

void freeUnionFind (GC_state s, void *ptr) {
  HM_UnionFindNode hufp = (HM_UnionFindNode)ptr;
  assert(hufp->payload != NULL);
  freeFixedSize(getHHAllocator(s), hufp->payload);
  freeFixedSize(getUFAllocator(s), hufp);
}

void HH_EBR_init(GC_state s) {
  s->hhEBR = EBR_new(s, &freeUnionFind);
}


void HH_EBR_leaveQuiescentState(GC_state s) {
  EBR_leaveQuiescentState(s, s->hhEBR);
}

void HH_EBR_retire(GC_state s, HM_UnionFindNode hhuf) {
  EBR_retire(s, s->hhEBR, (void *)hhuf);
}

#endif // MLTON_GC_INTERNAL_FUNCS

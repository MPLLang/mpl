/* Copyright (C) 2022 Jatin Arora
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

void freeChunk (GC_state s, void *ptr) {
  HM_freeChunk(s, (HM_chunk)ptr);
}

void HM_EBR_init(GC_state s) {
  s->hmEBR = EBR_new(s, &freeChunk);
}

void HM_EBR_enterQuiescentState (GC_state s) {
  EBR_enterQuiescentState(s, s->hmEBR);
}

void HM_EBR_leaveQuiescentState(GC_state s) {
  EBR_leaveQuiescentState(s, s->hmEBR);
}

void HM_EBR_retire(GC_state s, HM_chunk chunk) {
  EBR_retire(s, s->hmEBR, (void *)chunk);
}

#endif // MLTON_GC_INTERNAL_FUNCS

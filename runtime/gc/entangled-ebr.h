/** Epoch-based reclamation (EBR) of hierarchical heap records.
 */

#ifndef ENTANGLED_EBR_H_
#define ENTANGLED_EBR_H_

#if (defined(MLTON_GC_INTERNAL_FUNCS))


void HM_EBR_init(GC_state s);
void HM_EBR_enterQuiescentState(GC_state s);
void HM_EBR_leaveQuiescentState(GC_state s);
void HM_EBR_retire(GC_state s, HM_chunk chunk);

#endif // MLTON_GC_INTERNAL_FUNCS

#endif //CHUNK_EBR_H_

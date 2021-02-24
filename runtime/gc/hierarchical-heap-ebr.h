/* Copyright (C) 2021 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/** Epoch-based reclamation (EBR) of hierarchical heap records.
  */

#ifndef HIERARCHICAL_HEAP_EBR_H_
#define HIERARCHICAL_HEAP_EBR_H_

#if (defined (MLTON_GC_INTERNAL_TYPES))

struct HH_EBR_local {
  struct HM_chunkList limboBags[3];
  int limboIdx;
  uint32_t checkNext;
} __attribute__((aligned(128)));

// There is exactly one of these! Everyone shares a reference to it.
typedef struct HH_EBR_shared {
  size_t epoch;

  // announcement array, length = num procs
  // each announcement is packed: 63 bits for epoch, 1 bit for quiescent bit
  size_t *announce;

  // processor-local data, length = num procs
  struct HH_EBR_local *local;
} * HH_EBR_shared;

#else

struct HH_EBR_local;
struct HH_EBR_shared;
typedef struct HH_EBR_shared * HH_EBR_shared;

#endif // MLTON_GC_INTERNAL_TYPES

#if (defined (MLTON_GC_INTERNAL_FUNCS))

void HH_EBR_init(GC_state s);
void HH_EBR_enterQuiescentState(GC_state s);
void HH_EBR_leaveQuiescentState(GC_state s);
void HH_EBR_retire(GC_state s, HM_UnionFindNode hhuf);

#endif // MLTON_GC_INTERNAL_FUNCS


#endif // HIERARCHICAL_HEAP_EBR_H_

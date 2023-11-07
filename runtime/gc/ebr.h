/* Copyright (C) 2021 Sam Westrick
 * Copyright (C) 2022 Jatin Arora
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/** Epoch-based reclamation (EBR) of hierarchical heap records.
 */

#ifndef EBR_H_
#define EBR_H_

#if (defined(MLTON_GC_INTERNAL_TYPES))

struct EBR_local
{
  struct HM_chunkList limboBags[3];
  int limboIdx;
  uint32_t checkNext;
} __attribute__((aligned(128)));

typedef void (*EBR_freeRetiredObj) (GC_state s, void *ptr);

// There is exactly one of these! Everyone shares a reference to it.
typedef struct EBR_shared
{
  size_t epoch;

  // announcement array, length = num procs
  // each announcement is packed: 63 bits for epoch, 1 bit for quiescent bit
  size_t *announce;

  // processor-local data, length = num procs
  struct EBR_local *local;

  EBR_freeRetiredObj freeFun;
} * EBR_shared;

#else

struct EBR_local;
struct EBR_shared;
typedef struct EBR_shared *EBR_shared;

#endif // MLTON_GC_INTERNAL_TYPES

#if (defined(MLTON_GC_INTERNAL_FUNCS))

EBR_shared EBR_new(GC_state s, EBR_freeRetiredObj freeFun);
void EBR_enterQuiescentState(GC_state s, EBR_shared ebr);
void EBR_leaveQuiescentState(GC_state s, EBR_shared ebr);
void EBR_retire(GC_state s, EBR_shared ebr, void *ptr);

#endif // MLTON_GC_INTERNAL_FUNCS

#endif // EBR_H_

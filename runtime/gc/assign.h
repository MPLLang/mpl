/* Copyright (C) 1999-2017 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef ASSIGN_H
#define ASSIGN_H

#if (defined (MLTON_GC_INTERNAL_BASIS))

#include "hierarchical-heap.h"

PRIVATE pointer Assignable_findLockedTrueReplicaReader(
  GC_state, objptr, struct HM_HierarchicalHeap **
  );

PRIVATE pointer Assignable_findLockedTrueReplicaWriter(
  GC_state, objptr, struct HM_HierarchicalHeap **
  );

PRIVATE void Assignable_unlockReplicaReader(
  GC_state, struct HM_HierarchicalHeap *
  );

PRIVATE void Assignable_unlockReplicaWriter(
  GC_state, struct HM_HierarchicalHeap *
  );

PRIVATE int Assignable_isMaster(GC_state s, objptr o);

PRIVATE objptr Assignable_get(
  GC_state s, objptr src, Int64 index
  );

PRIVATE void Assignable_set(
  GC_state s, objptr dst, Int64 index, objptr src
  );

#endif  /* MLTON_GC_INTERNAL_BASIS */

#endif  /* ASSIGN_H */

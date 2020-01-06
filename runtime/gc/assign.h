/* Copyright (C) 1999-2017 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef ASSIGN_H
#define ASSIGN_H

#if (defined (MLTON_GC_INTERNAL_BASIS))

#include "hierarchical-heap.h"

PRIVATE void Assignable_writeBarrier(
  GC_state s, objptr dst, objptr* field, objptr src
  );

#endif  /* MLTON_GC_INTERNAL_BASIS */

#endif  /* ASSIGN_H */

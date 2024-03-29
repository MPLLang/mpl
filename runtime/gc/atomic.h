/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_BASIS))

PRIVATE void GC_assertAtomicState(GC_state s, uint32_t expected);

#endif /* (defined (MLTON_GC_INTERNAL_BASIS)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline void beginAtomic (GC_state s);
static inline void endAtomic (GC_state s);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

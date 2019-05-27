/* Copyright (C) 2012 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))

#define GC_HEAP_LIMIT_SLOP 512

#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */


#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline bool isPointerInGlobalHeap (GC_state s, pointer p);
static inline bool isObjptrInGlobalHeap (GC_state s, objptr p);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

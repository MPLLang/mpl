/* Copyright (C) 2012,2016 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_TYPES))
#endif /* (defined (MLTON_GC_INTERNAL_TYPES)) */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static inline objptr* getFwdPtrp (pointer p);
static inline objptr getFwdPtr (pointer p);
static inline bool hasFwdPtr (pointer p);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

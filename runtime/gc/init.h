/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#if (defined (MLTON_GC_INTERNAL_FUNCS))

static int processAtMLton (GC_state s, int argc,
                           char **argv, char **worldFile);

#endif /* (defined (MLTON_GC_INTERNAL_FUNCS)) */

PRIVATE int GC_init (GC_state s, int argc, char **argv);
PRIVATE void GC_lateInit (GC_state s);
PRIVATE void GC_traceInit (GC_state s);
PRIVATE void GC_traceFinish (GC_state s);
PRIVATE void GC_duplicate (GC_state d, GC_state s);

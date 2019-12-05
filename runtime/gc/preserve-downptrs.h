/* Copyright (C) 2018 Sam Westrick.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef PRESERVE_DOWNPTRS_H
#define PRESERVE_DOWNPTRS_H
#if (defined (MLTON_GC_INTERNAL_FUNCS))

void HM_preserveDownPtrs(GC_state s, struct ForwardHHObjptrArgs* args);

#endif  /* defined (MLTON_GC_INTERNAL_FUNCS) */
#endif  /* PRESERVE_DOWNPTRS_H */

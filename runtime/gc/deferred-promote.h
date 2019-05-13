/* Copyright (C) 2018 Sam Westrick.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef DEFERRED_PROMOTE_H
#define DEFERRED_PROMOTE_H
#if (defined (MLTON_GC_INTERNAL_FUNCS))

HM_chunkList HM_deferredPromote(GC_state s, struct ForwardHHObjptrArgs* args);

#endif  /* defined (MLTON_GC_INTERNAL_FUNCS) */
#endif  /* DEFERRED_PROMOTE_H */

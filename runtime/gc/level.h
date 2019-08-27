/* Copyright (C) 2017 Adrien Guatto.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef LEVEL_H
#define LEVEL_H

#if (defined (MLTON_GC_INTERNAL_BASIS))

PRIVATE bool HM_objptrIsAboveHH(GC_state s,
                                pointer p,
                                struct HM_HierarchicalHeap *hh);

#endif  /* MLTON_GC_INTERNAL_BASIS */

#endif  /* LEVEL_H */
